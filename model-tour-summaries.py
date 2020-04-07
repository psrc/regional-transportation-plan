import os
import pandas as pd

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

analysis_years = ['2014','2025','2040','2050']

input_directory = os.path.join(os.getcwd(),'inputs')
output_directory = os.path.join(os.getcwd(),'output')

# Create the output directory for the trip generation results
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

def tour_trip_mode(working_df, working_geo, working_cols, working_purpose, working_place_type,working_variable_name, working_modes, working_year):
    
    print('Removing any rows with a Mode of 0')
    working_df = working_df[working_df['tmodetp'] > 0]
    working_df = working_df[working_df[working_geo] != 0]
    
    print('Dropping all geographies except ' + working_geo)
    working_cols.append(working_geo)
    working_df = working_df[working_cols]

    print('Subsetting Tours to only include Home to ' + working_purpose[0] + ' tours')
    working_df = working_df[(working_df['toadtyp'] == 1) & (working_df['pdpurp'] == working_purpose[1])]
    working_df = working_df.drop(['toadtyp','pdpurp'], axis = 1)
    working_df['estimate'] = 1

    print('Combining by Place and Mode')
    place_results = working_df.groupby([working_geo,'tmodetp']).sum().reset_index()
    place_results['variable_description'] = ""
    
    print('Adding Mode Name as Variable Description')
    for current_mode in working_modes:
        place_results.loc[(place_results['tmodetp'] > current_mode[0]) & (place_results['tmodetp'] <= current_mode[1]), 'variable_description'] = current_mode[2] 
    place_results = place_results.drop(['parcel_id','tmodetp'], axis = 1)
    
    print('Combining by Mode Name and adding other details')
    place_results = place_results.groupby([working_geo,'variable_description']).sum().reset_index()
    place_results['place_type'] = working_place_type
    place_results['variable_name'] = working_variable_name
    place_results['year'] = working_year
    place_results['data_source'] = 'SoundCast'
    
    print('Calculating total Commute Trips by Place')
    total_trips = place_results.groupby([working_geo]).sum().reset_index()
    total_trips['place_type'] = working_place_type
    total_trips['variable_name'] = 'total-commute-trips'
    total_trips['year'] = working_year
    total_trips['data_source'] = 'SoundCast'
    total_trips['variable_description'] = 'Total Commute Trips'
    place_results = place_results.append(total_trips, sort=False)
    place_results  = place_results.rename(columns={working_geo:'geog_name'})
    place_results = place_results.reset_index()
    place_results = place_results.drop(['index'], axis = 1)

    print('Calculating shares of total for each place')
    place_results['temp'] = 0
    for ind in place_results.index: 
        current_name = place_results['geog_name'][ind]
        total_estimate = place_results[(place_results['geog_name'] == current_name) & (place_results['variable_name'] == 'total-commute-trips')]['estimate'].values[0]
        place_results['temp'][ind] = total_estimate
            
    place_results['share'] = place_results['estimate'] / place_results['temp']
    place_results = place_results.drop(['temp'], axis = 1)
    
    return(place_results)

def tour_trip_timebucket(working_df, working_geo, working_cols, working_purpose, working_buckets, working_place_type, working_variable_name, working_year, working_modes, working_totals):
    
    place_results = pd.DataFrame()
    
    print('Removing any rows with a Mode of 0')
    working_df = working_df[working_df['tmodetp'] > 0]
    working_df = working_df[working_df[working_geo] != 0]
    
    print('Dropping all geographies except ' + working_geo)
    working_cols.append(working_geo)
    working_df = working_df[working_cols]

    print('Subsetting Tours to only include Home to ' + working_purpose[0] + ' tours')
    working_df = working_df[(working_df['toadtyp'] == 1) & (working_df['pdpurp'] == working_purpose[1])]
    working_df = working_df.drop(['parcel_id','toadtyp','pdpurp'], axis = 1)
    working_df['estimate'] = 1

    print('Calculating ' + working_variable_name + ' by Place by Time Bucket for All Modes')
    for current_bucket in working_buckets:
    
        for current_mode in working_modes:
        
            interim_df = working_df[(working_df['tautotime'] > current_bucket[0]) & (working_df['tautotime'] <= current_bucket[1])]
            interim_df = interim_df[(interim_df['tmodetp'] > current_mode[0]) & (interim_df['tmodetp'] <= current_mode[1])]
            interim_df = interim_df.drop(['tmodetp','tautotime'], axis = 1)
            interim_places = interim_df.groupby([working_geo]).sum().reset_index()
            interim_places['variable_description'] = current_bucket[2]
            interim_places['variable_name'] = current_mode[2]
            interim_places['place_type'] = working_place_type
            interim_places['year'] = working_year
            interim_places['data_source'] = 'SoundCast'
    
            if place_results.empty:
                place_results = interim_places
            else:
                place_results = place_results.append(interim_places, sort=False)

    print('Combining Time Buckets and Totals into 1 dataframe')
    place_results = place_results.rename(columns={working_geo:'geog_name'})
    place_results['temp'] = 0
    place_results = place_results.reset_index()
    place_results = place_results.drop(['index'], axis = 1)

    working_totals.loc[working_totals['variable_description'] == 'Total Commute Trips', 'variable_description'] = 'All Modes'
    working_totals = working_totals.reset_index()
    working_totals = working_totals.drop(['index'], axis = 1)

    for ind in place_results.index: 
        current_name = place_results['geog_name'][ind]
        current_mode = place_results['variable_name'][ind]
        total_estimate = working_totals[(working_totals['geog_name'] == current_name) & (working_totals['variable_description'] == current_mode)]['estimate'].values[0]
        place_results['temp'][ind] = total_estimate
            
    place_results['share'] = place_results['estimate'] / place_results['temp']
    place_results = place_results.drop(['temp'], axis = 1)
    
    return(place_results)

def tour_total_trip_time(working_df,working_geo,working_cols,working_purpose,working_place_type,working_purpose_name,working_year):

    print('Removing any rows with a Mode of 0')
    working_df = working_df[working_df['tmodetp'] > 0]
    working_df = working_df[working_df[working_geo] != 0]
    
    print('Dropping all geographies except ' + working_geo)
    working_cols.append(working_geo)
    working_df = working_df[working_cols]

    print('Subsetting Tours to only include Home to ' + working_purpose[0] + ' tours')
    working_df = working_df[(working_df['toadtyp'] == 1) & (working_df['pdpurp'] == working_purpose[1])]
    working_df = working_df.drop(['parcel_id','toadtyp','pdpurp'], axis = 1)

    print('Grouping Travel Time results by ' + working_geo)
    place_results = working_df.groupby([working_geo]).sum().reset_index()     
    place_results['variable_description'] = 'Total '+ working_purpose_name + ' Time'
    place_results['variable_name'] = 'total-'+working_purpose_name.lower()+'-time'
    place_results['place_type'] = working_place_type
    place_results['year'] = working_year
    place_results['data_source'] = 'SoundCast'
    place_results['share'] = 1
    place_results = place_results.rename(columns={working_geo:'geog_name','tautotime':'estimate'})
    
    return(place_results)

#############################################################################################
#############################################################################################
### Summarize Tour Level Mode and Travel Time Results by model year
#############################################################################################
#############################################################################################
print('Loading parcels with overlays from disk')
parcel_overlay_file = os.path.join(input_directory,'shapefiles','wgs1984','parcels_with_overlays.csv')
all_parcels = pd.read_csv(parcel_overlay_file, low_memory=False)  
all_parcels.loc[all_parcels['place_name'].isna(), 'place_name'] = 'Unincorporated'
all_parcels.loc[all_parcels['county_nm'] == 'Kittitas County', 'county_nm'] = 'King County'
all_parcels.loc[all_parcels['county_nm'] == 'Lewis County', 'county_nm'] = 'Pierce County'
all_parcels.loc[all_parcels['county_nm'] == 'Mason County', 'county_nm'] = 'Kitsap County'
all_parcels.loc[all_parcels['county_nm'] == 'Thurston County', 'county_nm'] = 'Pierce County'  

final_places = pd.DataFrame()

geographic_types = [['place_name', 'pl'],['county_nm','co']]

for current_year in analysis_years:
    
    print('Loading tour results from disk for year ' + str(current_year))
    tour_file = os.path.join(input_directory,current_year,'_tour.tsv')
    all_tours = pd.read_csv(tour_file, sep = '\t')
    cols_to_keep = ['topcl','toadtyp','pdpurp','tmodetp','tlvorig','tardest']
    all_tours = all_tours[cols_to_keep]
    all_tours  = all_tours.rename(columns={'topcl':'parcel_id'})
    all_tours = pd.merge(all_tours,all_parcels,on='parcel_id',suffixes=('_x','_y'), how='left')
    all_tours['tautotime'] = all_tours['tardest'] - all_tours['tlvorig']
    all_tours = all_tours.fillna(0)
    
    for geographies in geographic_types:
    
        print('Loading lists need for each analysis year')
        all_modes = [[0,1,'Walk'],[1,2,'Bike'],[2,3,'Drive-Alone'],[3,5,'Carpool'],[5,8,'Transit']]
        time_buckets = [[0,15,'less than 15 minutes'],[15,30,'15 to 30 minutes'],[30,45,'30 to 45 minutes'],[45,60,'45 to 60 minutes'],[60,500,'more than 60 minutes']]
        mode_share_cols = ['parcel_id','toadtyp','pdpurp','tmodetp']
        all_time_modes = [[0,1,'Walk'],[1,2,'Bike'],[2,3,'Drive-Alone'],[3,5,'Carpool'],[5,8,'Transit'],[0,8,'All Modes']]
        mode_time_cols = ['parcel_id','toadtyp','pdpurp','tmodetp','tautotime']
        total_time_cols = ['parcel_id','toadtyp','pdpurp','tautotime']

        print('Calculating Mode Shares by ' + geographies[0] + ' for Work Tours')
        place_commute_trips = tour_trip_mode(all_tours, geographies[0], mode_share_cols,['Work',1], geographies[1], 'commute-trips',all_modes,current_year)
        place_commute_times = tour_trip_timebucket(all_tours, geographies[0], mode_time_cols,['Work',1],time_buckets, geographies[1], 'commute-trips',current_year,all_time_modes,place_commute_trips)
        place_total_commute_times = tour_total_trip_time(all_tours, geographies[0], total_time_cols,['Work',1], geographies[1], 'Commute',current_year)

        interim = place_commute_trips
        interim = interim.append(place_commute_times, sort=False)
        interim = interim.append(place_total_commute_times, sort=False)
      
        print('Combine Final Times and Commute Trips')
        if final_places.empty:
            final_places = interim
        
        else:
            final_places = final_places.append(interim, sort=False)
        
print('Replacing All Modes description to match Census Data Variable Description')
final_places.loc[final_places['variable_description'] == 'All Modes', 'variable_description'] = 'Total Commute Trips'

print('Writing final trip summary dataframe to csv for further analysis')
final_places.to_csv(os.path.join(output_directory,'tour_model_data_for_rtp.csv'),index=False)

print('All done.')
