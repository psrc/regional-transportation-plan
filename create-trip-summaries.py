import os
import h5py
import pandas as pd

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

current_year = '2014'
low_income_threshold = 25000
elderly_threshold = 65

all_modes = [[0,1,'Walk'],[1,2,'Bike'],[2,3,'Drive-Alone'],[3,5,'Carpool'],[5,8,'Transit'],[0,8,'All-Modes']]
time_buckets = [[0,15,'less than 15 minutes'],[15,30,'15 to 30 minutes'],[30,45,'30 to 45 minutes'],[45,60,'45 to 60 minutes'],[60,90,'60 to 90 minutes'],[90,500,'more than 90 minutes']]
distance_buckets = [[0,1,'less than 1 mile'],[1,3,'1 to 3 miles'],[3,5,'3 to 5 miles'],[5,10,'5 to 10 miles'],[10,15,'10 to 15 miles'],[15,20,'15 to 20 miles'],[20,500,'more than 20 miles']]

hh_variables=['hhno','hhparcel','hhincome']

working_path = os.getcwd()

def create_df_from_h5(h5_file, h5_table, h5_variables):

    h5_data = {}
    
    for var in h5_variables:
        h5_data[var] = h5_file[h5_table][var][:]
    
    return pd.DataFrame(h5_data)

def summarize_trips(current_df, working_geo, working_pl_type, working_year, working_purpose, working_list, working_variable):
    
    print('Dropping all geographies except ' + working_geo)
    working_cols = [working_geo]

    print('Working on ' + working_variable + ' buckets')
    for current in working_list:
        for current_mode in all_modes:
            working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-' + working_variable + '_' + current[2])
            working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips')
     
    working_cols = pd.unique(working_cols).tolist()
    interim_df = current_df[working_cols]
    place_results =interim_df.groupby([working_geo]).sum().reset_index()
       
    print('Calculating the share of trips by mode and ' + working_variable + ' bucket')
    for current_mode in all_modes:        
        for current in working_list:
            place_results['share_' + current_mode[2] + '_' + working_purpose + '-' + working_variable + '_' + current[2]] = place_results['total_' + current_mode[2] + '_' + working_purpose + '-' + working_variable + '_' + current[2]] / place_results['total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips']

    print('Removing the Total Trip results used for calculating trip shares')
    working_cols = [working_geo]

    for current in working_list:
        for current_mode in all_modes:
            working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-' + working_variable + '_' + current[2])
            working_cols.append('share_' + current_mode[2] + '_' + working_purpose + '-' + working_variable + '_' + current[2])
     
    working_cols = pd.unique(working_cols).tolist()
    place_results = place_results[working_cols]

    print('Creating long form results for ' + working_variable + ' buckets.')
    final_places = pd.melt(place_results, id_vars=[working_geo])
    final_places['place_type'] = working_pl_type
    final_places['year'] = working_year
    final_places[['variable_category','variable_mode','variable_name','variable_description']] = final_places.variable.str.split("_",expand=True,)
    final_places = final_places.drop(['variable'], axis = 1)
    final_places  = final_places.rename(columns={'value':'estimate'})
    final_places  = final_places.rename(columns={working_geo:'geog_name'})
    
    return(final_places)

def summarize_trip_totals(current_df, working_geo, working_pl_type, working_year, working_purpose):
       
    print('Dropping all geographies except ' + working_geo)
    working_cols = [working_geo]

    print('Working on ' + working_purpose + ' total and average trips, times and distances')
    for current_mode in all_modes:
        working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-time_total-time')
        working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-distance_total-distance')
        working_cols.append('total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips')
     
    working_cols = pd.unique(working_cols).tolist()
    interim_df = current_df[working_cols]
    place_results =interim_df.groupby([working_geo]).sum().reset_index()
       
    print('Calculating the share of total trips by mode')
    for current_mode in all_modes:        
        place_results['share_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips'] = place_results['total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips'] / place_results['total_All-Modes_' + working_purpose + '-trips_total-trips']

    print('Calculating average time and distance by mode')
    for current_mode in all_modes:        
        place_results['total_' + current_mode[2] + '_' + working_purpose + '-time_average-time'] = place_results['total_' + current_mode[2] + '_' + working_purpose + '-time_total-time'] / place_results['total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips']
        place_results['total_' + current_mode[2] + '_' + working_purpose + '-distance_average-distance'] = place_results['total_' + current_mode[2] + '_' + working_purpose + '-distance_total-distance'] / place_results['total_' + current_mode[2] + '_' + working_purpose + '-trips_total-trips']

    print('Creating long form results for ' + working_purpose + ' total and average trips, times and distances')
    final_places = pd.melt(place_results, id_vars=[working_geo])
    final_places['place_type'] = working_pl_type
    final_places['year'] = working_year
    final_places[['variable_category','variable_mode','variable_name','variable_description']] = final_places.variable.str.split("_",expand=True,)
    final_places = final_places.drop(['variable'], axis = 1)
    final_places  = final_places.rename(columns={'value':'estimate'})
    final_places  = final_places.rename(columns={working_geo:'geog_name'})
    
    return(final_places)

def create_parcel_trip_mode_summary(working_df, trip_purpose):
       
    for current in time_buckets:
        print('Calculating the number of ' + trip_purpose + ' trips with a travel time within ' + current[2] + ' minutes for year ' + str(current_year))
        working_df[current[2]] = 0
        working_df.loc[(working_df['travtime'] >= current[0]) & (working_df['travtime'] < current[1]), current[2]] = 1

    for current in distance_buckets:
        print('Calculating the number of ' + trip_purpose + ' trips with a travel distance within ' + current[2] + ' miles for year ' + str(current_year))
        working_df[current[2]] = 0
        working_df.loc[(working_df['travtime'] >= current[0]) & (working_df['travtime'] < current[1]), current[2]] = 1

    parcel_no = working_df['parcel_id'].unique().tolist()
    parcel_trips = pd.DataFrame(parcel_no, columns =['parcel_id'])

    for current in all_modes:
        print('Creating the daily '+current[2]+' trip total by household for year ' + str(current_year))    
        w_df = working_df.loc[(working_df['mode'] > current[0]) & (working_df['mode'] <= current[1])]
        w_df[current[2] + '_' + trip_purpose + '-trips'] = 1
        
        w_df = w_df.drop(['mode'], axis = 1)

        col_names = ['total_' + current[2] + '_' + trip_purpose + '-time_total-time',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_total-distance',
                     'parcel_id',
                     'total_' + current[2] + '_' + trip_purpose + '-time_less than 15 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-time_15 to 30 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-time_30 to 45 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-time_45 to 60 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-time_60 to 90 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-time_more than 90 minutes',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_less than 1 mile',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_1 to 3 miles',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_3 to 5 miles',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_5 to 10 miles',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_10 to 15 miles',
                     'total_' + current[2] + '_' + trip_purpose + '-distance_15 to 20 miles',                     
                     'total_' + current[2] + '_' + trip_purpose + '-distance_more than 20 miles',                      
                     'total_' + current[2] + '_' + trip_purpose + '-trips_total-trips'
                     ]

        w_df.columns = col_names
        w_parcel_trip = w_df.groupby(['parcel_id']).sum().reset_index()
        parcel_trips = pd.merge(parcel_trips,w_parcel_trip,on='parcel_id', suffixes=('_x','_y'), how='left')
        parcel_trips = parcel_trips.fillna(0)
        
    return(parcel_trips)

print('Loading parcels with overlays from disk')
parcel_overlay_file = os.path.join(working_path,'inputs','shapefiles','wgs1984','parcels_with_overlays.csv')
all_parcels = pd.read_csv(parcel_overlay_file, low_memory=False)  
   
# Cleanup of a few geograhpy names
all_parcels.loc[all_parcels['place_name'].isna(), 'place_name'] = 'Unincorporated'
all_parcels.loc[all_parcels['county_nm'] == 'Kittitas County', 'county_nm'] = 'King County'
all_parcels.loc[all_parcels['county_nm'] == 'Lewis County', 'county_nm'] = 'Pierce County'
all_parcels.loc[all_parcels['county_nm'] == 'Mason County', 'county_nm'] = 'Kitsap County'
all_parcels.loc[all_parcels['county_nm'] == 'Thurston County', 'county_nm'] = 'Pierce County'

final_summary = pd.DataFrame()
trips_file = os.path.join(working_path,'inputs',current_year,'_trip.tsv')
hh_person = os.path.join(working_path,'inputs',current_year,'hh_and_persons.h5')

print('Opening HH and Person H5 file and creating dataframes from them for year ' + str(current_year))
hh_people = h5py.File(hh_person,'r+')
hh_df = create_df_from_h5(hh_people, 'Household', hh_variables)
hh_df  = hh_df.rename(columns={'hhparcel':'parcel_id'}) 
cols_to_keep = ['hhno','parcel_id']
hh_to_parcel = hh_df[cols_to_keep] 

print('Opening the Soundcast Trip File for year ' + str(current_year) + ' - this can take awhile')
all_trips = pd.read_csv(trips_file, sep = '\t')
cols_to_keep = ['hhno','opurp','dpurp','mode','travtime','travdist']
all_trips = all_trips[cols_to_keep]

print('Adding Parcel ID to the Trips records so that they can be aggregated by Parcel')
all_trips = pd.merge(all_trips, hh_to_parcel, on='hhno', suffixes=('_x','_y'), how='left')

print('Trimming Down Trips list to only be trips by low income households')
low_income = hh_df[hh_df['hhincome'] <= low_income_threshold]
low_income_hhs = low_income['hhno'].unique().tolist()
low_income_trips = all_trips[all_trips['hhno'].isin(low_income_hhs)]

print('Trimming Down Trips list to only inlcude trips by households with elderly people in it')
person_variables=['hhno','pagey']
people = h5py.File(hh_person,'r+')
people_df = create_df_from_h5(people, 'Person', person_variables)
elderly = people_df[people_df['pagey'] >= elderly_threshold]
elderly = elderly.groupby(['hhno']).sum().reset_index()
elderly_hhs = elderly['hhno'].unique().tolist()
elderly_trips = all_trips[all_trips['hhno'].isin(elderly_hhs)]

print('Dropping HHNO from trips files since hh parcel is included')
all_trips = all_trips.drop(['hhno'], axis = 1)
low_income_trips = low_income_trips.drop(['hhno'], axis = 1)
elderly_trips = elderly_trips.drop(['hhno'], axis = 1)

household_types = [['low income',low_income_trips], ['elderly',elderly_trips], ['all', all_trips]]
for hhtype in household_types:
  
    #############################################################################################
    #############################################################################################
    ### Trips by Household for mode, time and distance
    #############################################################################################
    #############################################################################################

    print('Trimming the Trip file to hh, mode, time and distance for Home to Work and Work to Home trips for ' + hhtype[0])
    work_trips = hhtype[1]
    work_trips = work_trips.loc[((work_trips['opurp'] == 0) & (work_trips['dpurp'] == 1)) | ((work_trips['opurp'] == 1) & (work_trips['dpurp'] == 0))]
    work_trips = work_trips.drop(['opurp', 'dpurp'], axis = 1)
    work_parcel_trips = create_parcel_trip_mode_summary(work_trips, 'commute')
    work_parcel_trips = work_parcel_trips.groupby(['parcel_id']).sum().reset_index()
    
    print('Trimming the Trip file to hh, mode, time and distance for all non-work trips for ' + hhtype[0])
    nonwork_trips = hhtype[1]
    nonwork_trips = nonwork_trips.loc[(nonwork_trips['dpurp'] != 1) | (nonwork_trips['opurp'] != 1)]
    nonwork_trips = nonwork_trips.drop(['opurp', 'dpurp'], axis = 1) 
    nonwork_parcel_trips = create_parcel_trip_mode_summary(nonwork_trips, 'noncommute')
    nonwork_parcel_trips = nonwork_parcel_trips.groupby(['parcel_id']).sum().reset_index()

    print('Adding geographic lookups to the combined parcel data for ' + hhtype[0])
    parcel_data = pd.merge(all_parcels, work_parcel_trips, on='parcel_id', suffixes=('_x','_y'), how='left')
    parcel_data = pd.merge(parcel_data, nonwork_parcel_trips, on='parcel_id', suffixes=('_x','_y'), how='left')
    parcel_data = parcel_data.fillna(0)
    parcel_data['region_name'] = 'PSRC'

    #############################################################################################
    #############################################################################################
    ### Summarize Trips by Geography
    #############################################################################################
    #############################################################################################
    
    trip_summaries = [['commute', time_buckets, 'time'],
                      ['commute', distance_buckets, 'distance'],
                      ['noncommute', time_buckets, 'time'],
                      ['noncommute', distance_buckets, 'distance']]

    geographic_types = [['place_name', 'pl'],['county_nm','co'],['tract_id','tr'],['region_name', 're']]
    
    trip_purposes = ['commute','noncommute']
    
    for summaries in trip_summaries:
        print('Working on ' + summaries[0] + ' ' + summaries[2] + 'for ' + hhtype[0])
        
        for geographies in geographic_types:
            print('Working on ' + summaries[0] + ' ' + summaries[2] + ' for '+ geographies[0] + ' for ' + hhtype[0])
            current_summary = summarize_trips(parcel_data, geographies[0], geographies[1], current_year, summaries[0], summaries[1], summaries[2])
            current_summary['variable_hhtype'] = hhtype[0]
            
            if final_summary.size == 0:
                final_summary = current_summary
            
            else:
                final_summary = final_summary.append(current_summary, sort=False)
                
    for geographies in geographic_types:
        for purpose in trip_purposes:
            print('Working on summary of total ' + purpose + ' trips  ' + ' for '+ geographies[0] + ' for ' + hhtype[0])
            current_summary = summarize_trip_totals(parcel_data, geographies[0], geographies[1], current_year, purpose)
            current_summary['variable_hhtype'] = hhtype[0]
            final_summary = final_summary.append(current_summary, sort=False)
            
print('Writing final trip summary dataframe to csv for further analysis')
final_summary = final_summary.fillna(0)
final_summary.to_csv('trips_model_output_summary_data_' + str(current_year) + '.csv',index=False)

print('All done.')
