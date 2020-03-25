import os
import h5py
import pandas as pd

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

analysis_years = ['2014','2025','2040','2050']

low_low = 25000
low = 50000
med_low = 75000
med = 100000
med_high = 150000
high_low = 200000
high = 250000

size_categories = ['1 person','2 people','3 people','4 people','5 or more','Total-Households']
type_categories = ['Single-family','Multi-family','Condo','Mobile-home','Total-Households']
ownership_categories = ['Own','Rent','Total-Households']
income_categories = ['less than $25k','$25k to $50k','$50k to $75k','$75k to $100k','$100k to $150k','$150k to $200k','$200k to $250k','more than $250k','Total-Households']
gender_categories = ['Male','Female','Total-Population']
age_categories = ['0-17','18-64','65-84','85+','Total-Population']
worker_categories = ['Full-Time-Worker','Part-Time-Worker','Not-a-Worker','Total-Population']
student_categories = ['Full-Time-Student','Part-Time-Student','Not-a-Student','Total-Population']
job_categories = ['Education','Food-Service','Government','Industrial','Medical','Office','Retail','Resource','Service','Others','Total-Jobs']

working_path = os.getcwd()

# Parcel Columns to use and what to rename them
original_parcel_columns = ['PARCELID','XCOORD_P','YCOORD_P','EMPEDU_P','EMPFOO_P','EMPGOV_P','EMPIND_P','EMPMED_P','EMPOFC_P','EMPRET_P','EMPRSC_P','EMPSVC_P','EMPOTH_P','EMPTOT_P']
updated_parcel_columns = ['parcel_id','xcoord','ycoord','Education','Food-Service','Government','Industrial','Medical','Office','Retail','Resource','Service','Others','Total-Jobs']

# Lists for HH and Person Files
hh_variables=['hhno','hhsize','hhparcel','hhincome','hownrent','hrestype']
person_variables=['pno','hhno','pgend','pagey','pwtyp','pstyp']

# Function to create a datframe from the H5 files from Urbansim
def create_df_from_h5(h5_file, h5_table, h5_variables):

    h5_data = {}
    
    for var in h5_variables:
        h5_data[var] = h5_file[h5_table][var][:]
    
    return pd.DataFrame(h5_data)

def summarize_demographic_data(current_df, working_geo, working_pl_type, working_year, working_var_name, working_var_descr, working_total):
      
    print('Dropping all geographies except ' + working_geo + ' for year '+ str(working_year))
    working_cols = [working_geo]

    for current_var in working_var_descr:
        working_cols.append(current_var)

    current_df = current_df[working_cols]
    
    working_cols = [working_geo]

    for current_descr in working_var_descr:
        working_cols.append('total_'+ working_var_name + '_' + current_descr)
        
    current_df.columns = working_cols
    
    print('Grouping all parcel results by ' + working_geo + ' for year ' + str(current_year))
    place_results =current_df.groupby([working_geo]).sum().reset_index()
           
    print('Calculating share of total for ' + working_var_name)
    for current_descr in working_var_descr: 
        place_results['share_'+ working_var_name + '_' + current_descr] = place_results['total_'+ working_var_name + '_' + current_descr] / place_results['total_'+ working_var_name + '_' + working_total]
   
    place_results = place_results.fillna(0)
    
    if (working_var_name == "total-jobs" or working_var_name == "total-population" or working_var_name == "total-households"):
        place_results = place_results.drop(['share_'+ working_var_name + '_' + working_total], axis = 1)
    else:
        place_results = place_results.drop(['total_'+ working_var_name + '_' + working_total, 'share_'+ working_var_name + '_' + working_total], axis = 1)    
        
    

    print('Converting from wide to long and creating final columns for ' + working_geo + ' for year ' + str(current_year))
    final_places = pd.melt(place_results, id_vars=[working_geo])
    final_places['place_type'] = working_pl_type
    final_places['year'] = working_year
    final_places[['variable_category','variable_name','variable_description']] = final_places.variable.str.split("_",expand=True,)
    final_places = final_places.drop(['variable'], axis = 1)
    final_places  = final_places.rename(columns={'value':'estimate'})
    final_places  = final_places.rename(columns={working_geo:'geog_name'})
    
    return(final_places)

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

for current_year in analysis_years:

    hh_person = os.path.join(working_path,'inputs',current_year,'hh_and_persons.h5')
    parcel_file = os.path.join(working_path,'inputs',current_year,'parcels_urbansim.txt')
     
    #############################################################################################
    #############################################################################################
    ### People and Jobs on Parcels
    #############################################################################################
    #############################################################################################

    print('Opening parcel inputs into a pandas dataframe for year ' + str(current_year))
    parcels = pd.read_csv(parcel_file, sep = ' ')
    parcels.columns = parcels.columns.str.upper()
    parcels = parcels.loc[:,original_parcel_columns]
    parcels.columns = updated_parcel_columns

    print('Opening HH and Person H5 file and creating dataframes from them for year ' + str(current_year))
    hh_people = h5py.File(hh_person,'r+')
    households = create_df_from_h5(hh_people, 'Household', hh_variables)
    person_df = create_df_from_h5(hh_people, 'Person', person_variables)
    
    #############################################################################################
    #############################################################################################
    ### Household Categories
    #############################################################################################
    #############################################################################################

    print('Adding Household Income Category for year ' + str(current_year)) 
    households['less than $25k'] = 0
    households['$25k to $50k'] = 0
    households['$50k to $75k'] = 0
    households['$75k to $100k'] = 0
    households['$100k to $150k'] = 0
    households['$150k to $200k'] = 0
    households['$200k to $250k'] = 0
    households['more than $250k'] = 0
    households.loc[households['hhincome'] <= low_low, 'less than $25k'] = 1 
    households.loc[(households['hhincome'] > low_low) & (households['hhincome'] <= low), '$25k to $50k'] = 1 
    households.loc[(households['hhincome'] > low) & (households['hhincome'] <= med_low), '$50k to $75k'] = 1 
    households.loc[(households['hhincome'] > med_low) & (households['hhincome'] <= med), '$75k to $100k'] = 1 
    households.loc[(households['hhincome'] > med) & (households['hhincome'] <= med_high), '$100k to $150k'] = 1 
    households.loc[(households['hhincome'] > med_high) & (households['hhincome'] <= high_low), '$150k to $200k'] = 1 
    households.loc[(households['hhincome'] > high_low) & (households['hhincome'] <= high), '$200k to $250k'] = 1 
    households.loc[households['hhincome'] > high, 'more than $250k'] = 1 

    print('Adding Household Size Category for year ' + str(current_year))
    households['1 person'] = 0
    households['2 people'] = 0
    households['3 people'] = 0
    households['4 people'] = 0
    households['5 or more'] = 0
    households.loc[households['hhsize'] == 1, '1 person'] = 1 
    households.loc[households['hhsize'] == 2, '2 people'] = 1 
    households.loc[households['hhsize'] == 3, '3 people'] = 1 
    households.loc[households['hhsize'] == 4, '4 people'] = 1 
    households.loc[households['hhsize'] >= 5, '5 or more'] = 1 

    print('Adding Household Type for year ' + str(current_year)) 
    households['Single-family'] = 0
    households['Multi-family'] = 0
    households['Condo'] = 0
    households['Mobile-home'] = 0
    households.loc[households['hrestype'] == 1, 'Single-family'] = 1 
    households.loc[(households['hrestype'] == 2), 'Multi-family'] = 1 
    households.loc[(households['hrestype'] == 3) | (households['hrestype'] == 0), 'Condo'] = 1 
    households.loc[(households['hrestype'] == 4), 'Mobile-home'] = 1 

    print('Adding ownership Type for year ' + str(current_year))
    households['Own'] = 0
    households['Rent'] = 0
    households.loc[households['hownrent'] == 1, 'Own'] = 1 
    households.loc[households['hownrent'] >= 2 ,'Rent'] = 1 

    households['Total-Households'] = 1

    households = households.drop(['hhsize', 'hhincome', 'hownrent', 'hrestype'], axis = 1)

    #############################################################################################
    #############################################################################################
    ### Person Categories
    #############################################################################################
    #############################################################################################

    print('Adding Gender for year ' + str(current_year)) 
    person_df['Male'] = 0
    person_df['Female'] = 0
    person_df.loc[person_df['pgend'] == 1, 'Male'] = 1 
    person_df.loc[(person_df['pgend'] >= 2) | (person_df['pgend'] == 0), 'Female'] = 1 

    print('Adding Age for year ' + str(current_year))
    person_df['0-17'] = 0
    person_df['18-64'] = 0
    person_df['65-84'] = 0
    person_df['85+'] = 0
    person_df.loc[person_df['pagey'] < 18, '0-17'] = 1 
    person_df.loc[(person_df['pagey'] >= 18) & (person_df['pagey'] < 65), '18-64'] = 1 
    person_df.loc[(person_df['pagey'] >= 65) & (person_df['pagey'] < 85), '65-84'] = 1 
    person_df.loc[person_df['pagey'] >= 85, '85+'] = 1 

    print('Adding Worker Status for year ' + str(current_year))
    person_df['Full-Time-Worker'] = 0
    person_df['Part-Time-Worker'] = 0
    person_df['Not-a-Worker'] = 0
    person_df.loc[person_df['pwtyp'] == 0, 'Not-a-Worker'] = 1 
    person_df.loc[person_df['pwtyp'] == 1, 'Full-Time-Worker'] = 1 
    person_df.loc[person_df['pwtyp'] == 2, 'Part-Time-Worker'] = 1 

    print('Adding Student Status for year ' + str(current_year)) 
    person_df['Full-Time-Student'] = 0
    person_df['Part-Time-Student'] = 0
    person_df['Not-a-Student'] = 0
    person_df.loc[person_df['pstyp'] == 0, 'Not-a-Student'] = 1 
    person_df.loc[person_df['pstyp'] == 1, 'Full-Time-Student'] = 1 
    person_df.loc[person_df['pstyp'] == 2, 'Part-Time-Student'] = 1 

    person_df['Total-Population'] = 1

    print('Aggregating people results by Housholds for year ' + str(current_year))
    person_df = person_df.drop(['pno','pgend','pagey','pwtyp','pstyp'], axis = 1)
    population = person_df.groupby(['hhno']).sum().reset_index()
    
    print('Joining the Household and Population Data into one dataframe for year ' + str(current_year))
    demographic_results = pd.merge(households, population, on='hhno', suffixes=('_x','_y'), how='left')
    
    print('Aggregating combined household and population data by parcel-id for year ' + str(current_year))
    demographic_results = demographic_results.rename(columns={'hhparcel':'parcel_id'})
    demographic_results = demographic_results.groupby(['parcel_id']).sum().reset_index()
    demographic_results = demographic_results.drop(['hhno'], axis=1)
    
    print('Merging combined household and population data by parcel-id with jobs data for year ' + str(current_year))
    parcel_data = pd.merge(parcels, demographic_results, on='parcel_id', suffixes=('_x','_y'), how='left')
    parcel_data = parcel_data.drop(['xcoord','ycoord'], axis=1)
    
    print('Adding geographic lookups to the combined parcel data for year ' + str(current_year))
    parcel_data = pd.merge(all_parcels, parcel_data, on='parcel_id', suffixes=('_x','_y'), how='left')
    parcel_data = parcel_data.fillna(0)
    parcel_data['region_name'] = 'PSRC'
    
    #############################################################################################
    #############################################################################################
    ### Combine person and household results by parcel
    #############################################################################################
    #############################################################################################
    
    demographic_summaries = [['household-size', size_categories, 'Total-Households'],
                             ['household-type', type_categories,'Total-Households'],
                             ['household-ownership', ownership_categories, 'Total-Households'],
                             ['household-income', income_categories, 'Total-Households'],
                             ['gender', gender_categories, 'Total-Population'],
                             ['age', age_categories, 'Total-Population'],
                             ['worker', worker_categories, 'Total-Population'],
                             ['student', student_categories, 'Total-Population'],
                             ['jobs', job_categories, 'Total-Jobs'],
                             ['total-households',['Total-Households'],'Total-Households'],
                             ['total-population',['Total-Population'],'Total-Population'],
                             ['total-jobs',['Total-Jobs'],'Total-Jobs']
                             ]
    
    geographic_types = [['place_name', 'pl'],['county_nm','co'],['tract_id','tr'],['region_name', 're']]
      
    for summaries in  demographic_summaries:
        print('Working on ' + summaries[0] + ' for the year ' + str(current_year))
        
        for geographies in geographic_types:
            print('Working on ' + summaries[0] + ' for '+ geographies[0] + ' for the year ' + str(current_year))
            current_summary = summarize_demographic_data(parcel_data, geographies[0], geographies[1], current_year, summaries[0], summaries[1], summaries[2])
        
            if final_summary.size == 0:
                final_summary = current_summary
            
            else:
                final_summary = final_summary.append(current_summary, sort=False)

final_summary['variable_mode'] = ""
final_summary['variable_hhtype'] = "all"    

print('Writing final trip summary dataframe to csv for further analysis')
final_summary.to_csv('demographic_model_output_summary_data_all_years.csv',index=False)

print('All done.')
