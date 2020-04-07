import os
import h5py
import pandas as pd

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

analysis_years = ['2014','2025','2040','2050']

i1 = 25000
i2 = 50000
i3 = 75000
i4 = 100000
i5 = 150000
i6 = 200000

hhsize_descriptions = ['1 person','2 people','3 people','4 people','5 or more','Total-Households']
hhtype_descriptions = ['Single-Family','Multi-Family','Mobile-Home','Total-Households']
hhownership_descriptions = ['Own','Rent','Total-Households']
hhincome_descriptions = ['less than $25k','$25k to $50k','$50k to $75k','$75k to $100k','$100k to $150k','$150k to $200k','more than $200k','Median Income', 'Total-Households']
gender_descriptions = ['Male','Female','Total-Population']
age_descriptions = ['0-17','18-64','65-84','85+','Median Age','Total-Population']
worker_descriptions = ['Worker','Not-a-Worker','Total-Population']
student_descriptions = ['Student','Not-a-Student','Total-Population']
job_descriptions = ['Construction','Manufacturing','WTU','Retail','Services','Government','Education','Total-Jobs']

working_directory = os.getcwd()
input_directory = os.path.join(working_directory,'inputs')
output_directory = os.path.join(working_directory,'output')

# Create the output directory for the trip generation results
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

# Parcel Columns to use and what to rename them
original_parcel_columns = ['PARCELID','XCOORD_P','YCOORD_P','EMPEDU_P','EMPFOO_P','EMPGOV_P','EMPIND_P','EMPMED_P','EMPOFC_P','EMPRET_P','EMPRSC_P','EMPSVC_P','EMPOTH_P','EMPTOT_P']
final_parcel_columns = ['parcel_id','xcoord','ycoord','Construction','Manufacturing','WTU','Retail','Services','Government','Education','Total-Jobs']

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
        working_cols.append(working_var_name + '_' + current_descr)
        
    current_df.columns = working_cols
    
    print('Grouping all parcel results by ' + working_geo + ' for year ' + str(current_year))
    place_results =current_df.groupby([working_geo]).sum().reset_index()
                   
    print('Converting from wide to long and creating final columns for ' + working_geo + ' for year ' + str(current_year))
    final_places = pd.melt(place_results, id_vars=[working_geo])
    final_places['place_type'] = working_pl_type
    final_places['year'] = working_year
    final_places['data_source'] = 'UrbanSim'
    final_places[['variable_name','variable_description']] = final_places.variable.str.split("_",expand=True,)
    final_places = final_places.drop(['variable'], axis = 1)
    final_places  = final_places.rename(columns={'value':'estimate'})
    final_places  = final_places.rename(columns={working_geo:'geog_name'})

    print('Calculating shares of total for each place')
    final_places['temp'] = 0
    for ind in final_places.index: 
        current_name = final_places['geog_name'][ind]
        total_estimate = final_places[(final_places['geog_name'] == current_name) & (final_places['variable_description'] == working_total)]['estimate'].values[0]
        final_places['temp'][ind] = total_estimate
            
    final_places['share'] = final_places['estimate'] / final_places['temp']
    final_places = final_places.drop(['temp'], axis = 1)
    
    if 'total' in working_var_name:
        print('Total results are being kept')
    else:
        final_places = final_places[~final_places['variable_description'].str.contains('Total')]
     
    final_places.loc[final_places['variable_description'].str.contains('Median'), 'estimate'] = final_places['share']
    
    return(final_places)

print('Loading parcels with overlays from disk')
parcel_overlay_file = os.path.join(input_directory,'shapefiles','wgs1984','parcels_with_overlays.csv')
all_parcels = pd.read_csv(parcel_overlay_file, low_memory=False)  
   
# Cleanup of a few geograhpy names
all_parcels.loc[all_parcels['place_name'].isna(), 'place_name'] = 'Unincorporated'
all_parcels.loc[all_parcels['county_nm'] == 'Kittitas County', 'county_nm'] = 'King County'
all_parcels.loc[all_parcels['county_nm'] == 'Lewis County', 'county_nm'] = 'Pierce County'
all_parcels.loc[all_parcels['county_nm'] == 'Mason County', 'county_nm'] = 'Kitsap County'
all_parcels.loc[all_parcels['county_nm'] == 'Thurston County', 'county_nm'] = 'Pierce County'

final_summary = pd.DataFrame()

for current_year in analysis_years:

    hh_person = os.path.join(input_directory,current_year,'hh_and_persons.h5')
    parcel_file = os.path.join(input_directory,current_year,'parcels_urbansim.txt')
     
    #############################################################################################
    #############################################################################################
    ### People and Jobs on Parcels
    #############################################################################################
    #############################################################################################

    print('Opening parcel inputs into a pandas dataframe for year ' + str(current_year))
    parcels = pd.read_csv(parcel_file, sep = ' ')
    parcels.columns = parcels.columns.str.upper()
    parcels = parcels.loc[:,original_parcel_columns]
    
    print('Adjusting Job Sectors  to match Published Job Sectors')
    parcels['Construction'] = parcels['EMPRSC_P'] + parcels['EMPIND_P'] * 0.28
    parcels['Manufacturing'] = parcels['EMPIND_P'] * 0.38
    parcels['WTU'] = parcels['EMPIND_P'] * 0.34
    parcels['Retail'] = parcels['EMPRET_P']
    parcels['Services'] = (parcels['EMPEDU_P'] * 0.2196) + parcels['EMPFOO_P'] + parcels['EMPMED_P'] + parcels['EMPOFC_P'] + parcels['EMPSVC_P'] + parcels['EMPOTH_P']
    parcels['Government'] = parcels['EMPGOV_P']
    parcels['Education'] = parcels['EMPEDU_P'] * 0.7804
    parcels['Total-Jobs'] = parcels['EMPTOT_P']
    
    print('Renaming a few columns and trimming parcels to final columns')
    parcels  = parcels.rename(columns={'PARCELID':'parcel_id'})
    parcels  = parcels.rename(columns={'XCOORD_P':'xcoord'})
    parcels  = parcels.rename(columns={'YCOORD_P':'ycoord'})
    parcels = parcels[final_parcel_columns]

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
    households['more than $200k'] = 0
    households['Median Income'] = households['hhincome']
    households.loc[households['hhincome'] <= i1, 'less than $25k'] = 1 
    households.loc[(households['hhincome'] > i1) & (households['hhincome'] <= i2), '$25k to $50k'] = 1 
    households.loc[(households['hhincome'] > i2) & (households['hhincome'] <= i3), '$50k to $75k'] = 1 
    households.loc[(households['hhincome'] > i3) & (households['hhincome'] <= i4), '$75k to $100k'] = 1 
    households.loc[(households['hhincome'] > i4) & (households['hhincome'] <= i5), '$100k to $150k'] = 1 
    households.loc[(households['hhincome'] > i5) & (households['hhincome'] <= i6), '$150k to $200k'] = 1  
    households.loc[households['hhincome'] > i6, 'more than $200k'] = 1 

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
    households['Single-Family'] = 0
    households['Multi-Family'] = 0
    households['Mobile-Home'] = 0
    households.loc[households['hrestype'] == 1, 'Single-Family'] = 1 
    households.loc[(households['hrestype'] == 2) | (households['hrestype'] == 3) | (households['hrestype'] == 0), 'Multi-Family'] = 1 
    households.loc[(households['hrestype'] == 4), 'Mobile-Home'] = 1 

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
    person_df['Median Age'] = person_df['pagey']
    person_df.loc[person_df['pagey'] < 18, '0-17'] = 1 
    person_df.loc[(person_df['pagey'] >= 18) & (person_df['pagey'] < 65), '18-64'] = 1 
    person_df.loc[(person_df['pagey'] >= 65) & (person_df['pagey'] < 85), '65-84'] = 1 
    person_df.loc[person_df['pagey'] >= 85, '85+'] = 1 

    print('Adding Worker Status for year ' + str(current_year))
    person_df['Worker'] = 0
    person_df['Not-a-Worker'] = 0
    person_df.loc[person_df['pwtyp'] == 0, 'Not-a-Worker'] = 1 
    person_df.loc[(person_df['pwtyp'] == 1) | (person_df['pwtyp'] == 2) , 'Worker'] = 1 

    print('Adding Student Status for year ' + str(current_year)) 
    person_df['Student'] = 0
    person_df['Not-a-Student'] = 0
    person_df.loc[person_df['pstyp'] == 0, 'Not-a-Student'] = 1 
    person_df.loc[(person_df['pstyp'] == 1) | (person_df['pstyp'] == 2), 'Student'] = 1 

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
    
    demographic_summaries = [['household-size', hhsize_descriptions, 'Total-Households'],
                             ['household-type', hhtype_descriptions,'Total-Households'],
                             ['household-ownership', hhownership_descriptions, 'Total-Households'],
                             ['household-income', hhincome_descriptions, 'Total-Households'],
                             ['gender', gender_descriptions, 'Total-Population'],
                             ['age', age_descriptions, 'Total-Population'],
                             ['worker', worker_descriptions, 'Total-Population'],
                             ['student', student_descriptions, 'Total-Population'],
                             ['jobs', job_descriptions, 'Total-Jobs'],
                             ['total-households',['Total-Households'],'Total-Households'],
                             ['total-population',['Total-Population'],'Total-Population'],
                             ['total-jobs',['Total-Jobs'],'Total-Jobs']
                             ]
    
    geographic_types = [['place_name', 'pl'],['county_nm','co']]
      
    for summaries in  demographic_summaries:
        print('Working on ' + summaries[0] + ' for the year ' + str(current_year))
        
        for geographies in geographic_types:
            print('Working on ' + summaries[0] + ' for '+ geographies[0] + ' for the year ' + str(current_year))
            current_summary = summarize_demographic_data(parcel_data, geographies[0], geographies[1], current_year, summaries[0], summaries[1], summaries[2])
        
            if final_summary.size == 0:
                final_summary = current_summary
            
            else:
                final_summary = final_summary.append(current_summary, sort=False)


print('Writing final trip summary dataframe to csv for further analysis')
final_summary.to_csv(os.path.join(output_directory,'demographic_model_data_for_rtp.csv'),index=False)

print('All done.')
