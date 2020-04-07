# Load the libraries we need
import pandas as pd
import urllib
import os
import geopandas as gp
import zipfile
import getpass 
import shutil
import glob 
import requests

working_directory = os.getcwd()
input_directory = os.path.join(working_directory,'inputs')
output_directory = os.path.join(working_directory,'output')
temp_path = os.path.join('c:\\Users',getpass.getuser(),'Downloads')

block_shapefile = os.path.join(input_directory,'shapefiles','wgs1984','blocks.shp')
block_dbf = os.path.join(input_directory,'shapefiles','wgs1984','blocks.dbf')
place_shapefile = os.path.join(input_directory,'shapefiles','wgs1984','places.shp')
wgs_84 = '4326'

variable_names = os.path.join(input_directory,'census-variable-names.csv')
tract_jobs =  os.path.join(input_directory,'tract_jobs.csv')

# Create the output directory for the trip generation results
if not os.path.exists(output_directory):
    os.makedirs(output_directory)

# This option will supress the warning message on possbile copy issues - confirmed it is working as desired so turning it off
pd.set_option('chained_assignment',None)

acs_data = ['5yr']
analysis_years = ['2014','2015','2017','2018']
api_key = '6d9263105b3ca3213e093323b4ece211ab49d4e5'
data_tables = ['B01001','B01002','B08301','B08133','B08134','B11011','B11016','B14001','B19001','B19013','B23025','B25026']
data_set = 'acs/acs5'

# LODES to Employment Sectors
construction_resources = [['CNS01',1],['CNS02',1],['CNS04',1]]
manufacturing = [['CNS05',1]]
wtu = [['CNS03',1],['CNS06',1],['CNS08',1]]
retail = [['CNS07',1]]
services = [['CNS09',1],['CNS10',1],['CNS11',1],['CNS12',1],['CNS13',1],['CNS14',1],['CNS15',0.2196],['CNS16',1],['CNS17',1],['CNS18',1],['CNS19',1]]
government = [['CNS20',1]]
public_ed = [['CNS15', 0.7804]]

job_sectors = [['Construction',construction_resources],['Manufacturing',manufacturing],['WTU',wtu],
               ['Retail',retail],['Services',services],['Government',government],['Education',public_ed]]

def download_census_shapes(working_url,working_zip):
    
    with urllib.request.urlopen(working_url) as response, open(working_zip, 'wb') as out_file:
        shutil.copyfileobj(response, out_file)

    # Uncompress the Shapefile for use in the analysis and remove the zipfile
    working_archive = zipfile.ZipFile(working_zip, 'r')
    working_archive.extractall(temp_path)
    working_archive.close()
    os.remove(working_zip)

def get_data_profile(current_call,place_type,current_table):
    
    response = urllib.request.urlopen(current_call)
    census_data = response.read()
    working_df = pd.read_json(census_data)
    working_df = working_df.rename(columns=working_df.iloc[0]).drop(working_df.index[0])

    working_df = pd.melt(working_df, id_vars=['NAME','GEO_ID']) 

    # Clean up the data profiles to only include the estimate and margins of error
    working_df = working_df[~working_df['variable'].str.contains('EA')]
    working_df = working_df[~working_df['variable'].str.contains('MA')]
    working_df = working_df[~working_df['variable'].str.contains('PEA')]
    working_df = working_df[~working_df['variable'].str.contains('PMA')]
    working_df = working_df[working_df['variable'].str.contains(current_table)]
    
    working_df['place_type'] = place_type
    working_df['table'] = current_table
  
    return working_df

def spatial_join(target_shapefile,join_shapefile,keep_columns):
    
    # open join shapefile as a geodataframe
    join_layer = gp.GeoDataFrame.from_file(join_shapefile)
    target_layer = gp.GeoDataFrame.from_file(target_shapefile)
    
    # Create PSRC Flag in the Join Layer and trim down before joining
    join_layer['PSRC'] = 0
    join_layer.loc[(join_layer['COUNTYFP'] == '033')|(join_layer['COUNTYFP'] == '035')|(join_layer['COUNTYFP'] == '053')|(join_layer['COUNTYFP'] == '061'), 'PSRC'] = 1
    cols_to_keep = ['geometry','PSRC']
    join_layer = join_layer[cols_to_keep]
    
    # spatial join
    merged = gp.sjoin(target_layer, join_layer, how = "inner", op='intersects')
    merged = pd.DataFrame(merged)
    merged = merged[keep_columns]
    
    return merged

def gp_join(target_layer,join_shapefile,coord_sys,keep_columns):
    
    # open join shapefile as a geodataframe
    join_layer = gp.GeoDataFrame.from_file(join_shapefile)
    join_layer.crs = {'init' :coord_sys}

    # spatial join
    merged = gp.sjoin(target_layer, join_layer, how = "inner", op='intersects')
    merged = pd.DataFrame(merged)
    merged = merged[keep_columns]
    
    return merged

def create_point_from_polygon(polygon_shape,coord_sys):
    poly = gp.read_file(polygon_shape)
    points = poly.copy()
    points.geometry = points['geometry'].centroid
    points.crs = {'init' :coord_sys}
    
    # create a geodataframe for points and return
    geo_layer = gp.GeoDataFrame(points, geometry='geometry')
    geo_layer.crs = {'init' :coord_sys}
    
    return geo_layer

##################################################################################################
##################################################################################################    
#  Dictionary of output types from the dataprofiles
##################################################################################################
################################################################################################## 
api_outputs = {'E':'estimate',
               'M':'margin_of_error',
               'PE':'percent',
               'PM':'percent_margin_of_error'}

numeric_columns = ['estimate','margin_of_error','percent','percent_margin_of_error']

##################################################################################################
##################################################################################################    
#  Create Geographic lookups for Census Blocks
##################################################################################################
################################################################################################## 
print('Creating block lookup with Place Name, Tract ID and County')
keep_columns = ['block_id']
all_blocks = gp.read_file(block_dbf)
all_blocks = all_blocks[keep_columns]
all_blocks['county'] = all_blocks.block_id.str[2:5]
all_blocks['tract_id'] = all_blocks.block_id.str[0:11]

keep_columns = ['block_id','place_name']
block_layer = create_point_from_polygon(block_shapefile,wgs_84)
merged_blocks = gp_join(block_layer,place_shapefile,wgs_84,keep_columns)

all_blocks = pd.merge(all_blocks, merged_blocks, on='block_id', suffixes=('_x','_y'), how='left')
all_blocks = all_blocks.fillna("Unincorporated")

all_blocks.loc[all_blocks['county'] == '033', 'county_nm'] = 'King County'
all_blocks.loc[all_blocks['county'] == '035', 'county_nm'] = 'Kitsap County'
all_blocks.loc[all_blocks['county'] == '053', 'county_nm'] = 'Pierce County'
all_blocks.loc[all_blocks['county'] == '061', 'county_nm'] = 'Snohomish County'

final_df = pd.DataFrame()

for acs_data_type in acs_data:

    for year in analysis_years:
    
        all_profiles = pd.DataFrame()
        
        ##################################################################################################
        ##################################################################################################    
        #  LODES Employment Data
        ##################################################################################################
        ##################################################################################################   
        
        if year == '2018':
            lodes_year = '2017'
        else:
            lodes_year = year
        
        lodes_url = 'https://lehd.ces.census.gov/data/lodes/LODES7/wa/wac/wa_wac_S000_JT00_' + lodes_year + '.csv.gz'
        lodes_zip = 'wa_wac_S000_JT00_' + lodes_year + '.csv.gz'
        lodes_file =  os.path.join(working_directory,lodes_zip)
        

        print('Downloading the LEHD LODES data and uncompressing for year ' + year)
        r = requests.get(lodes_url, allow_redirects=True)
        open(lodes_zip, 'wb').write(r.content)
        lodes_df = pd.read_csv(lodes_file, compression='gzip')

        print('Loading Jobs by sector and census tract')
        tract_jobs_df = pd.read_csv(tract_jobs)
        tract_jobs_df['tract_id'] = tract_jobs_df['tract_id'].apply(str)
        cols_to_keep = ['tract_id',year]
        tract_jobs_df = tract_jobs_df[cols_to_keep]
        final_cols = ['tract_id','total_jobs']
        tract_jobs_df.columns = final_cols

        print('Cleaning up columns in LODES download')
        emp_cols = ['w_geocode','C000']
        for n in range(1,21):
            if n < 10:
                emp_cols.append('CNS0'+str(n))
            else:
                emp_cols.append('CNS'+str(n))
        
        lodes_df = lodes_df[emp_cols]

        print('Trimming LODES data to only include PSRC Census Blocks')
        lodes_df['w_geocode'] = lodes_df['w_geocode'].apply(str)
        lodes_df['tract_id'] = lodes_df.w_geocode.str[0:11]
        lodes_df['county'] = lodes_df.w_geocode.str[2:5]
        lodes_df = lodes_df[(lodes_df['county'] == '033') | (lodes_df['county'] == '035') | (lodes_df['county'] == '053') | (lodes_df['county'] == '061')]

        ##################################################################################################
        ##################################################################################################    
        #  Creating Tract Level Totals by Sector from Total PSRC Covered Estimates by Tract
        ##################################################################################################
        ##################################################################################################
        print('Calculating Tract Sector Totals and adjusting with Tract Total Employment')
        tracts_control = lodes_df.groupby(['tract_id']).sum().reset_index()
        emp_cols = ['tract_id','C000_tract_control']
        for n in range(1,21):
            if n < 10:
                emp_cols.append('CNS0'+str(n)+'_tract_control')
            else:
                emp_cols.append('CNS'+str(n)+'_tract_control')

        tracts_control.columns =  emp_cols

        for n in range(1,21):
            if n < 10:
                tracts_control['CNS0'+str(n)+'_share'] = tracts_control['CNS0'+str(n)+'_tract_control'] / tracts_control['C000_tract_control']
            else:
                tracts_control['CNS'+str(n)+'_share'] = tracts_control['CNS'+str(n)+'_tract_control'] / tracts_control['C000_tract_control']

        tracts_control = pd.merge(tracts_control, tract_jobs_df, on='tract_id',suffixes=('_x','_y'),how='left')
        tracts_control = tracts_control.fillna(0)

        print('Calculating adjusted tract jobs by sectors')
        tracts_control['C000_tract_control'] = tracts_control['total_jobs']

        for n in range(1,21):
            if n < 10:
                tracts_control['CNS0'+str(n)+'_tract_control'] = tracts_control['CNS0'+str(n)+'_share'] * tracts_control['C000_tract_control']
            else:
                tracts_control['CNS'+str(n)+'_tract_control'] = tracts_control['CNS'+str(n)+'_share'] * tracts_control['C000_tract_control']

        final_cols = ['tract_id', 'C000_tract_control']
        
        for n in range(1,21):
            if n < 10:
                final_cols.append('CNS0'+str(n)+'_tract_control')
            else:
                final_cols.append('CNS'+str(n)+'_tract_control')

        tracts_control = tracts_control[final_cols]

        ##################################################################################################
        ##################################################################################################    
        #  Creating Sector Shares of Tract Totals by Census Block
        ##################################################################################################
        ##################################################################################################
        print('Calculating Tract Sector Totals by Tract from LODES')
        tracts_lodes = lodes_df.groupby(['tract_id']).sum().reset_index()
        emp_cols = ['tract_id','C000_tract_lodes']
        
        for n in range(1,21):
            if n < 10:
                emp_cols.append('CNS0'+str(n)+'_tract_lodes')
            else:
                emp_cols.append('CNS'+str(n)+'_tract_lodes')

        tracts_lodes.columns =  emp_cols

        print('Merge tract level totals by sector with block level LODES estimates to calculate shares')
        lodes_shares_df = pd.merge(lodes_df, tracts_lodes, on='tract_id',suffixes=('_x','_y'),how='left')
        lodes_shares_df = lodes_shares_df.fillna(0)

        print('Calculating Share of Tract Totals by Employment Category for each Block')
        lodes_shares_df['C000_tract_share'] = lodes_shares_df['C000'] / lodes_shares_df['C000_tract_lodes']
        for n in range(1,21):
            if n < 10:
                lodes_shares_df['CNS0'+str(n)+'_tract_share'] = lodes_shares_df['CNS0'+str(n)] / lodes_shares_df['CNS0'+str(n)+'_tract_lodes']
            else:
                lodes_shares_df['CNS'+str(n)+'_tract_share'] = lodes_shares_df['CNS'+str(n)] / lodes_shares_df['CNS'+str(n)+'_tract_lodes']

        lodes_shares_df = lodes_shares_df.fillna(0)

        share_cols = ['w_geocode', 'tract_id','C000_tract_share']
        for n in range(1,21):
            if n < 10:
                share_cols.append('CNS0'+str(n)+'_tract_share')
            else:
                share_cols.append('CNS'+str(n)+'_tract_share')

        lodes_shares_df = lodes_shares_df[share_cols]

        ##################################################################################################
        ##################################################################################################    
        #  Combine Tract Controls with tract share by block and sector
        ##################################################################################################
        ##################################################################################################
        print('Calculating the adjsuted employment totals by census block')
        adjusted_lodes = pd.merge(lodes_shares_df, tracts_control, on='tract_id',suffixes=('_x','_y'),how='left')
        adjusted_lodes['C000_adjusted'] = adjusted_lodes['C000_tract_control'] * adjusted_lodes['C000_tract_share']

        for n in range(1,21):
            if n < 10:
                adjusted_lodes['CNS0'+str(n)+'_adjusted'] = adjusted_lodes['CNS0'+str(n)+'_tract_control'] * adjusted_lodes['CNS0'+str(n)+'_tract_share']
            else:
                adjusted_lodes['CNS'+str(n)+'_adjusted'] = adjusted_lodes['CNS'+str(n)+'_tract_control'] * adjusted_lodes['CNS'+str(n)+'_tract_share']

        final_cols = ['w_geocode', 'tract_id','C000_adjusted']
        for n in range(1,21):
            if n < 10:
                final_cols.append('CNS0'+str(n)+'_adjusted')
            else:
                final_cols.append('CNS'+str(n)+'_adjusted')

        adjusted_lodes = adjusted_lodes[final_cols]

        ##################################################################################################
        ##################################################################################################    
        #  Employment Data by Published sectors
        ##################################################################################################
        ##################################################################################################
        print('Combining NAICS codes into Published Employment Sector Results for Blocks')
        final_cols = ['w_geocode','tract_id','C000_adjusted']
        for sectors in job_sectors: 
            adjusted_lodes[sectors[0]] = 0
            final_cols.append(sectors[0])
   
            for n in range(0,len(sectors[1])):
                adjusted_lodes[sectors[0]] = adjusted_lodes[sectors[0]] + (adjusted_lodes[sectors[1][n][0]+'_adjusted'] * sectors[1][n][1])
                adjusted_lodes[sectors[0]] = adjusted_lodes[sectors[0]].round()

        adjusted_lodes['C000_adjusted'] = adjusted_lodes['C000_adjusted'].round()
        adjusted_lodes = adjusted_lodes[final_cols]    
        adjusted_lodes  = adjusted_lodes.rename(columns={'w_geocode':'block_id'})
        adjusted_lodes  = adjusted_lodes.rename(columns={'C000_adjusted':'Total-Jobs'})
        adjusted_lodes = adjusted_lodes.drop(['tract_id'], axis = 1)

        print('Adding Jobs to the blocks with Place Names')
        block_jobs = pd.merge(all_blocks, adjusted_lodes, on='block_id',suffixes=('_x','_y'),how='left')
        block_jobs = block_jobs.fillna(0)

        #############################################################################################
        #############################################################################################
        ### LODES Data by Place
        #############################################################################################
        #############################################################################################        
        print('Dropping all geographies except Place Name')
        working_var_descr = ['Construction','Manufacturing','WTU','Retail','Services','Government','Education']
        working_cols = ['place_name','Total-Jobs']
        for current_descr in working_var_descr:
            working_cols.append(current_descr)

        current_df = block_jobs[working_cols]
    
        working_cols = ['place_name','Total-Jobs']
        for current_descr in working_var_descr:
            working_cols.append(current_descr)    
        current_df.columns = working_cols
    
        print('Grouping results by Place Name')
        place_results =current_df.groupby(['place_name']).sum().reset_index()
                
        print('Converting from wide to long and creating final columns for Places')
        final_places = pd.melt(place_results, id_vars=['place_name'])
        final_places['place_type'] = 'pl'
        final_places['variable_name'] = 'jobs'
        final_places['year'] = year
        final_places['data_source'] = 'lehd/lodes'
        final_places  = final_places.rename(columns={'value':'estimate'})
        final_places  = final_places.rename(columns={'variable':'variable_description'})
        final_places  = final_places.rename(columns={'place_name':'geog_name'})
        
        print('Calculating shares of total jobs for each place')
        final_places['temp'] = 0
        for ind in final_places.index: 
            current_name = final_places['geog_name'][ind]
            total_jobs = final_places[(final_places['geog_name'] == current_name) & (final_places['variable_description'] == 'Total-Jobs')]['estimate'].values[0]
            final_places['temp'][ind] = total_jobs
            
        final_places['share'] = final_places['estimate'] / final_places['temp']
        final_places = final_places.drop(['temp'], axis = 1)
        final_places.loc[final_places['variable_description'] == 'Total-Jobs', 'variable_name'] = 'total-jobs' 
        
        #############################################################################################
        #############################################################################################
        ### LODES Data by County
        #############################################################################################
        #############################################################################################
        print('Dropping all geographies except County Name')
        working_var_descr = ['Construction','Manufacturing','WTU','Retail','Services','Government','Education']
        working_cols = ['county_nm','Total-Jobs']
        for current_descr in working_var_descr:
            working_cols.append(current_descr)

        current_df = block_jobs[working_cols]
    
        working_cols = ['county_nm','Total-Jobs']
        for current_descr in working_var_descr:
            working_cols.append(current_descr)    
        current_df.columns = working_cols
    
        print('Grouping results by County')
        county_results =current_df.groupby(['county_nm']).sum().reset_index()
                
        print('Converting from wide to long and creating final columns for Places')
        final_county = pd.melt(county_results, id_vars=['county_nm'])
        final_county['place_type'] = 'co'
        final_county['variable_name'] = 'jobs'
        final_county['year'] = year
        final_county['data_source'] = 'lehd/lodes'
        final_county  = final_county.rename(columns={'value':'estimate'})
        final_county  = final_county.rename(columns={'variable':'variable_description'})
        final_county  = final_county.rename(columns={'county_nm':'geog_name'})
        
        print('Calculating shares of total jobs for each county')
        final_county['temp'] = 0
        for ind in final_county.index: 
            current_name = final_county['geog_name'][ind]
            total_jobs = final_county[(final_county['geog_name'] == current_name) & (final_county['variable_description'] == 'Total-Jobs')]['estimate'].values[0]
            final_county['temp'][ind] = total_jobs
            
        final_county['share'] = final_county['estimate'] / final_county['temp']
        final_county = final_county.drop(['temp'], axis = 1)
        final_county.loc[final_county['variable_description'] == 'Total-Jobs', 'variable_name'] = 'total-jobs' 

        ##################################################################################################
        ##################################################################################################    
        # Download the Census Shapefiles and create a lookup for places in Washington
        ##################################################################################################
        ##################################################################################################
        place_zip = temp_path + '\\tl_'+str(year)+'_53_place.zip'
        place_url = 'https://www2.census.gov/geo/tiger/TIGER'+str(year)+'/PLACE/tl_'+str(year)+'_53_place.zip'

        county_zip = temp_path + '\\tl_'+str(year)+'_53_cousub.zip'
        county_url = 'https://www2.census.gov/geo/tiger/TIGER'+str(year)+'/COUSUB/tl_'+str(year)+'_53_cousub.zip'

        print('Downloading the Census Place shapefile and uncompressing for year ' +year)
        download_census_shapes(place_url, place_zip)

        print('Downloading the Census County shapefile and uncompressing for year ' +year)
        download_census_shapes(county_url, county_zip)

        place_shapefile = os.path.join(temp_path,'tl_'+str(year)+'_53_place.shp')
        county_shapefile = os.path.join(temp_path,'tl_'+str(year)+'_53_cousub.shp')

        print('Creating a lookup of Place GEOIDs and a PSRC Flag')
        keep_columns = ['GEOID','PSRC']
        places = spatial_join(place_shapefile, county_shapefile, keep_columns)
        
        print('Opening variable names crosswalk to model outputs')
        var_names = pd.read_csv(variable_names, low_memory=False)  
        
        ##################################################################################################
        ##################################################################################################    
        # Create a master dataframe of all profiles for all geographies
        ##################################################################################################
        ################################################################################################## 

        for tables in data_tables:
            print('Downloading '+tables+' for year '+year+' ACS '+ acs_data_type + ' data')    
            current_profile = '?get=group('+tables+')'

            print('Downloading all Places in Washington')
            census_api_call = 'https://api.census.gov/data/' + str(year) + '/'+ data_set + current_profile + '&' + 'for=place:*' +'&in=state:53' + '&key=' + api_key
            interim = get_data_profile(census_api_call,'pl',tables)
            interim['GEOID'] = interim.GEO_ID.str[9:]
            interim = pd.merge(interim,places,on='GEOID',suffixes=('_x','_y'),how='left')
            interim = interim[~interim['NAME'].str.contains('CDP')]
            interim.drop_duplicates(keep = 'first', inplace = True)
            interim = interim.reset_index()
            interim = interim[interim['PSRC'] == 1]
            interim = interim.drop(['index','PSRC','GEO_ID'],axis=1)
           
            if all_profiles.empty:
                all_profiles = interim
            else:
                all_profiles = all_profiles.append(interim, sort=False)
        
            print('Downloading all Counties in PSRC Region')
            census_api_call = 'https://api.census.gov/data/' + str(year) + '/'+ data_set + current_profile + '&' + 'for=county:033,035,053,061' +'&in=state:53' + '&key=' + api_key
            interim = get_data_profile(census_api_call,'co',tables)
            interim['GEOID'] = interim.GEO_ID.str[9:]
            interim.drop_duplicates(keep = 'first', inplace = True)
            interim = interim.reset_index()
            interim = interim.drop(['index','GEO_ID'],axis=1)
            all_profiles = all_profiles.append(interim, sort=False)
                       
        print('Removing extra text from Census Place Names')
        all_profiles['NAME'] = all_profiles['NAME'].str.replace(', Washington','')
        all_profiles['NAME'] = all_profiles['NAME'].str.replace(' city','')
        all_profiles['NAME'] = all_profiles['NAME'].str.replace(' town','')
        all_profiles.columns = all_profiles.columns.str.lower()
        all_profiles = all_profiles[~all_profiles['variable'].str.contains('M')]
        
        print('Adding Variable Names that match Model Output Variable Names')
        all_profiles = pd.merge(all_profiles, var_names, on='variable', suffixes=('_x','_y'), how='left')
        all_profiles = all_profiles.dropna(subset=['variable_description'])
        
        print('Removing extra variable columns')
        all_profiles = all_profiles.drop(['variable','table','geoid'], axis = 1)
        all_profiles['value'] = pd.to_numeric(all_profiles['value'])
        
        print('Group rows by variable descriptions based on Model Buckets')
        place_results = all_profiles.groupby(['name','place_type','variable_name','variable_description']).sum().reset_index()
        place_results  = place_results.rename(columns={'value':'estimate'}) 
        place_results  = place_results.rename(columns={'name':'geog_name'}) 
        
        print('Calculating totals by variable for share calculations')
        place_totals = place_results.groupby(['geog_name','variable_name']).sum().reset_index()
        place_totals['join_field'] = place_totals['geog_name'] + place_totals['variable_name']
        place_totals  = place_totals.rename(columns={'estimate':'total-value'}) 
        place_totals = place_totals[['join_field','total-value']]
        
        print('Adding totals to the Place Results')
        place_results['join_field'] = place_results['geog_name'] + place_results['variable_name']
        place_results = pd.merge(place_results, place_totals, on='join_field', suffixes=('_x','_y'), how='left')
        place_results['share'] = place_results['estimate'] / place_results['total-value']
        place_results = place_results.drop(['join_field','total-value'], axis=1)
        
        print('Adding columns for use when combined with model output')
        place_results['year'] = year
        place_results['data_source'] = data_set
        
        print('Appending the yearly dataframes')
        if final_df.empty:
            final_df = place_results
        else:
            final_df = final_df.append(place_results)
            
        # Remove temporary census shapefiles
        for fl in glob.glob(temp_path + '\\tl_'+str(year)+'_53_*'):
            os.remove(fl)
            
        # Remove temporary LODES file
        os.remove(lodes_file)
        
        # Add Jobs Data to Census
        final_df = final_df.append(final_places, sort=False)
        final_df = final_df.append(final_county, sort=False)
        final_df = final_df.fillna(0)

print('Output final Census Data by Place to csv for further analysis')
final_df.loc[final_df['variable_name'] == 'median age', 'variable_name'] = 'age' 
final_df.loc[final_df['variable_name'] == 'median income', 'variable_name'] = 'household-income' 

final_df.to_csv(os.path.join(output_directory,"observed_data_for_rtp.csv"), index=False)

print ('All Done.')
