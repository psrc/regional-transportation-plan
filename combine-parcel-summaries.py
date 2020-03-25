import pandas as pd
import os

summary_years = [2014,2025,2040,2050]

working_path = os.getcwd()
combined_results = pd.DataFrame()

for years in summary_years:
    print('Opening the '+str(years)+' parcel results')

    results_file = os.path.join(working_path,'trips_model_output_summary_data_'+str(years)+'.csv')
    results = pd.read_csv(results_file, low_memory=False) 
    
    if combined_results.size == 0:
        combined_results = results
            
    else:
        combined_results = combined_results.append(results, ignore_index=True)    

demographic_file = os.path.join(working_path,'demographic_model_output_summary_data_all_years.csv')
results = pd.read_csv(demographic_file, low_memory=False)
combined_results = combined_results.append(results, sort=False)  

combined_results.to_csv(os.path.join(working_path,'community-profiles','combined_model_output_summary_data.csv'),index=False)    