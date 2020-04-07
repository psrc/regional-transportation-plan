import pandas as pd
import os

data_directory = os.path.join(os.getcwd(),'output')

observed_file = os.path.join(data_directory,'observed_data_for_rtp.csv')
demographic_file = os.path.join(data_directory,'demographic_model_data_for_rtp.csv')
tour_file = os.path.join(data_directory,'tour_model_data_for_rtp.csv')

all_results = [observed_file, demographic_file, tour_file]

combined_results = pd.DataFrame()

for results in all_results:
    interim = pd.read_csv(results)
    
    if combined_results.empty:
        combined_results = interim
        
    else:
        combined_results = combined_results.append(interim, sort=False)
        
combined_results.to_csv(os.path.join(data_directory,'combined_output_summary_data.csv'),index=False)    