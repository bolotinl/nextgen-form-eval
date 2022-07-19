import os
import numpy as np
import pandas as pd

# define working dir
working_dir = '/home/ottersloth/cfe_calibration'

# ----------------------------------- Data Loading Dir ----------------------------------- #
# define basin list dir
basin_dir = '/home/ottersloth/data/camels_hourly'
basin_filename = 'basin_list_516.txt'
basin_file = os.path.join(basin_dir,basin_filename)

# define config dir
config_dir = os.path.join(working_dir,'configs')

# define observation file dir
#obs_dir = os.path.join(working_dir,'usgs-streamflow')
obs_dir = '/home/ottersloth/data/camels_hourly/usgs_streamflow'

# define atmospheric forcing file dir
forcing_path = '/home/ottersloth/data/camels_hourly/nldas_hourly'

# load basin list
with open(basin_file, "r") as f:
    basin_list = pd.read_csv(f, header=None)

nan_check = {}
nan_check['basin id'] = []
nan_check['Spinup - pet'] = []
nan_check['Spinup - precip'] = []
nan_check['Cal - pet'] = []
nan_check['Cal - precip'] = []
nan_check['Cal - usgs'] = []
nan_check['Val - pet'] = []
nan_check['Val - precip'] = []
nan_check['Val - usgs'] = []

# Loop through each basin
for i in range(basin_list.shape[0]): 
 
    g = basin_list[0][i]

    print(f"current basin:{g}.")

    # Load Observation file
    if int(g) > 10000000: obs_filename = str(g) + '-usgs-hourly.csv'
    else: obs_filename = '0' + str(g) + '-usgs-hourly.csv'
    obs_file = os.path.join(obs_dir,obs_filename)

    with open(obs_file) as f: 
        data = pd.read_csv(f)

    obs_data = data['QObs_CAMELS(mm/h)'].values
    eval_dates = data['date'].values

    # Load Forcing file
    if int(g) > 10000000: forcing_filename = str(g) + '_hourly_nldas.csv'
    else: forcing_filename = '0' + str(g) + '_hourly_nldas.csv'
    forcing_file = os.path.join(forcing_path,forcing_filename)
    
    with open(forcing_file) as f:
        forcing = pd.read_csv(forcing_file)

    # check spin-up forcing file
    spinup_start_idx_nldas = np.where(forcing['date']=='2001-10-01 00:00:00')
    spinup_end_idx_nldas = np.where(forcing['date']=='2002-09-30 23:00:00')

    if (spinup_start_idx_nldas[0].size == 0) or (spinup_end_idx_nldas[0].size == 0) : 
        print("none or missing forcing data for spinup period")
        continue

    forcing_spinup = forcing.iloc[spinup_start_idx_nldas[0][0]:spinup_end_idx_nldas[0][0]+1,:]

    pet_nan_spinup = np.where(np.isnan(forcing_spinup['potential_evaporation']))[0].shape
    precip_nan_spinup = np.where(np.isnan(forcing_spinup['total_precipitation']))[0].shape

    # check calibration period usgs obs and forcing
    cal_start_idx_usgs = np.where(data['date']=='2007-10-01 00:00:00')
    cal_end_idx_usgs = np.where(data['date']=='2013-09-30 23:00:00')
    
    if (cal_start_idx_usgs[0].size == 0) or (cal_end_idx_usgs[0].size == 0): 
        print("none or missing usgs streamflow data for the calibration period for this basin.") 
        continue
    
    obs_data_cal = obs_data[cal_start_idx_usgs[0][0]:cal_end_idx_usgs[0][0]+1]

    cal_start_idx_nldas = np.where(data['date']=='2007-10-01 00:00:00')
    cal_end_idx_nldas = np.where(data['date']=='2013-09-30 23:00:00')

    if (cal_start_idx_nldas[0].size == 0) or (cal_end_idx_nldas[0].size == 0): 
        print("none or missing forcing data for calibration period")
        continue

    forcing_cal = forcing[cal_start_idx_nldas[0][0]:cal_end_idx_nldas[0][0]+1]

    pet_nan_cal = np.where(np.isnan(forcing_cal['potential_evaporation']))[0].shape
    precip_nan_cal = np.where(np.isnan(forcing_cal['total_precipitation']))[0].shape
    usgs_nan_cal = np.where(np.isnan(obs_data_cal))[0].shape

    # check validation period usgs obs and forcing
    val_start_idx_usgs = np.where(data['date']=='2002-10-01 00:00:00')
    val_end_idx_usgs = np.where(data['date']=='2007-09-30 23:00:00')
    
    if (val_start_idx_usgs[0].size == 0) or (val_end_idx_usgs[0].size == 0): 
        print("none or missing usgs streamflow data for the validation period for this basin.") 
        continue

    obs_data_val = obs_data[val_start_idx_usgs[0][0]:val_end_idx_usgs[0][0]+1]

    val_start_idx_nldas = np.where(data['date']=='2002-10-01 00:00:00')
    val_end_idx_nldas = np.where(data['date']=='2007-09-30 23:00:00')

    if (val_start_idx_nldas[0].size == 0) or (val_end_idx_nldas[0].size == 0) : 
        print("none or missing forcing data for validation period")
        continue

    forcing_val = forcing[val_start_idx_nldas[0][0]:val_end_idx_nldas[0][0]+1]

    pet_nan_val = np.where(np.isnan(forcing_val['potential_evaporation']))[0].shape
    precip_nan_val = np.where(np.isnan(forcing_val['total_precipitation']))[0].shape
    usgs_nan_val = np.where(np.isnan(obs_data_val))[0].shape

    nan_check['basin id'].append(g)
    nan_check['Spinup - pet'].append(pet_nan_spinup[0])
    nan_check['Spinup - precip'].append(precip_nan_spinup[0])
    nan_check['Cal - pet'].append(pet_nan_cal[0])
    nan_check['Cal - precip'].append(precip_nan_cal[0])
    nan_check['Cal - usgs'].append(usgs_nan_cal[0])
    nan_check['Val - pet'].append(pet_nan_val[0])
    nan_check['Val - precip'].append(precip_nan_val[0])
    nan_check['Val - usgs'].append(usgs_nan_val[0])

df = pd.DataFrame(nan_check)
df.to_csv("check_for_nan_in_data.csv")