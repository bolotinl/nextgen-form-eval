import pandas as pd
import numpy as np
import os
import sys
import json
import re 

#---------------------------- define directories ----------------------------#
# define working directory
working_dir = '/Users/Sophie/Desktop/CUAHSI/nextgen-form-eval/Calibration'

# define GIUH and soil params files
GIUH_dir = "/Users/Sophie/Desktop/CUAHSI/Data/CFE_Config_lumped"

# define basin list dir
basin_dir = '/Users/Sophie/Desktop/CUAHSI/Data'

# define camel dataset dir
camel_dir = '/Users/Sophie/Desktop/CUAHSI/Data/camels_attributes_v2.0'

# define atmospheric forcing file dir
        #forcing_path = os.path.join(working_dir,'nldas-forcing')
forcing_path = '/Users/Sophie/Desktop/CUAHSI/Data'

# define dir for exported json
config_dir = os.path.join(working_dir,'configs')
if os.path.exists(config_dir)==False: os.mkdir(config_dir)

#---------------------------- Basin Attributes ----------------------------#
# read in basin list
basin_filename = 'basin_list_516.txt'
basin_file = os.path.join(basin_dir,basin_filename)

with open(basin_file, "r") as f:
    basin_list = pd.read_csv(f, header=None)

# get basin attributes for each basin
basin_attributes = {}

for attribute_type in ['clim', 'geol', 'hydro', 'name', 'soil', 'topo', 'vege']:
    camel_filename = "camels_" + attribute_type + ".txt"
    camel_file = os.path.join(camel_dir,camel_filename)
    with open(camel_file, "r") as f:
        basin_attributes[attribute_type] = pd.read_csv(f, sep=";")
    basin_attributes[attribute_type] = basin_attributes[attribute_type].set_index("gauge_id")


#---------------------------- Generate Config Files ----------------------------#

for i in range(basin_list.shape[0]): 
    #if i == 1 : break              # run for the first basin in the list

    #if i != 130: continue          # run for a specific basin in the list

    g = basin_list[0][i]

    # get forcing file
    forcing_filename = "0" + str(g) + "_hourly_nldas.csv"
    forcing_file = os.path.join(forcing_path,forcing_filename)

    # get giuh and soil param file
    giuh_filename = "0" + str(g) + "_bmi_config_cfe_pass.txt"
    giuh_file = os.path.join(GIUH_dir,giuh_filename)

    with open(giuh_file, "r") as f:
        giuh_data_all = pd.read_fwf(f,header=None)

    giuh_data = giuh_data_all.iloc[2:21,:]
    giuh_data = pd.concat([giuh_data,giuh_data_all.iloc[24,:]],ignore_index=True)

    for i in range(giuh_data.shape[0]): 
        giuh_data[0][i] = giuh_data[0][i].split('=')
        parameter_values = [giuh_data[0][j][1] for j in range(giuh_data.shape[0])]
        parameter_names = [giuh_data[0][j][0] for j in range(giuh_data.shape[0])]
        parameter_values = [re.sub("\[.*?\]", "", parameter_values[i]) for i in range(len(parameter_values))]

    parameter_values[0:18] = np.array(parameter_values[0:18],dtype ="double")
    parameter_values[18] = np.array(parameter_values[18].split(','),dtype="double")
    parameter_values[19] = np.array(parameter_values[19].split(','),dtype="double")

    param_dict = {}
    param_dict['soil_params'] = {}
    for i in range(len(parameter_names)): 
        if i <= 8: 
            soil_param_name = parameter_names[i].split('.')
            param_dict[soil_param_name[0]][soil_param_name[1]] = parameter_values[i]
        else: param_dict[parameter_names[i]] = parameter_values[i]

    del param_dict['soil_params']['expon']
    del param_dict['soil_params']['expon_secondary']
    param_dict['soil_params']['bb'] = param_dict['soil_params'].pop("b")
    param_dict['soil_params']["D"] = 2.0
    param_dict["soil_params"]["mult"] = 1000.0

    # generate json text
    dict_json = {"forcing_file":forcing_file, 
                    "catchment_area_km2":basin_attributes['topo']['area_gages2'][g], 
                    "alpha_fc":param_dict["alpha_fc"], 
                    "soil_params": param_dict["soil_params"], 
                    "refkdt":param_dict["refkdt"],
                    "max_gw_storage":param_dict["max_gw_storage"],              # [calibrating parameter]
                    "Cgw":param_dict["Cgw"],                                    # [calibrating parameter]
                    "expon":param_dict["expon"],                                # [calibrating parameter]
                    "gw_storage":param_dict["gw_storage"],         
                    "soil_storage":param_dict["soil_storage"], 
                    "K_lf":param_dict["K_lf"],                                  # [calibrating parameter]
                    "K_nash":param_dict["K_nash"],                              # [calibrating parameter]
                    "nash_storage":param_dict["nash_storage"].tolist(), 
                    "giuh_ordinates":param_dict["giuh_ordinates"].tolist(), 
                    "stand_alone":1, 
                    "unit_test":0, 
                    "compare_results_file":"",
                    }

    # save and export json files
    json_filename = 'cat_' + str(g) + '_bmi_config_cfe.json'
    json_file = os.path.join(config_dir,json_filename)

    with open(json_file, 'w', encoding='utf-8') as f:
        json.dump(dict_json, f, ensure_ascii=False, indent=4, separators=(',', ':'))