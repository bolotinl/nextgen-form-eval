library(mapview)
library(sf)
library(hydrofabric)
library(hyAggregate)
library(tidyverse)
library(glue)

VPU = "01"
ref_file = get_reference_fabric(VPU = VPU, type = "reference", dir = ".")
st_layers('reference_01.gpkg')

file = get_reference_fabric(VPU = VPU, type = "refactored", dir = ".")
st_layers('refactor_01.gpkg')

ngen_v1 =  aggregate_network_to_distribution(gf = 'refactor_01 (1).gpkg',
                                             outfile = glue("./01_ngen_{VPU}.gpkg"), 
                                             nexus_topology = FALSE) 
st_layers('refactor_01 (1).gpkg')



agg = read_sf('refactor_01 (1).gpkg', "refactored_flowpaths") 
plot(agg$geom)


ngen_v4 = aggregate_cfe_noahowp(gpkg = 'refactor_01 (1).gpkg', dir = 'data', add_to_gpkg = TRUE)
st_layers(ngen_v4)