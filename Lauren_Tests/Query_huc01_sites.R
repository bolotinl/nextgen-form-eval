library(dataRetrieval)
library(tidyverse)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/Hydrofabric_Case_Study")

sites <- whatNWISsites(huc = "01", 
                       parameterCd=c("00060"),
                       hasDataTypeCd="uv")

sites <- sites %>%
  filter(site_tp_cd == "ST")

write.csv(sites, "site_query_huc01.csv")

# --------
# Plug into QGIS and determine which ones are nested inside CAMELS basins
#---------

# Results: 
siteNumber <- c(# "01030350", # missing a ton
                # "01022330", # missing a lot
                # "01022294" # pretty good
                # "01022295" # pretty good
                # "01031300" # just okay, has some random gaps
                # "01031450" # similar to previous
                #"01138500" # pretty good
                ) 
parameterCd <- "00060"  # Discharge
startDate <- "2002-01-10" 
endDate <- "2013-09-30" 
dischargeUnit <- readNWISuv(siteNumber, parameterCd, 
                            startDate, endDate)


ggplot()+
  geom_line(data = dischargeUnit, 
            mapping = aes(x = dateTime, y = X_00060_00000, color = site_no))+
  theme_bw()

# -------
# HUC15 (Lower Colorado)
sites <- whatNWISsites(huc = "15", 
                       parameterCd=c("00060"),
                       hasDataTypeCd="uv")

sites <- sites %>%
  filter(site_tp_cd == "ST")

write.csv(sites, "site_query_huc15.csv")

# 09484550, 09484580, 09430030, 09430020, 09430010, 09497700

# -------
# HUC04 (Great Lakes)
sites <- whatNWISsites(huc = "04", 
                       parameterCd=c("00060"),
                       hasDataTypeCd="uv")

sites <- sites %>%
  filter(site_tp_cd == "ST")

write.csv(sites, "site_query_huc04.csv")

# 