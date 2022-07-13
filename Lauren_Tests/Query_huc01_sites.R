library(dataRetrieval)
library(tidyverse)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/Hydrofabric_Case_Study")

sites <- whatNWISsites(huc = "01", 
                       parameterCd=c("00060"),
                       hasDataTypeCd="uv")

sites <- sites %>%
  filter(site_tp_cd == "ST")

write.csv(sites, "site_query_huc01.csv")
