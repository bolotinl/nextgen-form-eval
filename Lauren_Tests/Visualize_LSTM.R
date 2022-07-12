library(tidyverse)
library(viridis)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(data.table)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")
file.list <- list.files(path = getwd(), pattern = ".csv")
lstm <- lapply(file.list, read.csv)
lstm <- rbindlist(lstm)

summary(lstm)

lstm %>%
  ggplot(aes(x=basin, y = NSE)) +
  geom_col(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "Basin", y = "NSE", title = "LSTM")+
  theme_bw()

meta <- read.csv("/Volumes/GoogleDrive/My Drive/Overland Flow MS/Data/CAMELS_US/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_metadata/gauge_information.csv")
lstm <- merge(lstm, meta, by.x = "basin", by.y = "GAGE_ID")
lstm <- lstm %>%
  mutate(lstm_recode = ifelse(NSE < -1, -1, NSE))


world <- ne_countries(scale = "medium", returnclass = "sf")
states <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")

ggplot(data = states)+
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = lstm, mapping = aes(x = LONG, y = LAT, color = NSE))+
  scale_colour_gradientn(colours = c("red", "white", "blue"),
                      limits = c(-1, 1),
                      guide = guide_coloursteps(even.steps = FALSE,
                                                show.limits = TRUE),
                      breaks = c(-1, 0, 1))+
  theme_bw() 


