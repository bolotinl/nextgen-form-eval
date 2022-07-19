## Call in packages ############################################################
library(tidyverse)
library(viridis)
library(viridisLite)
library(rnaturalearth)
library(rnaturalearthdata)
library(gridExtra)
library(rnaturalearthhires)
library(data.table)
library(ggpubr)
library(corrplot)
library(hrbrthemes)
library(reshape2)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/hydro_signatures")
list.files()

file.list.lstm <- list.files(path = getwd(), pattern = "fold")
sigs.lstm <- lapply(file.list.lstm, read.csv)
sigs.lstm <- rbindlist(sigs.lstm)
rm(file.list.lstm)

sigs.cfe <- read.csv("CFE.csv")

sigs.lstm <- sigs.lstm %>% filter(basin_id %in% sigs.cfe$basin_id)

sigs.lstm.sim <- sigs.lstm %>% 
  select(ends_with('qsim'))
colnames(sigs.lstm.sim) <- paste0(colnames(sigs.lstm.sim), "_lstm")

sigs.obs <- sigs.lstm %>% 
  select(ends_with('qobs'))

basin.list <- sigs.cfe$basin_id

colnames(sigs.cfe)[3:15] <- paste0(colnames(sigs.cfe[3:15]), "_cfe")

## Make one master data frame
signatures <- cbind(sigs.obs, sigs.lstm.sim)
signatures <- cbind(signatures, sigs.cfe)
signatures <- cbind(basin.list, signatures)
signatures <- signatures %>% select(-c(X))

rm(sigs.cfe, sigs.lstm, sigs.lstm.sim, sigs.obs, basin.list)

## Merge with location information
meta <- read.csv('../gauge_information.csv')
meta <- meta %>% select(c(GAGE_ID, LAT, LONG))

signatures <- merge(signatures, meta, by.x = "basin_id", by.y = "GAGE_ID")

## Merge with model performances
metrics <- read.csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics/model_performance_values.csv")
metrics <- metrics %>% select(-c(model))

signatures <- merge(signatures, metrics, by = 'basin_id')
names(signatures)

######################
# RUNOFF RATIO ####
######################

RR_lstm <- signatures %>% 
  select(c(RR_qobs, RR_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(RR_qsim = RR_qsim_lstm, NNSE = lstm_nnse)
RR_cfe <- signatures %>% 
  select(c(RR_qobs, RR_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(RR_qsim = RR_qsim_cfe, NNSE = cfe_nnse)
RR <- rbind(RR_lstm, RR_cfe)

RR_p <- ggplot()+
  geom_point(data = RR, mapping = aes(x = RR_qsim, y = RR_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  xlim(0,1)+
  ylim(0,1)+
  labs(x = 'Simulated', y = 'Observed', title = 'Runoff Ratio', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )

RR_p

######################
# BASEFLOW INDEX ####
######################

BFI_lstm <- signatures %>% 
  select(c(BFI_qobs, BFI_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(BFI_qsim = BFI_qsim_lstm, NNSE = lstm_nnse)
BFI_cfe <- signatures %>% 
  select(c(BFI_qobs, BFI_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(BFI_qsim = BFI_qsim_cfe, NNSE = cfe_nnse)
BFI <- rbind(BFI_lstm, BFI_cfe)

BFI_p <- ggplot()+
  geom_point(data = BFI, mapping = aes(x = BFI_qsim, y = BFI_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  xlim(0,1)+
  ylim(0,1)+
  labs(x = 'Simulated', y = 'Observed', title = 'Baseflow Index', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )


BFI_p

######################
# Mean Q ####
######################

Qmean_lstm <- signatures %>% 
  select(c(Qmean_qobs, Qmean_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(Qmean_qsim = Qmean_qsim_lstm, NNSE = lstm_nnse)
Qmean_cfe <- signatures %>% 
  select(c(Qmean_qobs, Qmean_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(Qmean_qsim = Qmean_qsim_cfe, NNSE = cfe_nnse)
Qmean <- rbind(Qmean_lstm, Qmean_cfe)

Qmean_p <- ggplot()+
  geom_point(data = Qmean, mapping = aes(x = Qmean_qsim, y = Qmean_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Mean Streamflow (mm/hr)', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0, 0.5)+
  ylim(0, 0.5)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )


Qmean_p


######################
# FDC Slope ####
######################

FDC_Slope_lstm <- signatures %>% 
  select(c(FDC_Slope_qobs, FDC_Slope_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(FDC_Slope_qsim = FDC_Slope_qsim_lstm, NNSE = lstm_nnse)
FDC_Slope_cfe <- signatures %>% 
  select(c(FDC_Slope_qobs, FDC_Slope_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(FDC_Slope_qsim = FDC_Slope_qsim_cfe, NNSE = cfe_nnse)
FDC_Slope <- rbind(FDC_Slope_lstm, FDC_Slope_cfe)

FDC_Slope_p <- ggplot()+
  geom_point(data = FDC_Slope, mapping = aes(x = FDC_Slope_qsim, y = FDC_Slope_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Slope of the Flow Duration Curve', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,55)+
  ylim(0,55)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )

FDC_Slope_p

######################
# 95th PERCENTILE FLOW ####
######################

Q95_lstm <- signatures %>% 
  select(c(Q95_qobs, Q95_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(Q95_qsim = Q95_qsim_lstm, NNSE = lstm_nnse)
Q95_cfe <- signatures %>% 
  select(c(Q95_qobs, Q95_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(Q95_qsim = Q95_qsim_cfe, NNSE = cfe_nnse)
Q95 <- rbind(Q95_lstm, Q95_cfe)

Q95_p <- ggplot()+
  geom_point(data = Q95, mapping = aes(x = Q95_qsim, y = Q95_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = '95th Percentile Flow (mm/hr)', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,2)+
  ylim(0,2)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )


Q95_p


######################
# 5TH PERCENTILE FLOW ####
######################

Q5_lstm <- signatures %>% 
  select(c(Q5_qobs, Q5_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(Q5_qsim = Q5_qsim_lstm, NNSE = lstm_nnse)
Q5_cfe <- signatures %>% 
  select(c(Q5_qobs, Q5_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(Q5_qsim = Q5_qsim_cfe, NNSE = cfe_nnse)
Q5 <- rbind(Q5_lstm, Q5_cfe)

Q5_p <- ggplot()+
  geom_point(data = Q5, mapping = aes(x = Q5_qsim, y = Q5_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = '5th Percentile Flow (mm/hr)', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(-0.05,0.15)+
  ylim(-0.05,0.15)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )


Q5_p


######################
# NO FLOW FREQUENCY ####
######################




######################
# MEAN HALF FLOW DATE ####
######################

HFD_Mean_lstm <- signatures %>% 
  select(c(HFD_Mean_qobs, HFD_Mean_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(HFD_Mean_qsim = HFD_Mean_qsim_lstm, NNSE = lstm_nnse)
HFD_Mean_cfe <- signatures %>% 
  select(c(HFD_Mean_qobs, HFD_Mean_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(HFD_Mean_qsim = HFD_Mean_qsim_cfe, NNSE = cfe_nnse)
HFD_Mean <- rbind(HFD_Mean_lstm, HFD_Mean_cfe)

HFD_Mean_p <- ggplot()+
  geom_point(data = HFD_Mean, mapping = aes(x = HFD_Mean_qsim, y = HFD_Mean_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Mean Half Flow Date (timestep)', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(0,5500)+
  ylim(0,5500)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
  )


HFD_Mean_p

######################
# STREAM ELASTICITY ####
######################

Stream_Elas_lstm <- signatures %>% 
  select(c(Stream_Elas_qobs, Stream_Elas_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(Stream_Elas_qsim = Stream_Elas_qsim_lstm, NNSE = lstm_nnse)
Stream_Elas_cfe <- signatures %>% 
  select(c(Stream_Elas_qobs, Stream_Elas_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(Stream_Elas_qsim = Stream_Elas_qsim_cfe, NNSE = cfe_nnse)
Stream_Elas <- rbind(Stream_Elas_lstm, Stream_Elas_cfe)

Stream_Elas_p <- ggplot()+
  geom_point(data = Stream_Elas, mapping = aes(x = Stream_Elas_qsim, y = Stream_Elas_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Stream Elasticity', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(-25,15)+
  ylim(-25,15)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'none'
        )


Stream_Elas_p


######################
# NO FLOW ####
######################

ZeroQ_Freq_lstm <- signatures %>% 
  select(c(ZeroQ_Freq_qobs, ZeroQ_Freq_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(ZeroQ_Freq_qsim = ZeroQ_Freq_qsim_lstm, NNSE = lstm_nnse)
ZeroQ_Freq_cfe <- signatures %>% 
  select(c(ZeroQ_Freq_qobs, ZeroQ_Freq_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(ZeroQ_Freq_qsim = ZeroQ_Freq_qsim_cfe, NNSE = cfe_nnse)
ZeroQ_Freq <- rbind(ZeroQ_Freq_lstm, ZeroQ_Freq_cfe)

ZeroQ_Freq_p <- ggplot()+
  geom_point(data = ZeroQ_Freq, mapping = aes(x = ZeroQ_Freq_qsim, y = ZeroQ_Freq_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Stream Elasticity', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(-25,15)+
  ylim(-25,15)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'bottom'
  )+
  guides(fill = guide_legend(override.aes = list(size=6)))



ZeroQ_Freq_p

######################
# NO FLOW ####
######################

LowQ_Freq_lstm <- signatures %>% 
  select(c(LowQ_Freq_qobs, LowQ_Freq_qsim_lstm, lstm_nnse)) %>% 
  mutate(model = 'LSTM') %>%
  rename(LowQ_Freq_qsim = LowQ_Freq_qsim_lstm, NNSE = lstm_nnse)
LowQ_Freq_cfe <- signatures %>% 
  select(c(LowQ_Freq_qobs, LowQ_Freq_qsim_cfe, cfe_nnse)) %>% 
  mutate(model = 'CFE') %>%
  rename(LowQ_Freq_qsim = LowQ_Freq_qsim_cfe, NNSE = cfe_nnse)
LowQ_Freq <- rbind(LowQ_Freq_lstm, LowQ_Freq_cfe)

LowQ_Freq_p <- ggplot()+
  geom_point(data = LowQ_Freq, mapping = aes(x = LowQ_Freq_qsim, y = LowQ_Freq_qobs, fill = model, size = NNSE), shape = 21, color = 'black', alpha = 0.6)+
  labs(x = 'Simulated', y = 'Observed', title = 'Stream Elasticity', fill = 'Model')+
  geom_abline(intercept = 0, slope = 1)+
  xlim(-25,15)+
  ylim(-25,15)+
  theme_bw()+
  labs(color = "Model")+
  scale_fill_manual(labels=c('CFE', 'LSTM'), values = c("red", "blue"))+
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.position = 'bottom'
  )+
  guides(fill = guide_legend(override.aes = list(size=6)))
LowQ_Freq_p


p_grid <- grid.arrange(ZeroQ_Freq_p, LowQ_Freq_p, BFI_p, RR_p, Q95_p, Q5_p, Qmean_p, FDC_Slope_p, HFD_Mean_p, Stream_Elas_p, nrow = 5)
p_grid
ggsave(plot = p_grid, filename = "signature_scatters.png", dpi = 400, device = NULL)
## Calculate the difference between simulated sigs for each model and observed sigs
signatures <- signatures %>% 
  mutate(
    BFI_diff_lstm = BFI_qobs - BFI_qsim_lstm,
    BFI_diff_cfe = BFI_qobs - BFI_qsim_cfe,
    RR_diff_lstm = RR_qobs - RR_qsim_lstm,
    RR_diff_cfe = RR_qobs - RR_qsim_cfe,
    FDC_Slope_diff_lstm = FDC_Slope_qobs - FDC_Slope_qsim_lstm,
    FDC_Slope_diff_cfe = FDC_Slope_qobs - FDC_Slope_qsim_cfe,
    Q95_diff_lstm = Q95_qobs - Q95_qsim_lstm,
    Q95_diff_cfe = Q95_qobs - Q95_qsim_cfe,
    Q5_diff_lstm = Q5_qobs - Q5_qsim_lstm,
    Q5_diff_cfe = Q5_qobs - Q5_qsim_cfe,
    Qmean_diff_lstm = Qmean_qobs - Qmean_qsim_lstm,
    Qmean_diff_cfe = Qmean_qobs - Qmean_qsim_cfe,
    ZeroQ_Freq_diff_lstm = ZeroQ_Freq_qobs - ZeroQ_Freq_qsim_lstm,
    ZeroQ_Freq_diff_cfe = ZeroQ_Freq_qobs - ZeroQ_Freq_qsim_cfe,
    Stream_Elas_diff_lstm = Stream_Elas_qobs - Stream_Elas_qsim_lstm,
    Stream_Elas_diff_cfe = Stream_Elas_qobs - Stream_Elas_qsim_cfe,
    HFD_Mean_diff_lstm = HFD_Mean_qobs - HFD_Mean_qsim_lstm,
    HFD_Mean_diff_cfe = HFD_Mean_qobs - HFD_Mean_qsim_cfe,
    HighQ_Freq_diff_lstm = HighQ_Freq_qobs - HighQ_Freq_qsim_lstm,
    HighQ_Freq_diff_cfe = HighQ_Freq_qobs - HighQ_Freq_qsim_cfe,
    HighQ_Dur_diff_lstm = HighQ_Dur_qobs - HighQ_Dur_qsim_lstm,
    HighQ_Dur_diff_cfe = HighQ_Dur_qobs - HighQ_Dur_qsim_cfe,
    LowQ_Freq_diff_lstm = LowQ_Freq_qobs - LowQ_Freq_qsim_lstm,
    LowQ_Freq_diff_cfe = LowQ_Freq_qobs - LowQ_Freq_qsim_cfe,
    LowQ_Dur_diff_lstm = LowQ_Dur_qobs - LowQ_Dur_qsim_lstm,
    LowQ_Dur_diff_cfe = LowQ_Dur_qobs - LowQ_Dur_qsim_cfe
  )

signatures_diff <- signatures %>% select(c(basin_id, contains("diff"), LAT, LONG))

# Z-normalize all differences
# New value = (x – μ) / σ
# 
# x: Original value
# μ: Mean of data
# σ: Standard deviation of data

# signatures_diff_zscore <- sapply(signatures_diff, function(signatures_diff) (signatures_diff-mean(signatures_diff))/sd(signatures_diff))
# signatures_diff_zscore <- signatures_diff_zscore %>% as.data.frame()
# 
# signatures_diff_zscore$basin_id <- signatures_diff$basin_id
# signatures_diff_zscore$LAT <- signatures_diff$LAT
# signatures_diff_zscore$LONG <- signatures_diff$LONG
# 
# signatures_diff_log <- signatures_diff
# signatures_diff_log[2:27] <- log(signatures_diff_log[2:27])

## PICK UP HERE !!!!!!
# signatures_diff_norm <- signatures_diff
# run.list <- seq(2, 27)

# normalize_sig <- function(x){
#   x <- (2((x-min(x))/(max(x)-min(x)))-1)
#   signatures_diff_norm[x] <<- y 
# }
# 
# signatures_diff_norm$basin_id <- signatures_diff$basin_id
# signatures_diff_norm$LAT <- signatures_diff$LAT
# signatures_diff_norm$LONG <- signatures_diff$LONG


############################################################
## MAKE MAPS ###############################################
############################################################

world <- ne_countries(scale = "medium", returnclass = "sf")
states <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")

rng = range(c((-1), (1))) #a range to have the same min and max for both plots
mid = 0

######################
# BASEFLOW INDEX ####
######################
ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff, mapping = aes(x = LONG, y = LAT, color = BFI_diff_lstm))+
  labs(color = "Difference in BFI", x = "", y = "", title = "LSTM")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red", limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff, mapping = aes(x = LONG, y = LAT, color = BFI_diff_cfe))+
  labs(color = "Difference in BFI", x = "", y = "", title = "CFE")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red", limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())


######################
# RUNOFF RATIO ####
######################
ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = RR_diff_lstm))+
  labs(color = "Difference in RR", x = "", y = "", title = "LSTM")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = RR_diff_cfe))+
  labs(color = "Difference in RR", x = "", y = "", title = "CFE")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red", limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())


######################
# FDC SLOPE ####
######################
ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = FDC_Slope_diff_lstm))+
  labs(color = "Difference in FDC Slope", x = "", y = "", title = "LSTM")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = FDC_Slope_diff_cfe))+
  labs(color = "Difference in FDC Slope", x = "", y = "", title = "CFE")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

######################
# RUNOFF RATIO ####
######################
ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = RR_diff_lstm))+
  labs(color = "Difference in RR", x = "", y = "", title = "LSTM")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = RR_diff_cfe))+
  labs(color = "Difference in RR", x = "", y = "", title = "CFE")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red", limits=c(floor(rng[1]), ceiling(rng[2])))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())


######################
# Q MEAN ####
######################
ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = Qmean_diff_lstm))+
  labs(color = "Difference in Q Mean", x = "", y = "", title = "LSTM")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = signatures_diff_norm, mapping = aes(x = LONG, y = LAT, color = Qmean_diff_cfe))+
  labs(color = "Difference in Q Mean", x = "", y = "", title = "CFE")+
  scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                        high = "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())
