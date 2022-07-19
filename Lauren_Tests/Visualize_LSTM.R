## Call in packages
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

## Bring in LSTM performances
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")
lstm <- read.csv("LSTM_test_metrics_all.csv")

summary(lstm)

boxplot(lstm$NNSE)

## Plot performances
  # Extremely skewed:
lstm %>%
  ggplot(aes(x=NNSE)) +
  geom_density(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "NNSE", title = "LSTM")+
  theme_bw()

## Map performances
meta <- read.csv("/Volumes/GoogleDrive/My Drive/Overland Flow MS/Data/CAMELS_US/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/basin_metadata/gauge_information.csv")
dat <- merge(lstm, meta, by.x = "basin", by.y = "GAGE_ID")

# Should no longer be necessary with NNSE:
# dat <- dat %>%
#   mutate(lstm_recode = ifelse(NSE < -1, -1, NSE))

dat <- dat[complete.cases(dat),]


world <- ne_countries(scale = "medium", returnclass = "sf")
states <- rnaturalearth::ne_states(country = "united states of america", returnclass = "sf")

# ggplot(data = states)+
#   geom_sf(fill = "white") +
#   coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
#   geom_point(data = dat, mapping = aes(x = LONG, y = LAT, color = lstm_recode))+
#   labs(color = "NSE")+
#   scale_colour_gradientn(colours = c("red", "white", "blue"),
#                       limits = c(-1, 1),
#                       guide = guide_coloursteps(even.steps = FALSE,
#                                                 show.limits = TRUE),
#                       breaks = c(-1, 0, 1))+
#   theme_bw() 

map_lstm <- ggplot(data = states)+
  geom_sf(fill = "white", size = 0.25, color = "black") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = dat, mapping = aes(x = LONG, y = LAT, color = NNSE))+
  labs(color = "Normalized NSE", x = "", y = "", title = "Hourly LSTM Performance on CAMELS")+
  scale_color_viridis(option = "D")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank())

ggsave(plot = map_lstm, filename = "map_lstm.png", dpi = 300, device = NULL)

## Plot CDF -----
# Bring in cfe: 
cfe <- read.csv("/Users/laurenbolotin/Downloads/models_performance.csv")

names(cfe)
cfe <- cfe %>% select(basin_id, cfe_nnse)

dat <- merge(dat, cfe, by.x = "basin", by.y = "basin_id")

dat$cfe_nnse[dat$basin == '6879650'] <- 0.5000012797089692

NNSE_df <- dat %>% select(c(NNSE, cfe_nnse))

NNSE_df <- melt(NNSE_df)
NNSE_df_boxplot <- NNSE_df
NNSE_df_boxplot$variable <- recode(NNSE_df_boxplot$variable, NNSE = "LSTM")
NNSE_df_boxplot$variable <- recode(NNSE_df_boxplot$variable, cfe_nnse = "CFE")

boxplot <- ggplot(NNSE_df_boxplot, aes(x=variable, y=value, fill = variable)) + 
  geom_boxplot(alpha=0.8)+
  stat_summary(fun=mean, geom="point", shape=18, size=8, color="black", fill="black") +
  labs(y = "NNSE", 
       x = 'Model', 
       title = 'Box Plot of Model Performance')+
  theme_bw()+
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.5, size = 20), 
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    panel.grid = element_blank())+
  scale_fill_manual(labels=c('LSTM', 'CFE'), values = c("blue", "red"))
ggsave(plot = boxplot, filename = "boxplot.png", dpi = 400, device = NULL,
       units = c('in'), height = 6, width = 5)

ggplot() +
  stat_ecdf(data = dat, mapping = aes(x = NNSE, color = "LSTM"), geom = "step",color = "darkorange")+
  stat_ecdf(data = dat, mapping = aes(x = NNSE_cfe, color = "CFE"), geom = "step",color = "deepskyblue4")+
  labs(x = "Normalized NSE", y = "CDF", title = "CDF of Model Performance", color = "Model")+
  scale_color_manual(name = "Model", values = c(LSTM = "darkorange", CFE = "deepskyblue4"))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

cdf_plot <- ggplot() +
  stat_ecdf(data = NNSE_df, mapping = aes(x = value, color = variable), geom = "step", size = 1.5)+
  scale_color_manual(name = "Model")+
  labs(x = "Normalized NSE", y = "CDF", title = "CDF of Model Performance", color = "Model")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 20), 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        panel.grid = element_blank())+
  theme(legend.position = c(0.15, 0.88), legend.background = element_blank())+
  scale_color_manual(labels=c('LSTM', 'CFE'), values = c("blue", "red"))

ggsave(plot = cdf_plot, filename = "cdf_comparison.png", 
       dpi = 400, device = NULL, units = c('in'), height = 6, width = 5)


















## Plot by important attributes:
attr <- read.csv("../CAMELS_Attributes/CAMELS_attributes_all.csv")
dat <- merge(dat, attr, by.x = "basin", by.y = "gauge_id")
lstm_attr <- merge(lstm, attr, by.x = "basin", by.y = "gauge_id")
rm(attr)

  # Snow
ggscatter(data = dat, 
          x = "frac_snow", 
          y = "NSE", 
          add = c("reg.line"),
          conf.int = TRUE,
          conf.int.level = 0.95,
          cor.coef = TRUE,
          cor.method = "spearman")+
  ylim(-1,1)+
  theme_bw()

  # Aridity
ggscatter(data = dat, 
          x = "aridity", 
          y = "NSE", 
          add = c("reg.line"),
          conf.int = TRUE,
          conf.int.level = 0.95,
          cor.coef = TRUE,
          cor.method = "spearman")+
  ylim(-1,1)+
  theme_bw()

## Set up correlation matrix using the adjusted NSEs
lstm_attr <- lstm_attr %>%
  select(-c(basin, X))

lstm_attr <- select_if(lstm_attr, is.numeric)

sapply(lstm_attr, class)
cor.df <- cor(lstm_attr, method = "spearman", use = "complete.obs")
corrplot(cor.df, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 90, tl.cex = 0.5)

## Can't get color legend when I only look at one column
corrplot(cor.df[,1:53][1:53,1,drop = FALSE], cl.pos = 'n',  tl.col = "black", tl.srt = 90, tl.cex = 0.6)
corrplot(cor.df[1:53,][1,1:53,drop = FALSE], cl.pos = 'r',  tl.col = "black", tl.srt = 90)


setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/data_for_model_selection")
file.list <- list.files(getwd(), pattern = "_NSE.csv")
dfs <- lapply(file.list, read.csv)

daymet_1 <- dfs[[1]]
daymet_2 <- dfs[[2]]
nldas_1 <- dfs[[3]]
nldas_2 <- dfs[[4]]

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")
file.list <- list.files(path = getwd(), pattern = ".csv")
lstm <- lapply(file.list, read.csv)
lstm <- rbindlist(lstm)

rm(dfs, dfs_long, file.list, file.list.sub)

names(lstm)
colnames(lstm) <- c("basin", "NSE_hourly")

names(nldas_1)
nldas_1 <- nldas_1 %>%
  select(c(X, lstm)) %>%
  rename(basin = X, NSE_daily = lstm)

nldas_1 <- merge(nldas_1, lstm, by = "basin")
nldas_1$diff <- nldas_1$NSE_hourly-nldas_1$NSE_daily
nldas_1 <- merge(nldas_1, meta, by.x = "basin", by.y = "GAGE_ID")
nldas_1 <- nldas_1[complete.cases(nldas_1),]


n1 <- ggplot(data = states)+
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = nldas_1, mapping = aes(x = LONG, y = LAT, color = diff))+
  labs(color = "NSE hourly - NSE daily")+
  scale_colour_gradientn(colours = c("red", "white", "blue"),
                         limits = c(-1, 1),
                         guide = guide_coloursteps(even.steps = FALSE,
                                                   show.limits = TRUE),
                         breaks = c(-1, 0, 1))+
  theme_bw()

# ------------

names(nldas_2)
nldas_2 <- nldas_2 %>%
  select(c(X, lstm)) %>%
  rename(basin = X, NSE_daily = lstm)

nldas_2 <- merge(nldas_2, lstm, by = "basin")
nldas_2$diff <- nldas_2$NSE_hourly-nldas_2$NSE_daily
nldas_2 <- merge(nldas_2, meta, by.x = "basin", by.y = "GAGE_ID")
nldas_2 <- nldas_2[complete.cases(nldas_2),]


n2 <- ggplot(data = states)+
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = nldas_2, mapping = aes(x = LONG, y = LAT, color = diff))+
  labs(color = "NSE hourly - NSE daily")+
  scale_colour_gradientn(colours = c("red", "white", "blue"),
                         limits = c(-1, 1),
                         guide = guide_coloursteps(even.steps = FALSE,
                                                   show.limits = TRUE),
                         breaks = c(-1, 0, 1))+
  theme_bw()


# -----------
names(daymet_1)
daymet_1 <- daymet_1 %>%
  select(c(X, lstm)) %>%
  rename(basin = X, NSE_daily = lstm)

daymet_1 <- merge(daymet_1, lstm, by = "basin")
daymet_1$diff <- daymet_1$NSE_hourly-daymet_1$NSE_daily
daymet_1 <- merge(daymet_1, meta, by.x = "basin", by.y = "GAGE_ID")
daymet_1 <- daymet_1[complete.cases(daymet_1),]


d1 <- ggplot(data = states)+
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = daymet_1, mapping = aes(x = LONG, y = LAT, color = diff))+
  labs(color = "NSE hourly - NSE daily")+
  scale_colour_gradientn(colours = c("red", "white", "blue"),
                         limits = c(-1, 1),
                         guide = guide_coloursteps(even.steps = FALSE,
                                                   show.limits = TRUE),
                         breaks = c(-1, 0, 1))+
  theme_bw() 

# --------
names(daymet_2)
daymet_2 <- daymet_2 %>%
  select(c(X, lstm)) %>%
  rename(basin = X, NSE_daily = lstm)

daymet_2 <- merge(daymet_2, lstm, by = "basin")
daymet_2$diff <- daymet_2$NSE_hourly-daymet_2$NSE_daily
daymet_2 <- merge(daymet_2, meta, by.x = "basin", by.y = "GAGE_ID")
daymet_2 <- daymet_2[complete.cases(daymet_2),]


d2 <- ggplot(data = states)+
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-128, -65), ylim = c(24, 50), expand = FALSE)+
  geom_point(data = daymet_2, mapping = aes(x = LONG, y = LAT, color = diff))+
  labs(color = "NSE hourly - NSE daily")+
  scale_colour_gradientn(colours = c("red", "white", "blue"),
                         limits = c(-1, 1),
                         guide = guide_coloursteps(even.steps = FALSE,
                                                   show.limits = TRUE),
                         breaks = c(-1, 0, 1))+
  theme_bw()


p_grid <- grid.arrange(n1, n2, d1, d2, nrow = 2)

