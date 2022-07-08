library(tidyverse)
library(data.table)
library(gridExtra)
library(pscl)

# Data Prep -------------------------------------------------------
## Set working directory to where the model performances are, bring in data
setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/data_for_model_selection")
file.list <- list.files(getwd(), pattern = ".csv")
dfs <- lapply(file.list, read.csv, header = TRUE)

dfs_long <- rbindlist(dfs)
dfs_long$diff <- dfs_long$lstm-dfs_long$sac
dfs_long <- dfs_long %>% filter(diff == 0)

## Write function to reduce to only 2 models
remove_mc <- function(x){
dfs[[x]] <<- dfs[[x]] %>%
  select(-c(mc))
}

run.list <- seq(1, 44)
lapply(run.list, remove_mc)

## Write function to remove cases where performance is equal for both models
remove_equal <- function(x){
  dfs[[x]] <<- dfs[[x]] %>%
    filter(lstm-sac != 0)
}

lapply(run.list, remove_equal)

## Write function to convert to binary where: 
## the performance metric of LSTM > SAC yields a 0 and the opposite yields a 1

make_binary <- function(x){
dfs[[x]]$model <<- ifelse(dfs[[x]]$lstm > dfs[[x]]$sac, 0, 1)
}

lapply(run.list, make_binary)
 
## Clean up environment
rm(dfs_long, file.list, make_binary, remove_equal, remove_mc)

## Bring in and pair catchment attributes
clim <- read.csv("../CAMELS_Attributes/camels_clim.txt", header = TRUE, sep = ";")
geol <- read.csv("../CAMELS_Attributes/camels_geol.txt", header = TRUE, sep = ";")
hydro <- read.csv("../CAMELS_Attributes/camels_hydro.txt", header = TRUE, sep = ";")
soil <- read.csv("../CAMELS_Attributes/camels_soil.txt", header = TRUE, sep = ";")
vege <- read.csv("../CAMELS_Attributes/camels_vege.txt", header = TRUE, sep = ";")
topo <- read.csv("../CAMELS_Attributes/camels_topo.txt", header = TRUE, sep = ";")

attr <- merge(clim, geol, by = "gauge_id")
attr <- merge(attr, hydro, by = "gauge_id")
attr <- merge(attr, soil, by = "gauge_id")
attr <- merge(attr, vege, by = "gauge_id")
attr <- merge(attr, topo, by = "gauge_id")

rm(clim, geol, soil, hydro, vege, topo)

attr <- attr %>% select(c(gauge_id, organic_frac, elev_mean, slope_mean, area_gages2, soil_depth_pelletier,
                          sand_frac, silt_frac, clay_frac, geol_permeability, p_mean,
                          pet_mean, aridity, frac_snow, high_prec_freq, high_prec_dur,
                          low_prec_freq, low_prec_dur))

# Run logistic regression ---------------------------------------------------
## Create one df per data source ----
d_aNSE_1 <- dfs[[1]]
d_KGE_1 <- dfs[[6]]
d_aNSE_2 <- dfs[[12]]
d_KGE_2 <- dfs[[17]]
n_aNSE_1 <- dfs[[23]]
n_KGE_1 <- dfs[[28]]
n_aNSE_2 <- dfs[[34]]
n_KGE_2 <- dfs[[39]]

d_aNSE_1 <- merge(d_aNSE_1, attr, by.x = "X", by.y = "gauge_id")
d_KGE_1 <- merge(d_KGE_1, attr, by.x = "X", by.y = "gauge_id")
d_aNSE_2 <- merge(d_aNSE_2, attr, by.x = "X", by.y = "gauge_id")
d_KGE_2 <- merge(d_KGE_2, attr, by.x = "X", by.y = "gauge_id")
n_aNSE_1 <- merge(n_aNSE_1, attr, by.x = "X", by.y = "gauge_id")
n_KGE_1 <- merge(n_KGE_1, attr, by.x = "X", by.y = "gauge_id")
n_aNSE_2 <- merge(n_aNSE_2, attr, by.x = "X", by.y = "gauge_id")
n_KGE_2 <- merge(n_KGE_2, attr, by.x = "X", by.y = "gauge_id")

d_aNSE_1_split <- sample(c(rep(0, 0.8 * nrow(d_aNSE_1)), rep(1, 0.2 * nrow(d_aNSE_1))))
d_aNSE_1_train <- d_aNSE_1[d_aNSE_1_split == 0, ]   
d_aNSE_1_test <- d_aNSE_1[d_aNSE_1_split == 1, ]  

d_aNSE_2_split <- sample(c(rep(0, 0.8 * nrow(d_aNSE_2)), rep(1, 0.2 * nrow(d_aNSE_2))))
d_aNSE_2_train <- d_aNSE_2[d_aNSE_2_split == 0, ]   
d_aNSE_2_test <- d_aNSE_2[d_aNSE_2_split == 1, ]   

d_KGE_1_split <- sample(c(rep(0, 0.8 * nrow(d_KGE_1)), rep(1, 0.2 * nrow(d_KGE_1))))
d_KGE_1_train <- d_KGE_1[d_KGE_1_split == 0, ]   
d_KGE_1_test <- d_KGE_1[d_KGE_1_split == 1, ]   

d_KGE_2_split <- sample(c(rep(0, 0.8 * nrow(d_KGE_2)), rep(1, 0.2 * nrow(d_KGE_2))))
d_KGE_2_train <- d_KGE_2[d_KGE_2_split == 0, ]   
d_KGE_2_test <- d_KGE_2[d_KGE_2_split == 1, ]   

n_aNSE_1_split <- sample(c(rep(0, 0.8 * nrow(n_aNSE_1)), rep(1, 0.2 * nrow(n_aNSE_1))))
n_aNSE_1_train <- n_aNSE_1[n_aNSE_1_split == 0, ]   
n_aNSE_1_test <- n_aNSE_1[n_aNSE_1_split == 1, ]  

n_aNSE_2_split <- sample(c(rep(0, 0.8 * nrow(n_aNSE_2)), rep(1, 0.2 * nrow(n_aNSE_2))))
n_aNSE_2_train <- n_aNSE_2[n_aNSE_2_split == 0, ]   
n_aNSE_2_test <- n_aNSE_2[n_aNSE_2_split == 1, ]   

n_KGE_1_split <- sample(c(rep(0, 0.8 * nrow(n_KGE_1)), rep(1, 0.2 * nrow(n_KGE_1))))
n_KGE_1_train <- n_KGE_1[n_KGE_1_split == 0, ]   
n_KGE_1_test <- n_KGE_1[n_KGE_1_split == 1, ]   

n_KGE_2_split <- sample(c(rep(0, 0.8 * nrow(n_KGE_2)), rep(1, 0.2 * nrow(n_KGE_2))))
n_KGE_2_train <- n_KGE_2[n_KGE_2_split == 0, ]   
n_KGE_2_test <- n_KGE_2[n_KGE_2_split == 1, ]   

rm(list = ls(pattern = "split"))
#----
## Set ggplot theme
theme_set(theme_bw())

## Run Models ----
## Train logistic regression model
fit.full.n_KGE_1 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                low_prec_freq+low_prec_dur, data = n_KGE_1_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
n_KGE_1_test$prob <- predict(fit.full.n_KGE_1, newdata=n_KGE_1_test,
                         type="response")
pR2(fit.full.n_KGE_1)[4]
varImp(fit.full.n_KGE_1)

accuracy <- table(n_KGE_1_test$prob)
sum(diag(accuracy))/sum(accuracy)
## [1] 1

## Train logistic regression model
fit.full.n_KGE_2 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                          sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                          pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                          low_prec_freq+low_prec_dur, data = n_KGE_2_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
n_KGE_2_test$prob <- predict(fit.full.n_KGE_2, newdata=n_KGE_2_test,
                             type="response")
pR2(fit.full.n_KGE_2)
pR2(fit.full.n_KGE_2)[4]
accuracy <- table(n_KGE_2_test$prob)
sum(diag(accuracy))/sum(accuracy)
## Train logistic regression model
fit.full.n_aNSE_1 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                          sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                          pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                          low_prec_freq+low_prec_dur, data = n_aNSE_1_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
n_aNSE_1_test$prob <- predict(fit.full.n_aNSE_1, newdata=n_aNSE_1_test,
                             type="response")
pR2(fit.full.n_aNSE_1)
pR2(fit.full.n_aNSE_1)[4]
## Train logistic regression model
fit.full.n_aNSE_2 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                          sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                          pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                          low_prec_freq+low_prec_dur, data = n_aNSE_2_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]
pR2(fit.full.n_aNSE_2)
pR2(fit.full.n_aNSE_2)[4]

## Do prediction on new data
n_aNSE_2_test$prob <- predict(fit.full.n_aNSE_2, newdata=n_aNSE_2_test,
                             type="response")


## Train logistic regression model
fit.full.d_KGE_1 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                          sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                          pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                          low_prec_freq+low_prec_dur, data = d_KGE_1_train, family = "binomial")
pR2(fit.full.d_KGE_1)
pR2(fit.full.d_KGE_1)[4]

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
d_KGE_1_test$prob <- predict(fit.full.d_KGE_1, newdata=d_KGE_1_test,
                             type="response")

## Train logistic regression model
fit.full.d_KGE_2 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                          sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                          pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                          low_prec_freq+low_prec_dur, data = d_KGE_2_train, family = "binomial")
pR2(fit.full.d_KGE_2)
pR2(fit.full.d_KGE_2)[4]

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
d_KGE_2_test$prob <- predict(fit.full.d_KGE_2, newdata=d_KGE_2_test,
                             type="response")

## Train logistic regression model
fit.full.d_aNSE_1 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                           sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                           pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                           low_prec_freq+low_prec_dur, data = d_aNSE_1_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
d_aNSE_1_test$prob <- predict(fit.full.d_aNSE_1, newdata=d_aNSE_1_test,
                              type="response")
pR2(fit.full.d_aNSE_1)
pR2(fit.full.d_aNSE_1)[4] # OKAY

## Train logistic regression model
fit.full.d_aNSE_2 <- glm(model ~ organic_frac+elev_mean+slope_mean+area_gages2+soil_depth_pelletier+
                           sand_frac+silt_frac+clay_frac+geol_permeability+p_mean+
                           pet_mean+aridity+frac_snow+high_prec_freq+high_prec_dur+
                           low_prec_freq+low_prec_dur, data = d_aNSE_2_train, family = "binomial")

# summary_lr <- summary(fit.full)
# summary_lr <- summary_lr[["coefficients"]]

## Do prediction on new data
d_aNSE_2_test$prob <- predict(fit.full.d_aNSE_2, newdata=d_aNSE_2_test,
                              type="response")
pR2(fit.full.d_aNSE_2)
pR2(fit.full.d_aNSE_2)[4] # OKAY

#----

## Plot distribution of prediction ----
p1 <- n_KGE_1_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "NLDAS KGE 1")+
  ylim(0,9)

p2 <- n_KGE_2_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "NLDAS KGE 2")+
  ylim(0,9)

p3 <- n_aNSE_1_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "NLDAS Alpha-NSE 1")+
  ylim(0,9)

p4 <- n_aNSE_2_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="#69b3a2", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "NLDAS Alpha-NSE 2")+
  ylim(0,9)

p5 <- d_KGE_1_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "Daymet KGE 1")+
  ylim(0,9)

p6 <- d_KGE_2_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "Daymet KGE 2")+
  ylim(0,9)

p7 <- d_aNSE_1_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "Daymet Alpha-NSE 1")+
  ylim(0,9)

p8 <- d_aNSE_2_test %>%
  ggplot(aes(x=prob)) +
  geom_density(fill="palegreen4", color="black", alpha=0.8)+
  labs(x = "Probability SAC-SMA > LSTM", y = "Density", title = "Daymet Alpha-NSE 2")+
  ylim(0,9)

p_grid <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 4)
ggsave("LR_pgrid.png", plot = p_grid, dpi = 300)
#----



# It generally seems that area, clay frac, p_mean, and low_prec dur and freq are important

## Run a simpler version of the model using only the significant predictors
# fit.reduced <- glm(model ~ area_gages2+clay_frac, data = d_aNSE_1, family = "binomial")
# summary(fit.reduced)

