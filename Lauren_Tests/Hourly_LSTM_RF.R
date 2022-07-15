library(randomForest)
library(ggpubr)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")
lstm <- read.csv("LSTM_test_metrics_all.csv")

train <- lstm %>% filter(fold == "fold_4")
test <- lstm[!(lstm$fold %in% train$fold),]

train <- train %>% select(c(basin, NNSE))
test <- test %>% select(c(basin, NNSE))

meta <- read.csv("../CAMELS_Attributes/CAMELS_attributes_all.csv")
meta <- meta %>% select(c( gauge_id,
                           organic_frac,
                           elev_mean,
                           slope_mean,
                           area_gages2,
                           soil_depth_pelletier,
                           sand_frac,
                           silt_frac,
                           clay_frac,
                           geol_permeability,
                           p_mean,
                           pet_mean,
                           aridity,
                           frac_snow,
                           high_prec_freq,
                           high_prec_dur,
                           low_prec_freq,
                           low_prec_dur))

dat_train <- merge(train, meta, by.x = "basin", by.y = "gauge_id")
dat_test <- merge(test, meta, by.x = "basin", by.y = "gauge_id")

dat_train <- dat_train %>% select(-c(basin))
dat_train <- dat_train[complete.cases(dat_train),]
dat_test <- dat_test %>% select(-c(basin))
dat_test <- dat_test[complete.cases(dat_test),]


rf <- randomForest(
  NNSE ~ .,
  data=dat_train
)

rf

pred = predict(rf, newdata=dat_test[-1])
pred
pred <- pred %>% as.data.frame()

compare <- cbind(dat_test, pred)
colnames(compare)[19] <- "p_NNSE"

ggscatter(data = compare, 
          x = "NNSE", 
          y = "p_NNSE")+ 
  theme_bw()

lm_test_R2adj <- summary(lm(p_NNSE~NNSE, data = compare))$adj.r.squared %>%
  round(2) ## 0.45  

C1_RF_plot <- ggplot(compare, aes(x = NNSE, y = p_NNSE))+
  geom_abline(intercept = 0, slope = 1)+
  geom_point(shape=21, size=2,alpha=0.6)+
  geom_smooth(, method='lm', se = FALSE)+
  labs(x = "Cluster 1 Membership", y = "Predicted Cluster 1 Membership")+
  # scale_y_continuous(limits=c(0,1),labels = function(x) paste0(x*100, "%"))+
  # scale_x_continuous(limits=c(0,1),labels = function(x) paste0(x*100, "%"))+
  annotate(geom="text", x=.2, y=.92, label= bquote(paste("Adj. R"^"2", " = ",.(lm_test_R2adj))), color="blue")+ 
  theme_bw()+theme(legend.position = "bottom")
C1_RF_plot

