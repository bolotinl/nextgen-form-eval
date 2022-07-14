library(data.table)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")

file.list = list.files(path = getwd(), pattern = ".csv")
dat <- lapply(file.list[1:10], read.csv)

fold_0 <- dat[[1]] # None are that bad, min NSE is -1.8
fold_1 <- dat[[2]] # Min NSE is -10.15
fold_2 <- dat[[3]] # None are that bad, min NSE is -0.83
fold_3 <- dat[[4]] # Has several of the top worst NSEs of all basins, min is -80.4
fold_4 <- dat[[5]] # Min is -5
fold_5 <- dat[[6]] # Min is -5
fold_6 <- dat[[7]] # Min is -4.5
fold_7 <- dat[[8]] # The worst NSE, -93, second lowest is only -9
fold_8 <- dat[[9]] # Min is -10
fold_9 <- dat[[10]] 

fold_0$fold <- "fold_0"
fold_1$fold <- "fold_1"
fold_2$fold <- "fold_2"
fold_3$fold <- "fold_3"
fold_4$fold <- "fold_4"
fold_5$fold <- "fold_5"
fold_6$fold <- "fold_6"
fold_7$fold <- "fold_7"
fold_8$fold <- "fold_8"
fold_9$fold <- "fold_9"

dat_long <- rbindlist(list(fold_0, fold_1, fold_2, fold_3, fold_4, fold_5, fold_6, fold_7, fold_8, fold_9))
dat_long$NNSE <-1.0/(2.0-dat_long$NSE)

write.csv(dat_long, "LSTM_test_metrics_all.csv")
