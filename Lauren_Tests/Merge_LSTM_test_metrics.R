library(data.table)

setwd("/Volumes/GoogleDrive/.shortcut-targets-by-id/1VHhRiquNhK8-7koJsgKwgwtMr36BLLsV/Summer Institute Fellows 2022/NextGen1 - Model Selection and Evaluation/Data/LSTM Performance Metrics")

file.list = list.files(path = getwd(), pattern = ".csv")
dat <- lapply(file.list, read.csv)
dat <- rbindlist(dat)

write.csv(dat, "LSTM_test_metrics_all.csv")
