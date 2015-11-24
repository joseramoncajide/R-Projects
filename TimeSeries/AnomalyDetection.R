install.packages("devtools")
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)

help(AnomalyDetectionTs)

res = AnomalyDetectionTs(df.breakout, max_anoms=0.02, direction='both', plot=TRUE, title = "dds")
res$plot
res$anoms
