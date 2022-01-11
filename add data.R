#load("~/Downloads/global_annual.rda")

load("~/Documents/GitHub/Global mapping/station_predictor/global_annual.rda")
summary(global_annual$value_mean)
library(devtools)
use_data(global_annual, overwrite = T)
