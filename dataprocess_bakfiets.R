ipak <- function(pkg){

  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.muenster.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
packages <- c( "raster", "dplyr",  "rgdal","Matrix","xgboost", "data.table" , "randomForest", "glmnet" )
ipak(packages)
install_github("mengluchu/APMtools")

#bakfietsdata
#lf_lo = list.files("/data/lu01/NWA/Bakfietsdata", pattern = "^.*morning.*.csv$", full.names = T)
lf_lo = list.files("E:/NWA/Bakfietsdata", pattern = "^.*morning.*.csv$", full.names = T)
bakfile1 = read.csv(lf_lo[1])
proj = "+proj=longlat +datum=WGS84"

#rasterdir = "/data/gghdc/gap/output/2019_09_nijmegen/1/laea"
#lf  = list.files(rasterdir, pattern = "*.map$", full.names = T)
#lf  = lf[ which(grepl("road_class|indust", lf))]
#sr = stack(lf)
#writeRaster(sr, "/data/lu01/NWA/allNLstack.grd", format="raster")
sr =  stack("E:/NWA/allNLstack.grd")
df = retrieve_predictor(sr, bakfile1, c("Lon", "Lat"), proj)}

max_depth = 5
eta = 0.02
nthread = 2
nrounds = 400
