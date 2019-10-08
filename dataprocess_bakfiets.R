ipak <- function(pkg){

  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos='http://cran.muenster.r-project.org')
  sapply(pkg, require, character.only = TRUE)
}
packages <- c( "raster", "dplyr", "devtools", "rgdal","Matrix","xgboost", "data.table" , "randomForest", "glmnet" )
ipak(packages)
install_github("mengluchu/APMtools")
library(APMtools)

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
sr =  stack("/data/lu01/NWA/allNLstack.grd")
sr =  stack("E:/NWA/allNLstack.grd")
df = retrieve_predictor(sr, bakfile1, c("Lon", "Lat"), proj)}

# predict tiles
xgbname = "xgb16-Jul_bakfiets.tif"
rfname = "rf16-Jul_bakfiets.tif"
laname = "la16-Jul_bakfiets.tif"
predicLA_RF_XGBtiles(df = df , rasstack = sr, yname = "NO2", varstring = "|road_class_|indus", xgbname=xgbname, rfname = rfname, laname = laname, ntree = 1000,   max_depth = 6, eta = 0.02, nthread = 4, nrounds = 1000 )}

#
xgb6 = raster(xgbname)
myTheme <- rasterTheme(region=c(brewer.pal(4, "Greys"), colorRamps::matlab.like2(n =10) ))
rf6 = raster(rfname)
La6 = raster(laname)
levelplot(stack(rf6, xgb6, La6), par.setting=myTheme, names.attr = c("Bakrf", "Bakxgb","BakLa"))


# data inspection
# use GeoHub

# model validation
training1 = 1: 6000
test1 = 8001: 9328

asub = df
vaststring = "road_class|indust"
xgboost_LUR(asub, max_depth =6, eta =0.02, nthread =4, nrounds = 1000,y_varname= c("NO2"), training = training1, test = test1, grepstring = vaststring)

rf_LUR(asub, y_varname= c("NO2"), ntree = 1000, training = training1, test = test1, grepstring = vaststring )
#RMSE       MAE       IQR
#2.1648822 1.0374890 0.8371235
Lasso(asub,alpha =1 , vis1  = T,"NO2",training = training1, test = test1,grepstring = vaststring )
#RMSE      MAE      IQR
#4.743484 3.084635 4.078639

impo = xgboost_imp(asub, max_depth =6, eta =0.02, nthread =4, nrounds = 1000, y_varname= c("NO2"), training = training1, test = test1, grepstring = vaststring )
impo = data.frame(impo)
rownames(impo)[order( impo)]
#"road_class_1_100"  "road_class_1_1000" "road_class_1_25"   "road_class_1_300"  "road_class_1_50"
#[6] "road_class_1_500"  "road_class_1_800"  "industry_25"       "industry_50"       "road_class_1_3000"
Lasso(asub, alpha =1 , vis1  = T, "NO2", training = training1, test = test1, grepstring = vaststring )


