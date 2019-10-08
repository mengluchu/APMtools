#' predict tiles using LA, RF, SGB
#' @param df the dataframe for building the model
#' @param rasstack rasterstack, predictors
#' @param yname the y variable name
#' @param varstring the variables to subset (grep style)
#' @param xgbname output filename for xgb
#' @param rfname output filename for rf
#' @param lanme output filename for LA
#' @param ntree RF ntree, default 1000
#' @examples
#' \donttest{
#' xgbname = "/data/lu01/NWA/xgb6-Jul_oaq.tif"
#' rfname = "/data/lu01/NWA/RF6-Juloaq.tif"
#' laname= "/data/lu01/NWA/LA6-Juloaq.tif"
#' lus = raster("/data/lu01/NWA/predictor/NLstack.grd")
#' lf_lo = list.files("/data/lu01/NWA/Bakfietsdata", pattern = "^.*morning.*.csv$", full.names = T)
#' bakfile1 = read.csv(lf_lo[1])
#' proj = "+proj=longlat +datum=WGS84"
#' df = retrieve_predictor(lus, bakfile1, c("Lon", "Lat"), proj)
#' predicLA_RF_XGBtiles(df, lus, "NO2", xgbname=xgbname, rfname = rfname, laname = laname )}

#' @export
predicLA_RF_XGBtiles <-function(df, rasstack, yname, varstring = "|road_class_|indus", xgbname, rfname, laname, ntree = 1000,   max_depth = 6, eta = 0.02, nthread = 4, nrounds = 1000, ...){

  inde_var = subset_grep(df, paste0(yname,varstring)) #subset variables
  pre_mat3 = subset_grep(inde_var, varstring) # prediction matrix
  # reorder the dataframe!
  pre_mat3 %>% select (names(rasstack)) -> pre_mat3
  # make sure the nams match!
  stopifnot(all.equal(names(rasstack), names(pre_mat3)))

  pre_mat3 = na.omit(pre_mat3)
  inde_var = na.omit(inde_var)
  formu = as.formula(paste(yname, "~.", sep = ""))

  #xgb
  #pre_mat3$NO2  = inde_var$NO2
  df1 = data.table(inde_var, keep.rownames = F)
  dfmatrix = sparse.model.matrix(formu, data = df1)[, -1]
  bst <- xgboost(data = dfmatrix, label = inde_var[, yname],  max_depth = max_depth, eta = eta, nthread = nthread, nrounds = nrounds, verbose = 0)
  sday = predict(rasstack, bst,  fun = predfun)
  writeRaster(sday, xgbname, overwrite = TRUE )

  ##RF
  bst = randomForest(formu, data = inde_var, ntree = ntree, ...)
  sdayR = predict(rasstack, bst)
  writeRaster(sdayR,rfname , overwrite = TRUE )

  # LA
  L_day <- glmnet::cv.glmnet(as.matrix(pre_mat3), inde_var[, yname], type.measure = "mse", standardize = TRUE, alpha = 1, lower.limit = 0)
  sdayL = predict(rasstack, L_day, fun = predfun)
  writeRaster(sdayL, laname, overwrite = TRUE )
}
