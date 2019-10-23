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
predicLA_RF_XGBtiles <-function(df, rasstack, yname, varstring = "road_class_|indus", xgbname, rfname, laname, ntree = 1000,   max_depth = 6, eta = 0.02, nthread = 4, nrounds = 1000, ...){
  predfun <- function(model, data) {
    v <- predict(model, as.matrix(data ))
  }

  #indep_dep = subset_grep(df, paste0(yname,"|",varstring) # RESPONSE+PREDICTOR matrix
  pre_mat3 = subset_grep(df, varstring) # prediction matrix
  # reorder the dataframe!
  re = names(rasstack)
  pre_mat3 = pre_mat3 %>% dplyr::select (re)

  # make sure the nams match!
  stopifnot(all.equal(names(rasstack), names(pre_mat3)))

  pre_mat3 = na.omit(pre_mat3)
  # .$y
  yvar = df%>% dplyr::select(yname)%>%unlist()
  #yvar = df%>% .$yname
  indep_dep = data.frame(yvar = yvar, pre_mat3)
  names(indep_dep)[1]="yvar"
  formu = as.formula(paste("yvar", "~.", sep = ""))

  ##RF
  bst = randomForest(formu, data = indep_dep, ntree = ntree )
  bst
  sdayR = predict(rasstack, bst)
  writeRaster(sdayR,rfname , overwrite = TRUE )

  # LA
  L_day <- glmnet::cv.glmnet(as.matrix(pre_mat3),yvar, type.measure = "mse", standardize = TRUE, alpha = 1, lower.limit = 0)
  sdayL = predict(rasstack, L_day, fun = predfun)
  writeRaster(sdayL, laname, overwrite = TRUE )

  #xgb
  #pre_mat3$NO2  = inde_var$NO2
  df1 = data.table(indep_dep, keep.rownames = F)
  dfmatrix = sparse.model.matrix(formu, data = df1)[, -1]
  bst <- xgboost(data = dfmatrix, label = yvar,  max_depth = max_depth, eta = eta, nthread = nthread, nrounds = nrounds, verbose = 0)
  sday = predict(rasstack, bst,  fun = predfun)
  writeRaster(sday, xgbname, overwrite = TRUE )

}
