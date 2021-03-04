#' predict tiles using LA, RF, SGB
#' @param df the dataframe for building the model
#' @param rasstack rasterstack, predictors
#' @param yname the y variable name
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
predicLA_RF_XGBtiles= function (df, rasstack, yname, xgbname, rfname, laname, ntree,
                                  mtry, nrounds = 3000, eta = 0.007, gamma = 5, max_depth = 6,
                                  xgb_alpha = 0, xgb_lambda = 2, subsample = 0.7, grepstring)
{
  predfun <- function(model, data) {
    v <- predict(model, as.matrix(data))
  }
  dfpredict = subset_grep(df, grepstring = grepstring)
  rasstack = raster::subset(rasstack, names(dfpredict))
  re = names(rasstack)
  pre_mat3 = df %>% dplyr::select(re)
  stopifnot(all.equal(names(rasstack), names(pre_mat3)))
  pre_mat3 = na.omit(pre_mat3)
  yvar = df %>% dplyr::select(yname) %>% unlist()
  indep_dep = data.frame(yvar = yvar, pre_mat3)
  names(indep_dep)[1] = "yvar"
  formu = as.formula(paste("yvar", "~.", sep = ""))
  bst = randomForest(formu, data = indep_dep, ntree = ntree,
                     mtry = mtry)
  sdayR = predict(rasstack, bst)
  writeRaster(sdayR, rfname, overwrite = TRUE)
  L_day <- glmnet::cv.glmnet(as.matrix(pre_mat3), yvar, type.measure = "mse",
                             standardize = TRUE, alpha = 1, lower.limit = 0)
  sdayL = predict(rasstack, L_day, fun = predfun)
  writeRaster(sdayL, laname, overwrite = TRUE)
  xgb_stack(sr = rasstack, df_var = indep_dep, y_var = "yvar",
            xgbname = xgbname, nrounds = nrounds, eta = eta, gamma = gamma,
            max_depth = max_depth, xgb_alpha = xgb_alpha, xgb_lambda = xgb_lambda,
            subsample = subsample, grepstring = grepstring)
}

