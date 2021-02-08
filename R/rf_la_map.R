#' mapping rf_lasso
#' @param variabledf dataframe containing predictors and response
#' @param rasterstack predictor rasterstack
#' @param y_varname response name
#' @param grepstring predictors name matching grepl format
#' @param rfla_name name or directory to save the predicted map
#' @export
rf_La_map = function(variabledf,raster_stack, rfla_name= "rfla.tif", numtrees = 1000, mtry = 23, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring, ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training, ], prenres)
  y_test = variabledf[test, y_varname]
  x_test = variabledf[test, ]
  x_train = subset_grep(variabledf[training, ], grepstring)

  formu = as.formula(paste(y_varname, "~.", sep = ""))

  rf3 <- randomForest(formu, data = pre_mat, ntrees = numtrees, mtry = mtry)
  allp = predict(rf3, x_test, predict.all = T)  #get all the tree predictions, instead of the mean
  cvfit <- glmnet::cv.glmnet(as.matrix(x_train),variabledf[training, y_varname],
                             type.measure = "mse", standardize = TRUE, alpha = 1,
                             lower.limit = 0)
  # aggregate using a regularization, here lasso, you can also do elastic net, training alpha or specify alpha between 0 and 1
  rpre= predict(rf3, raster_stack, predict.all=T) # get all the tree predictions
  rf_la =  predict(cvfit, newx = rpre) # use the regularization (here lasso) model to predict
  writeRaster( rf_la, rfla_name, overwrite = TRUE )
}
