#' @title xgb predictions
#' @description  XGboost prediction on rasterstacks, allowing missing value
#' @details use as.matrix instead of a sparse matrix model, the R Matrix package is therefore not needed and missing data is kept in, though in situations with lots of zeros, it is less efficient than using a sparse matrix. The same dense array is used in the xgboost_imp, xgboost_LUR, and xgb_pre. The other funciton, predicLA_RF_XGB_tiles used a sparse matrix, which means rows with missing values are missed.
#' @param sr raster stack of predictors
#' @param df_var dataframe containing predictors and responses
#' @param y_var specify the name of the response
#' @param xgbname specify the name/directoray of the predicted raster file to save
#' @export
#'
xgb_stack = function(sr, df_var, y_var, xgbname= "xgb.tif", max_depth,
                     eta, nthread, gamma = 1, nrounds,
                     verbose = 0, xgb_lambda=0, xgb_alpha=0){
  re = names(sr)
  pre_mat3 = df_var %>% dplyr::select(re)
  stopifnot(all.equal(names(sr), names(pre_mat3)))

  yvar = df_var%>% dplyr::select(y_var) %>% unlist()
  #indep_dep = data.frame(yvar = yvar, pre_mat3)
  #df1 = data.table(indep_dep, keep.rownames = F)
  #formu = as.formula(paste("yvar", "~.-1", sep = ""))
  #dfmatrix = sparse.model.matrix(formu, data = df1) #seems have to drop na this way
  dfmatrix =  as.matrix(pre_mat3)
  bst <- xgboost(data = dfmatrix, label = yvar, max_depth = max_depth,
                 eta = eta, nthread = nthread, gamma = gamma, nrounds = nrounds,
                 verbose = verbose, lambda = xgb_lambda, alpha =xgb_alpha)
  predfun <- function(model, data) {
    v <- predict(model, as.matrix(data))
  }
  b= predict(sr, bst, fun = predfun)
  writeRaster(b, filename =  xgbname, overwrite=TRUE)}
