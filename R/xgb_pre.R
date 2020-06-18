#' @title xgb predictions
#' @export


xgb_pre = function(variabledf, max_depth = 4, eta = 0.02,  gamma=0, xgb_lambda=1, xgb_alpha = 0.02, nrounds = 300, subsample = 0.7, y_varname = c("day_value", "night_value", "value_mean"), training , test ,   grepstring , ...) {
  pre_mat =  subset_grep(variabledf, grepstring )%>%dplyr::select(-y_varname)
  x_train = pre_mat[training,]
  y_train = variabledf[training, y_varname]
  x_test = pre_mat[test, ]
  y_test = variabledf[test, y_varname]
  dfmatrix = as.matrix(x_train)
  outputvec = variabledf[training, y_varname]

  bst <- xgboost(data = dfmatrix, label = outputvec, gamma= gamma, max_depth = max_depth, lambda = xgb_lambda, alpha = xgb_alpha, eta = eta,  subsample = subsample,  nrounds = nrounds, verbose = 0)
  print(bst)
  df_test = as.matrix(x_test)
  predict(bst, df_test)
}
