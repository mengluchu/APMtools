#' @title xgb predictions
#' @export


xgb_pre = function(variabledf, max_depth = 4, eta = 0.02, nthread = 2,gamma=0, xgb_lambda, nrounds = 300, subsample = 0.7, y_varname = c("day_value", "night_value", "value_mean"), training , test ,   grepstring , ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  sub_mat = subset_grep(variabledf, prenres)

  pre_mat = sub_mat[training, ]
  y_train = sub_mat[training, y_varname]

  x_test = sub_mat[test, ]

  dfmatrix = as.matrix(pre_mat)
  outputvec = variabledf[training, y_varname]
  bst <- xgboost(data = dfmatrix, label = outputvec, gamma= gamma, max_depth = max_depth, lambda = xgb_lambda, eta = eta,  subsample = subsample, nthread = nthread, nrounds = nrounds, verbose = 0)
  print(bst)
  df_test = as.matrix(x_test)
  predict(bst, df_test)
}
