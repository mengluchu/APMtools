#' @title xgb predictions
#' @export


xgb_pre = function(variabledf, max_depth = 4, eta = 0.02, nthread = 2,gamma=0, nrounds = 300, subsample = 0.7, y_varname = c("day_value", "night_value", "value_mean"), training , test ,   grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  sub_mat = subset_grep(variabledf, prenres)

  pre_mat = sub_mat[training, ]
  y_train = sub_mat[training, y_varname]

  x_test = sub_mat[test, ]

  df1 = data.table(pre_mat, keep.rownames = F)
  formu = as.formula(paste(y_varname, "~.", sep = ""))
  dfmatrix = sparse.model.matrix(formu, data = df1)[, -1]
  outputvec = variabledf[training, y_varname]
  bst <- xgboost(data = dfmatrix, label = outputvec, gamma = gamma, max_depth = max_depth, eta = eta,  subsample = subsample,thread = nthread, nrounds = nrounds, verbose = 0)
  print(bst)
  df_test = data.table(x_test, keep.rownames = F)
  dfmatrix_test = sparse.model.matrix(formu, data = df_test)[, -1]
  xgbpre = predict(bst, dfmatrix_test)
}
