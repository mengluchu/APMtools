#' @title Lasso predictions
#' @export

Lasso_pre = function(variabledf,  alpha = 1, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast") {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat_all = subset_grep(variabledf[training, ], prenres)
  pre_mat = pre_mat_all%>%dplyr::select(-y_varname)
  pre_mat_tr = pre_mat[training, ]
  pre_mat_test = pre_mat[test, ]
  y_tr_value = variabledf[training, y_varname]
  y_test_value = variabledf[test, y_varname]

  cvfit <- glmnet::cv.glmnet(as.matrix(pre_mat_tr), y_tr_value, type.measure = "mse", standardize = TRUE, alpha = alpha, lower.limit = 0)
  as.vector(predict(cvfit, newx = as.matrix(pre_mat_test)))

}
