#' @title rf predictions
#' @export
rf_pre = function(variabledf,  numtrees = 2000, mtry = 33, y_varname = c("day_value", "night_value", "value_mean"),   test, training, grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training, ], prenres)

  x_test = variabledf[test, ]

  formu = as.formula(paste(y_varname, "~.", sep = ""))

  rf3 <- ranger(formu, data = pre_mat, num.trees = numtrees, mtry = mtry, importance = "impurity")
  print(rf3)
  df = data.frame(imp_val = rf3$variable.importance)
  predictions(predict(rf3, data = x_test))
}
