#' Accuracy matrix of aggregating random forest using lasso

#' @param variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @param vis if true, plot variable importance
#' @return plot variable importance and an error matrix
#' @export


rf_Lasso_LUR = function(variabledf, vis = F, numtrees = 2000, mtry = 33, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring, ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training, ], prenres)
  y_test = variabledf[test, y_varname]
  x_test = variabledf[test, ]

  formu = as.formula(paste(y_varname, "~.", sep = ""))

  rf3 <- ranger(formu, data = pre_mat, num.trees = numtrees, mtry = mtry, importance = "impurity")

  df = data.frame(imp_val = rf3$variable.importance)

  if (vis) {
    imp_plot = ggplot(df, aes(x = reorder(rownames(df), imp_val), y = imp_val, fill = imp_val)) + geom_bar(stat = "identity", position = "dodge") + coord_flip() + ylab("Variable Importance") + xlab("") + ggtitle(paste("Information Value Summary",                                                                                                                                                                                                                        y_varname, sep = ": ")) + guides(fill = F) + scale_fill_gradient(low = "red", high = "blue")
    print(imp_plot)
  }
  pre = prediction_with_pp_La (rf3, as.matrix(pre_mat), variabledf[training, y_varname], as.matrix(variabledf[test,]))
  return(error_matrix(y_test, pre))
}