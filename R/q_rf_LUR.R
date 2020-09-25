#' Quantile Random forest  (using ranger, quantregForest)
#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param upq upper quantile default 0.95
#' @param lq lower quantile, default 0.05
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @return an error matrix and coverage probability, default 0.90 prediction interval.
#' @export


q_rf_LUR = function(variabledf,  upq = "quantile= 0.05",
                    lq = "quantile= 0.95",  numtrees = 1000, mtry = NULL, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring, ...) {
  prenres = paste(y_varname, "|", grepstring, sep = "")
  pre_mat = subset_grep(variabledf[training, ], prenres)


  y_test = variabledf[test, y_varname]
  x_test = variabledf[test, ]

  formu = as.formula(paste(y_varname, "~.", sep = ""))

  quantRF <- ranger(formu, data = pre_mat, num.trees = numtrees, mtry = mtry, importance = "permutation", quantreg = T)
  # compute predictions (mean) for each validation site

  pred.distribution <- predict(quantRF,
                               data = x_test,
                               type = "quantiles",
                               quantiles = seq(0.01, 0.99, by = 0.01))

  t.quant90 <- cbind(
    pred.distribution$predictions[, upq],
    pred.distribution$predictions[, lq])

  t.lower <- (t.quant90[,1])

  t.upper <- (t.quant90[,2])
  covprob90 = mean(t.lower <= y_test &  y_test <= t.upper)

  pred <- predictions( predict(quantRF, data =x_test, what = mean))

  # rf_residual <- pre_rf - rdf_test$NO2
  return(c(error_matrix(y_test, pred),  covprob90) )


}
