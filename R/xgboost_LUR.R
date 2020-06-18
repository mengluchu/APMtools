#' xgBoost regression trees for LUR
#' @param variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @return plot variable importance and an error matrix
#' @export


xgboost_LUR = function(variabledf, max_depth = 4, eta = 0.02,  xgb_lambda=1,xgb_alpha = 0.02, nrounds = 300, gamma = 1, subsample = 0.7, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring, ...) {

    pre_mat =  subset_grep(variabledf, grepstring )%>%dplyr::select(-y_varname)
    x_train = pre_mat[training,]
    y_train = variabledf[training, y_varname]
    x_test = pre_mat[test, ]
    y_test = variabledf[test, y_varname]
    dfmatrix = as.matrix(x_train)
    outputvec = variabledf[training, y_varname]

    bst <- xgboost(data = dfmatrix, label = outputvec, max_depth =max_depth, gamma=gamma, eta =eta,  lambda = xgb_lambda, alpha = xgb_alpha, nrounds =nrounds,verbose = 0)
    dfmatrix_test = as.matrix(x_test)
    xgbpre = predict(bst, dfmatrix_test)
    error_matrix(y_test, xgbpre)
}
