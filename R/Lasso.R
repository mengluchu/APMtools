#' Lasso model for LUR, crossvalidation, default fold (10)

#' @param  variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @return  error matrix, plot selected (min MSE ) coefficients
#' @export


Lasso = function(variabledf, vis1 = T, alpha = 1, printlambda =F, y_varname, training, test, grepstring ) {
    prenres = paste(y_varname, "|", grepstring, sep = "")
    pre_mat_all = subset_grep(variabledf, prenres)
    pre_mat = pre_mat_all%>%dplyr::select(-y_varname)

    pre_mat_tr = pre_mat[training, ]
    pre_mat_test = pre_mat[test, ]
    y_tr_value = variabledf[training, y_varname]
    y_test_value = variabledf[test, y_varname]

    cvfit <- glmnet::cv.glmnet(as.matrix(pre_mat_tr), y_tr_value, type.measure = "mse", standardize = TRUE, alpha = alpha, lower.limit = 0)
    if (printlambda){
    print(paste("min:", cvfit$lambda.min))
    print(paste("1se:", cvfit$lambda.1se))}


    if (vis1) {
        plot(cvfit)
        Lassoselected(cvfit)
    }
    elastic_pred = predict(cvfit, newx = as.matrix(pre_mat_test))
    error_matrix(y_test_value, elastic_pred)
}
