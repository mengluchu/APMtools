#' Boost regression trees for LUR (using b)

#' @param variabledf the dataframe containing predictors and dependent variable
#' @param y_varname  name of the dependent variable.
#' @param training the index for the rows used for training.
#' @param test the index for the rows used for testing.
#' @param grepstring the variable/column names of predictors in Lasso, grepl stlye, e.g. 'ROAD|pop|temp|wind|Rsp|OMI|eleva|coast'
#' @return plot variable importance and an error matrix
#' @export


Brt_LUR = function(variabledf, opti = F, vis1 = T, ntree = 1000,  interaction.depth= 6,bag.fraction = 0.5,shrinkage= 0.01, y_varname = c("day_value", "night_value", "value_mean"), training, test, grepstring = "ROAD|pop|temp|wind|Rsp|OMI|eleva|coast", ...) {
    prenres = paste(y_varname, "|", grepstring, sep = "")
    pre_mat = subset_grep(variabledf[training, ], prenres)


    y_test = variabledf[test, y_varname]
    x_test = variabledf[test, ]



    if (opti) {
        Xmat = subset_grep(variabledf[training, ], grepstring)
        rf3 <- gbm.step(data = pre_mat, gbm.x = names(Xmat), gbm.y = y_varname, family = "gaussian", n.trees = ntree,interaction.depth= interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction)
        ntree = rf3$gbm.call$best.trees
    } else {
        formu = as.formula(paste(y_varname, "~.", sep = ""))
        rf3 = gbm(formula = formu, data = pre_mat, distribution = "gaussian", n.trees = ntree, interaction.depth= interaction.depth, shrinkage = shrinkage, bag.fraction = bag.fraction)
    }

    if (vis1) {
        plot(rf3)
    }
    pre_rf <- predict.gbm(rf3, x_test, n.trees = ntree, type = "response")
    # rf_residual <- pre_rf - rdf_test$NO2

    return(error_matrix(y_test, pre_rf))


}
