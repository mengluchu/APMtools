#' error matrix outputs RMSE, MAE, IQR
#' @param validataion The validataion vector
#' @param prediction The prediction vector
#' @return an errormatrix with RMSE, MAE, IQR, relative RMSE, relative MAE, relative IQR, R-squared, Explained Variance
#' @details R-squared is caluclated as 1- mse(e)/var(y), which is the same as 1- ss(e)/ss(y). The explained variance is calculated as 1 - var(e)/var(y). For a machine learning technique, the error does not have zero mean. This means the R-squared indicates a reduction in sum of squares of errors (ss), instead of variance. Therefore, an additional indicator, epxlained variance is calculated.
#' @export

    error_matrix = function(validation, prediction) {
        rmse <- function(test, pred) {
            sqrt(mean((pred - test)^2))
        }
        MAE = function(test, pred) {
            mean(abs(pred - test))

        }
        IQR <- function(test, pred) {
            a2 = summary(as.vector(pred - test))
            as.vector(a2[5] - a2[2])

        }
        rIQR <- function(test, pred) {
            a2 = summary(as.vector(pred - test))
            as.vector(a2[5] - a2[2])/median(test)
            # divided by median
        }
        explained_variance <- function(test, pred) {
            1-   var(pred - test)/var(test)
        }
        Rsquared <- function(test, pred) {
            1-   mean((pred - test)^2)/var(test)
        }

        rrmse <- function(test, pred) {
            rmse = sqrt(mean((pred - test)^2))
            rmse/mean(test)
        }
        rMAE = function(test, pred) {
            mean(abs(pred - test))/mean(test)
        }
        rmse1 = rmse(validation, prediction)
        rrmse1 = rrmse(validation, prediction)

        MAE1 = MAE(validation, prediction)
        rMAE1 = rMAE(validation, prediction)
        IQR1 = IQR(validation, prediction)
        rIQR1 = rIQR(validation, prediction)

        rsqd1 = Rsquared(validation, prediction)
        expvar1 = explained_variance(validation, prediction)
        c(RMSE = rmse1, RRMSE = rrmse1, IQR = IQR1, rIQR = rIQR1, MAE=MAE1, rMAE = rMAE1, rsq = rsqd1, explained_var = expvar1)
    }


