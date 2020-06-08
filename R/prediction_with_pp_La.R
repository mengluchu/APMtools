#' Aggregate random forest using lasso
#' @param rfmodel ranger model
#' @param trainingXY training matrix, with same response and predictor names as in the rfmodel
#' @param testingXY  testing matrix, with same response and predictor names as in the rfmodel
#' @param trainingY training response vection
#' @export

prediction_with_pp_La = function(rfmodel, trainingXY, trainingY, testingXY)
{
  allp = predict(r1,trainingXY , predict.all = T)%>%predictions #get all the tree predictions, instead of the mean
  cvfit <- glmnet::cv.glmnet(allp,trainingY ,
                             type.measure = "mse", standardize = TRUE, alpha = 1,
                             lower.limit = 0)
  # aggregate using a regularization, here lasso, you can also do elastic net, training alpha or specify alpha between 0 and 1
  rpre= predict(r1,testingXY, predict.all=T)%>%predictions # get all the tree predictions
  predict(cvfit, newx = rpre) # use the regularization (here lasso) model to predict
}