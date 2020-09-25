#' get h2o cv prediction
#' @param h2omodel h2omodel
#' @return k-fold cv prediction
#' @export

get_h2o_pred = function(h2omodel)
{
  cvpreds_id  = h2omodel@model$cross_validation_holdout_predictions_frame_id$name
  as.data.frame(h2o.getFrame(cvpreds_id))$predict
}

