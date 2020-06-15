#' apply ranger to rasterstrack
#' @param df dataframe with response and predictors
#' @param yname the response variable
#' @param rasstack the rasterstack for the model to be applied to
#' @export
ranger_stack = function(df, yname, rasstack, mtry, num.trees )
{
  formu = as.formula(paste(yname, "~.", sep = ""))
  ra = ranger(formu, df, num.trees = num.trees, mtry = mtry )

  nam = names(rasstack)
  rasstack[is.na(rasstack[])] = -1
  names(rasstack) = nam # fill the missing value to -1
  rr =predict(ra,  as.matrix(rasstack))
  rangerprediction= rasstack[[1]] # get a layer and then put predictions in
  values(rangerprediction)= rr%>%predictions
  rangerprediction
}
