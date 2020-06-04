#' subset by grep
#' @param df the dataframe to subset
#' @param grepstring the namestring of the colomns to grab, greplstyle. This function is actually the same as dplyr::select(matches()).
#' @export
subset_grep = function(df, grepstring) {
    df[, which(grepl(grepstring, names(df)))]
}
