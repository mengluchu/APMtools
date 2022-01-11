#' Merge new predictor variables (rasters)
#' @param tomerge the dataframe to merge the raster files to
#' @param rasterfile the R raster object (raster package) to merge
#' @param  varname the name of the new variable
#' @param lon_lat_name a vector of the names of longitude and latitude. Commonly Longitude first and latitude second.
#' @return a dataframe with merged variables
#' @examples
#' \donttest{merged = mergeraster2file(merged, c("lon","lat"), 'elevation.map', 'elevation')}
#' @export
mergeraster2file = function(tomerge, rasterfile, lon_lat_name, varname) {
    location = tomerge%>%dplyr::select(lon_lat_name)
    rex = raster::extract(rasterfile, location)
    merged = cbind(tomerge, rex)
    names(merged)[ncol(merged)] = varname
    merged
}
