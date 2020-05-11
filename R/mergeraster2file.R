#' Merge new predictor variables (rasters)
#' @param tomerge the dataframe to merge the raster files to
#' @param rasterfile the dir of the raster file to merge
#' @param  varname the name of the new variable
#' @param lon_lat_name a vector of the names of longitude and latitude.
#' @return a dataframe with merged variables
#' @examples
#' \donttest{merged = mergeraster2file(merged, c("lon","lat"), 'elevation.map', 'elevation')}
#' @export
mergeraster2file = function(tomerge, lon_lat_name,rasterfile, varname) {
    r = raster(rasterfile)

    location = tomerge%>%dplyr::select(lon_lat_name)
    rex = raster::extract(r, location)
    merged = cbind(tomerge, rex)
    names(merged)[ncol(merged)] = varname
    merged
}
