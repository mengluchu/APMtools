#'@title crop to center and quick view with mapview

#'@param ras raster or rasterstack
#'@param vis if True then apply mapview
#'@param size windowsize for cropping

crop_center= function(ras, size, vis=F,...){
a = crop(c, extent(c, floor(nrow(c)/2)-size, floor(nrow(c)/2)+size, floor(ncol(c)/2)-size, floor(ncol(c)/2)+size))
if(vis)
  mapview(a)}
