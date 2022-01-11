#'@title crop a (raster or stack) from center and quick view with mapview

#'@param ras raster or rasterstack
#'@param vis if True then apply mapview
#'@param size windowsize for cropping, number of pixels from the centre.
#'@export

crop_center= function(ras, size, vis=F,...){
a = crop(c, extent(ras, floor(nrow(ras)/2)-size, floor(nrow(ras)/2)+size, floor(ncol(ras)/2)-size, floor(ncol(ras)/2)+size))
if(vis)
  mapview(a)}
