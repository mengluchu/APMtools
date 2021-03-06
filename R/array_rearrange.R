#'@title  rearrange array from n-d to m-d, with m < n

#'@description array_rearrange (original_array=X,   flatten=c(2,3), position=1)
#'@param original_array array to rearranged
#'@param flatten dimensions to be flattened.
#'@param position place /order/position of the new dimension

array_rearrange <- function (original_array, flatten = c(1), position=min(flatten))
{
  flatten1 <- sort(flatten)
  a <- c(1:length(dim(original_array)))
  a <- a[-flatten1]
  apermarray <- aperm( original_array, c(flatten1,a ))

  arrschema <- c(prod(dim(original_array)[flatten1]), dim(original_array)[a])
  dim(apermarray) <- arrschema
  # now the new dimension is at the front i.e. the first dimension of an array.
  # the following codes put the new dimension at spicified or default position

  if(position == 1) # if position is at 1, same as newarr1
    newarr = apermarray
  else {
    b <- c(1: length(dim(apermarray)))
    if (position+1 > length(dim(apermarray))) #if position is at the last dimension
      s <- c(2:position, 1)
    else
      s <- c(2:position, 1, (position + 1):length(b))
    # new schema
    newarr<-aperm(apermarray, s)
  }
  return(newarr)
}
