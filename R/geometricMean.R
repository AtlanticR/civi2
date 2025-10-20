#' Calculate the geometric mean
#'
#' @param x numeric vector
#'
#' @return numeric scalar, the geometric mean of x
#' @export
#'
#' @examples
#' geometricMean(c(1,2,3,4,5))
geometricMean <- function(x,warn = TRUE){
  if(0 %in% x){
    if(warn) warning("There are zeros in your dataset. They will be removed before taking the geometric mean.")
    x <- x[x!=0]
  }
  if(any(is.na(x))){
    if(warn) warning("There are NAs in your dataset. They will be removed before taking the geometric mean.")
    x <- x[!is.na(x)]
  }
  prod(x)^(1/length(x))
}
