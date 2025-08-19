#' Calculate the geometric mean
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
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
