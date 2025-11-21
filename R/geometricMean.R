#' Calculate the Geometric Mean
#'
#' Computes the geometric mean of a numeric vector. The geometric mean is the
#' nth root of the product of n numbers, useful for data representing rates of
#' change or ratios.
#'
#' @param x A numeric vector. Zeros and NaN removed before the calculation of
#'   the mean and the warnings are handled according to the \code{warn}
#'   parameter.
#' @param warn Logical. If \code{TRUE} (default), warnings are issued when zeros
#'   or NaN values are encountered and removed. If \code{FALSE}, removal occurs
#'   silently.
#'
#' @return A numeric scalar representing the geometric mean of \code{x} (after
#'   removal of zeros and NaN values). Returns \code{NA} if any \code{NA} values
#'   are present in the input.
#'
#' @details
#' The function implements the following data cleaning steps:
#' \itemize{
#'   \item Zeros are removed before calculation (they would bias the result toward 0)
#'   \item NaN values are removed
#'   \item If any \code{NA} values are present, \code{NA} is returned immediately
#' }
#'
#' The geometric mean is calculated as: \eqn{GM = (x_1 \times x_2 \times ... \times x_n)^{1/n}}
#'
#' @examples
#' geometricMean(c(1, 2, 3, 4, 5))
#' geometricMean(c(2, 8))  # Should return 4
#' geometricMean(c(1, 0, 5), warn = FALSE)  # Silently removes zero
#'
#' @seealso
#' \code{\link{mean}} for arithmetic mean
#' \code{\link{prod}} for the product of vector elements
#'
#' @export
geometricMean <- function(x,warn = TRUE){
  if(0 %in% x){
    if(warn) warning("There are zeros in your dataset. They will be removed before taking the geometric mean.")
    x <- x[x!=0]
  }

  if(any(is.na(x))){
    if(any(all(is.nan(x)==is.na(x)))){
      if(warn) warning("There are NaNs in your dataset. They will be removed before taking the geometric mean.")
      x <- x[!is.nan(x)]
    } else {
      return(NA)
    }


  }
  prod(x)^(1/length(x))
}
