#' Transforms skewness to be within -0.5 and 0.5
#'
#' This function looks at the skewness of a variable. If it is not within
#' -0.5 and 0.5 (i.e. normal distribution) it will determine the lambda
#' required to transform the data to have a skewness equal to that of a normal
#' distribution.
#'
#' @param data a vector of data
#' @importFrom MASS boxcox
#' @importFrom e1071 skewness
#' @export
#'
#' @examples
#' \dontrun{
#' data <- c(1,5,6,1000,60000,60001, 60002)
#' tS <- transformSkewness(data=data)
#' }
#'
#' @return vector of transformed data

transformSkewness <- function(v) {

  # Calculate initial skewness of v
  initial_skewness <- skewness(v, na.rm=TRUE)
  # Define a function to calculate skewness for a given lambda
  calculate_skewness <- function(lambda, v) {
    skew <- skewness(as.vector(t(abs(v)^lambda)), na.rm=TRUE)
    return(skew)
  }

  # Define an objective function to minimize the difference from target range
  objective_function <- function(lambda, v, target_range = c(-0.5, 0.5)) {
    skew <- calculate_skewness(lambda, v)
    penalty <- abs(skew - mean(target_range, na.rm=TRUE))
    return(penalty)
  }

  # Use optimize() to find lambda
  opt_result <- optimize(f = function(lambda) objective_function(lambda, v),
                         interval = c(0, 3),  # Adjust interval as needed
                         maximum = FALSE)

  # Retrieve the optimal lambda
  optimal_lambda <- opt_result$minimum
  final_skewness <- calculate_skewness(optimal_lambda, v)

  #cat(sprintf("Final skewness with optimal lambda %.4f: %.4f\n", optimal_lambda, final_skewness))

  cat("Initial skewness", initial_skewness, " final skewness :", final_skewness, " optimal lambda: ", optimal_lambda, "\n")

  # Return transformed values with optimal lambda
  transformed_values <- as.vector(t(abs(v)^optimal_lambda))
  return(transformed_values)
}


# transformSkewness <- function(data) {
#   library(MASS)
#
#   # Remove NA and NaN values
#   data_clean <- na.omit(data)
#   data_clean <- data_clean[!is.nan(data_clean)]
#
#   # Ensure data is positive for Box-Cox transformation
#   data_pos <- data_clean - min(data_clean) + 1
#
#   # Calculate initial skewness
#   initial_skewness <- sum((data_pos - mean(data_pos))^3) / ((length(data_pos) - 1) * sd(data_pos)^3)
#
#   # Apply Box-Cox transformation
#   boxcox_result <- boxcox(data_pos ~ 1, plot = FALSE)
#   lambda <- boxcox_result$x[which.max(boxcox_result$y)]
#
#   # Apply appropriate transformation based on lambda
#   if (abs(lambda) < 1e-6) {  # Check if lambda is close to 0
#     transformed_data <- log(data_pos)
#     lambda <- 0  # For clarity, set lambda to 0 when log transformation is applied
#   } else {
#     transformed_data <- (data_pos ^ lambda - 1) / lambda
#   }
#
#   # Calculate final skewness
#   final_skewness <- sum((transformed_data - mean(transformed_data))^3) / ((length(transformed_data) - 1) * sd(transformed_data)^3)
#
#   # Print initial skewness, final skewness, and lambda
#   cat("Initial skewness:", initial_skewness, "\n")
#   cat("Optimal lambda:", lambda, "\n")
#   cat("Final skewness:", final_skewness, "\n")
#
#   # Optionally scale to ensure skewness is within target range
#   if (final_skewness < -0.5 || final_skewness > 0.5) {
#     transformed_data <- scale(transformed_data, center = TRUE, scale = TRUE)
#     final_skewness <- sum((transformed_data - mean(transformed_data))^3) / ((length(transformed_data) - 1) * sd(transformed_data)^3)
#     cat("Final skewness after scaling:", final_skewness, "\n")
#   }
#
#   return(transformed_data)
# }
#
