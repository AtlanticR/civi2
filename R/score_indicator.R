#' Score an Indicator Using Standard Deviation Thresholds
#'
#' This function scores a numeric indicator based on deviations from the median.
#' Values are classified into a 1–n_cat scale using ±k standard deviation limits:
#' values below the lower threshold are assigned 1, values above the upper
#' threshold are assigned n_cat, and values within the range are binned evenly.
#'
#' Optionally, a histogram can be generated showing the distribution of values
#' with out-of-range thresholds highlighted.
#'
#' @param data A data frame containing the indicator column.
#' @param col Character. Name of the numeric column to score. Default is "Value".
#' @param k Numeric. Number of standard deviations used to define outlier thresholds. Default is 3.
#' @param n_cat Integer. Number of score categories. Default is 5.
#' @param plot Logical. If TRUE, plots a histogram of the indicator with threshold lines.
#' @param a Boolean indicating if transformSkewness should be applied
#'
#' @return A numeric vector of scored values (1 to n_cat), with NA preserved.
#'
#' @details
#' Scoring rules:
#' \itemize{
#'   \item Values < median - k * sd are assigned score 1
#'   \item Values > median + k * sd are assigned score n_cat
#'   \item Remaining values are divided into equal-frequency bins between thresholds
#' }
#'
#' The function is intended for ecological or indicator-based scoring workflows
#' where relative deviation from central tendency is used to define status classes.
#'
#' @examples
#' \dontrun{
#' df$score <- score_indicator(df, col = "vulnw", plot = TRUE)
#' }
#'
#' @export

score_indicator <- function(data, col="Value", k = 3, n_cat = 5, plot = FALSE, transform=FALSE) {

  x <- data[[col]]

  if (transform) {
    message('transformation happening')
    x <- as.vector(transformSkewness(x))
  }

  med <- median(x, na.rm = TRUE)
  s   <- sd(x, na.rm = TRUE)

  low  <- med - k * s
  high <- med + k * s

  out <- rep(NA_real_, length(x))

  # outliers
  out[x < low] <- 1
  out[x > high] <- n_cat

  # in-range values
  in_range <- x >= low & x <= high & !is.na(x)

  if (any(in_range)) {

    cut_breaks <- seq(
      min(x[in_range], na.rm = TRUE),
      max(x[in_range], na.rm = TRUE),
      length.out = n_cat + 1
    )


    out[in_range] <- as.numeric(
      cut(
        x[in_range],
        breaks = n_cat,
        labels = 1:n_cat,
        include.lowest = TRUE
      )
    )
  }

  # -----------------------
  # PLOT OPTION (COLOURED)
  # -----------------------
  if (plot) {

    h <- hist(x, breaks = 50, plot = FALSE)

    # assign each bin a score based on midpoint
    bin_scores <- cut(
      h$mids,
      breaks = c(-Inf, low, high, Inf),
      labels = c("low", "mid", "high")
    )

    # map to colors
    bin_col <- ifelse(
      h$mids < low, "red",
      ifelse(h$mids > high, "darkred", "skyblue")
    )

    plot(h,
         col = bin_col,
         border = "white",
         main = paste0(col, " distribution (score-aware)"),
         xlab = col)

    abline(v = c(low, high),
           col = "red", lwd = 2, lty = 2)

    if (exists("cut_breaks")) {
      abline(v = cut_breaks,
             col = "blue",
             lwd = 2,
             lty = 3)
    }
  }
  return(out)
}
