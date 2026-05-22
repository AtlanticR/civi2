## Verifying each indicator

check_outliers_3sd <- function(value, plot = FALSE) {

  med <- median(value, na.rm = TRUE)
  s   <- sd(value, na.rm = TRUE)

  low  <- med - 3 * s
  high <- med + 3 * s

  in_range <- value >= low & value <= high & !is.na(value)


  # identify outliers (outside the range)
  outliers <- value < low | value > high
  has_outliers <- any(outliers, na.rm = TRUE)

  if (plot) {
    hist(value, breaks = 50,
         main = "Value distribution with ±3 SD limits")

    abline(v = c(low, high),
           col = "red", lwd = 2, lty = 2)

    cut_breaks <- seq(
      min(value[in_range], na.rm = TRUE),
      max(value[in_range], na.rm = TRUE),
      length.out = 4
    )


    abline(v = cut_breaks, col = "blue", lwd = 2)
  }
  return(list(
    has_outliers = has_outliers,
    n_outliers   = sum(outliers, na.rm = TRUE),
    low          = low,
    high         = high,
    outlier_idx  = which(outliers)
  ))
}




# 1. ind_coastal_sensitivity_index Has specific breaks

# 2. INDICATOR: ind_harbour_condition

## Not transformed (I don't think this one is)
check_outliers_3sd(ind_harbour_condition$Value, plot=TRUE)

## transformSkewness - This works
check_outliers_3sd(transformSkewness(ind_harbour_condition$Value), plot=TRUE)

## New way - This works
x <- ind_harbour_condition$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)




# 3. INDICATOR: ind_degree_of_protection
check_outliers_3sd(ind_degree_of_protection$Value, plot=TRUE)

## transformSkewness
check_outliers_3sd(transformSkewness(ind_degree_of_protection$Value), plot=TRUE)

## New way - this looks better
x <- ind_degree_of_protection$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)



# 4. INDICATOR: ind_sea_level_change
check_outliers_3sd(ind_sea_level_change$Value, plot=TRUE)

## transformSkewness
check_outliers_3sd(transformSkewness(ind_sea_level_change$Value), plot=TRUE)

## New way - This looks better
x <- ind_sea_level_change$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)





# 5. INDICATOR: ind_ice_day_change
check_outliers_3sd(ind_ice_day_change$Value, plot=TRUE)
## transformSkewness
check_outliers_3sd(transformSkewness(ind_ice_day_change$Value), plot=TRUE)


## New way - This looks better
x <- ind_ice_day_change$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)





# 6. INDICATOR: ind_replacement_cost
check_outliers_3sd(ind_replacement_cost$Value, plot=TRUE)
## transformSkewness
check_outliers_3sd(transformSkewness(ind_replacement_cost$Value), plot=TRUE)

## New way - This looks better
x <- ind_replacement_cost$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)





# 7. INDICATOR: ind_harbour_utilization
check_outliers_3sd(ind_harbour_utilization$Value, plot=TRUE)

## transformSkewness
check_outliers_3sd(transformSkewness(ind_harbour_utilization$Value), plot=TRUE)

## New way - This looks better
x <- ind_harbour_utilization$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)





# 8. INDICATOR: ind_sch_proximity_lakes
check_outliers_3sd(ind_sch_proximity_lakes$Value, plot=TRUE)


## transformSkewness
check_outliers_3sd(transformSkewness(ind_sch_proximity_lakes$Value), plot=TRUE)

## New way - This looks better
x <- ind_sch_proximity_lakes$Value
x_norm <- rnorm(length(x), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
check_outliers_3sd(x_norm, plot=TRUE)


### NEW NEW
score_indicator <- function(data, col="Value", k = 3, n_cat = 5, plot = FALSE) {

  x <- data[[col]]

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
  }
  return(out)
}
