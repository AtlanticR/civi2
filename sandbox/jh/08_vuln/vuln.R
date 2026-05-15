#### NEW
x$vulnw_cat <- as.numeric(cut(x$vulnw, breaks=3, label=1:3))
hist(x$vulnw, breaks = 100)

abline(
  v = hist(x$vulnw, breaks = 3, plot = FALSE)$breaks,
  col = "red",
  lwd = 2
)

h$breaks

#values ≤ 0.4 (but > 0.3) → 1
#values ≤ 0.5 (but > 0.4) → 2
#values ≤ 0.7 (but > 0.5) → 3

## BUT WHEN I DO:
x$vulnw_cat[which(x$vulnw > 0.5)]

## THEY ARE NOT ALL 3 so?
## 0.5 is not necessarily a cut boundary for the cut function (see onenote)
## THE CORRECT WAY IS

x$vulnw <- transformSkewness(x$vulnw)

brks <- seq(min(x$vulnw, na.rm=TRUE),
            max(x$vulnw, na.rm=TRUE),
            length.out = 4)

brks

#Any value > 0.3219448 and ≤ 0.4286647 → category 1
#Any value > 0.4286647 and ≤ 0.5353845 → category 2
#Any value > 0.5353845 and ≤ 0.6421044 → category 3

x$vulnw_cat[which(x$vulnw > 0.5353845)]

## NOW TO SEE A VISUAL:
hist(x$vulnw, breaks = 100)

abline(
  v = brks,
  col = "red",
  lwd = 2
)


## WE ARE NOTICING THAT THE CUT IS LIKELY BE IMPACTED BY THE OUTLIER
## SO WE ARE CHECKING WHICH ARE 3 ST DEVS AWAY
# Median
med <- median(x$vulnw, na.rm = TRUE)

# Mean and SD
m <- mean(x$vulnw, na.rm = TRUE)
s <- sd(x$vulnw, na.rm = TRUE)

# Output
med

data.frame(
  level = c("-3sd", "-2sd", "-1sd", "median-based mean", "+1sd", "+2sd", "+3sd"),
  value = c(
    m - 3*s,
    m - 2*s,
    m - 1*s,
    m,
    m + 1*s,
    m + 2*s,
    m + 3*s
  )
)

## NEW

med <- median(x$vulnw, na.rm = TRUE)
s   <- sd(x$vulnw, na.rm = TRUE)

low  <- med - 3*s
high <- med + 3*s

in_range <- x$vulnw >= low & x$vulnw <= high & !is.na(x$vulnw)

# compute actual cut breaks (THIS is what cut() internally approximates)
cut_breaks <- seq(
  min(x$vulnw[in_range], na.rm = TRUE),
  max(x$vulnw[in_range], na.rm = TRUE),
  length.out = 4
)

hist(x$vulnw, breaks = 50, main = "vulnw with cut + SD limits")

abline(v = c(low, high), col = "red", lwd = 2, lty = 2)
abline(v = cut_breaks, col = "blue", lwd = 2)
