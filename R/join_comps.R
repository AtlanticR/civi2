#' join_comps
#'
#' Join multiple indicator data.frames into a single data.frame by HarbourCode.
#'
#' @param x named list of indicator data.frames. The name of the data.frame in the list will be used as a prefix for the Value and Score columns. All data.frames must have a HarbourCode,Value, and Score columns.
#'
#' @returns A single data.frame with HarbourCode and the renamed Value and Score columns from each indicator data.frame, prefixed by the indicator name.
#' @export
#'
#' @examples
#' df1 <- data.frame(HarbourCode = c("H1", "H2"), Value = c(10, 20), Score = c(1, 2))
#' df2 <- data.frame(HarbourCode = c("H1", "H3"), Value = c(30, 40), Score = c(3, 4))
#' join_comps(list(Indicator1 = df1, Indicator2 = df2))
join_comps <- function(x) {
  x |>
    imap(function(df, dfname) {
      if (!all(c("HarbourCode", "Value", "Score") %in% colnames(df))) {
        stop(paste0("Data frame '", dfname, "' must contain HarbourCode, Value, and Score columns."))
      }
      df |>
        rename(!!paste0(dfname, "_Value") := Value,
               !!paste0(dfname, "_Score") := Score)
    }) |>
    reduce(full_join, by = "HarbourCode")
}
