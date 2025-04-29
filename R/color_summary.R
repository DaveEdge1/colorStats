#' Colored Summary Statistics
#'
#' @description
#' Provides basic summary statistics for a numeric vector with colored output
#' for better visualization in the console.
#'
#' @param x A numeric vector.
#' @param digits Number of decimal places to round to.
#'
#' @return A named list with summary statistics.
#'
#' @examples
#' color_summary(rnorm(100))
#' color_summary(runif(50), digits=4)
#'
#' @importFrom stats median sd quantile
#' @importFrom crayon green blue yellow red
#' @export
color_summary <- function(x, digits = 2) {
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  # Calculate statistics
  stats <- list(
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    q25 = quantile(x, 0.25, na.rm = TRUE),
    q75 = quantile(x, 0.75, na.rm = TRUE)
  )

  # Round values
  stats <- lapply(stats, round, digits = digits)

  # Print with colors
  cat(green("Summary Statistics:"), "\n")
  cat(blue("Mean:   "), stats$mean, "\n")
  cat(blue("Median: "), stats$median, "\n")
  cat(yellow("SD:     "), stats$sd, "\n")
  cat(red("Min:    "), stats$min, "\n")
  cat(red("Max:    "), stats$max, "\n")
  cat(yellow("Q25:    "), stats$q25, "\n")
  cat(yellow("Q75:    "), stats$q75, "\n")

  invisible(stats)
}
