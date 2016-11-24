#' Cumulative product function that accounts for NAs
#'
#' The function \code{f_cumprodtest} calculates cumulative products of a numeric vector, adjusting for the occurence of NAs on left-hand side of sample.
#'
#' @param x single numeric vector.
#'
#' @details This is merely a support function to deal with cumulative products of series that may or may not have NAs in the early part of their sample.
#'
#' @return A numeric vector of the same length as x.
#'
#' @export
#'
#' @examples
#' require(zoo)
#' f_cumprodtest(c(NA, NA, 1.5, 2, 3))
#' f_cumprodtest(c(1.2, 1.5, 2, 3))

f_cumprodtest <- function(x) {
  x1 <- na.trim(x, sides = "left")
  if (length(x1) < length(x)) x2 <- c(1, x1) else x2 <- x1
  x3 <- cumprod(x2)
  c(rep(NA, length(x) - length(x3)), x3)
}
