
#' Weighted interval score
#'
#' Computes weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param x A vector of class `distribution`.
#' @param actual A vector of observed values
#' @param ... additional named arguments. See methods.
#'
#' @return A vector of weighted interval scores
#' @export
#'
#' @examples
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2,.4,.6,.8)))
#' weighted_interval_score(dstn, c(2, 10.5))
weighted_interval_score <- function(x, actual, ...) {
  UseMethod("weighted_interval_score")
}

#' @export
weighted_interval_score.distribution <- function(x, actual, ...) {
  arg_is_numeric(actual, allow_na = TRUE)
  n <- length(actual)
  if (n > 1 && n != length(x)) {
    cli_stop("actual must be a scalar or a vector the same length as x.")
  }
  map2_dbl(vec_data(x), actual, weighted_interval_score, ...)
}

#' @export
weighted_interval_score.dist_default <- function(x, actual, probs, ...) {
  q <- quantile(x, probs, ...)
  x <- new_quantiles(q, probs)
  weighted_interval_score(x, actual)
}

#' @export
weighted_interval_score.dist_quantiles <- function(x, actual,...) {

  q <- field(x, "q")
  tau <- field(x, "tau")
  wis <- 2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)))
  return(wis)
}
