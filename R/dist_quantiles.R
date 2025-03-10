#' @importFrom vctrs field vec_cast new_rcrd
new_quantiles <- function(values = double(), quantile_levels = double()) {
  arg_is_probabilities(quantile_levels)

  vec_cast(values, double())
  vec_cast(quantile_levels, double())
  stopifnot(length(values) == length(quantile_levels))
  stopifnot(!vctrs::vec_duplicate_any(quantile_levels))
  if (is.unsorted(quantile_levels)) {
    o <- vctrs::vec_order(quantile_levels)
    values <- values[o]
    quantile_levels <- quantile_levels[o]
  }
  if (is.unsorted(values, na.rm = TRUE)) {
    cli::cli_abort("`values[order(quantile_levels)]` produces unsorted quantiles.")
  }

  new_rcrd(list(values = values, quantile_levels = quantile_levels),
    class = c("dist_quantiles", "dist_default")
  )
}



#' @importFrom vctrs vec_ptype_abbr vec_ptype_full
#' @export
vec_ptype_abbr.dist_quantiles <- function(x, ...) "dist_qntls"
#' @export
vec_ptype_full.dist_quantiles <- function(x, ...) "dist_quantiles"

#' @export
format.dist_quantiles <- function(x, digits = 2, ...) {
  m <- suppressWarnings(median(x))
  paste0("quantiles(", round(m, digits), ")[", vctrs::vec_size(x), "]")
}


#' A distribution parameterized by a set of quantiles
#'
#' @param values A vector of values
#' @param quantile_levels A vector of probabilities corresponding to `values`
#'
#' @export
#'
#' @examples
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' quantile(dstn, p = c(.1, .25, .5, .9))
#' median(dstn)
#'
#' # it's a bit annoying to inspect the data
#' distributional::parameters(dstn[1])
#' nested_quantiles(dstn[1])[[1]]
#'
#' dist_quantiles(1:4, 1:4 / 5)
#' @importFrom vctrs as_list_of vec_recycle_common new_vctr
dist_quantiles <- function(values, quantile_levels) {
  if (!is.list(values)) values <- list(values)
  if (!is.list(quantile_levels)) quantile_levels <- list(quantile_levels)

  values <- as_list_of(values, .ptype = double())
  quantile_levels <- as_list_of(quantile_levels, .ptype = double())
  args <- vec_recycle_common(values = values, quantile_levels = quantile_levels)
  qntls <- as_list_of(map2(args$values, args$quantile_levels, new_quantiles))
  new_vctr(qntls, class = "distribution")
}

validate_dist_quantiles <- function(values, quantile_levels) {
  map(quantile_levels, arg_is_probabilities)
  common_length <- vctrs::vec_size_common( # aborts internally
    values = values,
    quantile_levels = quantile_levels
  )
  length_diff <- vctrs::list_sizes(values) != vctrs::list_sizes(quantile_levels)
  if (any(length_diff)) {
    cli::cli_abort(c(
      "`values` and `quantile_levels` must have common length.",
      i = "Mismatches found at position(s): {.val {which(length_diff)}}."
    ))
  }
  level_duplication <- map_lgl(quantile_levels, vctrs::vec_duplicate_any)
  if (any(level_duplication)) {
    cli::cli_abort(c(
      "`quantile_levels` must not be duplicated.",
      i = "Duplicates found at position(s): {.val {which(level_duplication)}}."
    ))
  }
}


#' Summarize a distribution with a set of quantiles
#'
#' @param x a `distribution` vector
#' @param probs a vector of probabilities at which to calculate quantiles
#' @param ... additional arguments passed on to the `quantile` method
#'
#' @return a `distribution` vector containing `dist_quantiles`
#' @export
#'
#' @examples
#' library(distributional)
#' dstn <- dist_normal(c(10, 2), c(5, 10))
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' # because this distribution is already quantiles, any extra quantiles are
#' # appended
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- c(
#'   dist_normal(c(10, 2), c(5, 10)),
#'   dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' )
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
extrapolate_quantiles <- function(x, probs, ...) {
  UseMethod("extrapolate_quantiles")
}

#' @export
#' @importFrom vctrs vec_data
extrapolate_quantiles.distribution <- function(x, probs, ...) {
  arg_is_probabilities(probs)
  dstn <- lapply(vec_data(x), extrapolate_quantiles, probs = probs, ...)
  new_vctr(dstn, vars = NULL, class = "distribution")
}

#' @export
extrapolate_quantiles.dist_default <- function(x, probs, ...) {
  values <- quantile(x, probs, ...)
  new_quantiles(values = values, quantile_levels = probs)
}

#' @export
extrapolate_quantiles.dist_quantiles <- function(x, probs, ...) {
  new_values <- quantile(x, probs, ...)
  quantile_levels <- field(x, "quantile_levels")
  values <- field(x, "values")
  new_quantiles(
    values = c(values, new_values),
    quantile_levels = c(quantile_levels, probs)
  )
}

is_dist_quantiles <- function(x) {
  is_distribution(x) & all(stats::family(x) == "quantiles")
}



#' @export
#' @importFrom stats median qnorm family
median.dist_quantiles <- function(x, na.rm = FALSE, ..., middle = c("cubic", "linear")) {
  quantile_levels <- field(x, "quantile_levels")
  values <- field(x, "values")
  if (0.5 %in% quantile_levels) {
    return(values[match(0.5, quantile_levels)])
  }
  if (length(quantile_levels) < 2 || min(quantile_levels) > 0.5 || max(quantile_levels) < 0.5) {
    return(NA)
  }
  if (length(quantile_levels) < 3 || min(quantile_levels) > .25 || max(quantile_levels) < .75) {
    return(stats::approx(quantile_levels, values, xout = 0.5)$y)
  }
  quantile(x, 0.5, ..., middle = middle)
}

# placeholder to avoid errors, but not ideal
#' @export
mean.dist_quantiles <- function(x, na.rm = FALSE, ..., middle = c("cubic", "linear")) {
  median(x, ..., middle = middle)
}

#' @export
#' @importFrom stats quantile
#' @import distributional
quantile.dist_quantiles <- function(
    x, p, ...,
    middle = c("cubic", "linear"),
    left_tail = c("normal", "exponential"),
    right_tail = c("normal", "exponential")) {
  arg_is_probabilities(p)
  middle <- match.arg(middle)
  left_tail <- match.arg(left_tail)
  right_tail <- match.arg(right_tail)
  quantile_extrapolate(x, p, middle, left_tail, right_tail)
}


quantile_extrapolate <- function(x, tau_out, middle, left_tail, right_tail) {
  tau <- field(x, "quantile_levels")
  qvals <- field(x, "values")
  r <- range(tau, na.rm = TRUE)
  qvals_out <- rep(NA, length(tau_out))

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau)) {
    return(qvals[match(tau_out, tau)])
  }
  if (length(qvals) < 3 || r[1] > .25 || r[2] < .75) {
    cli::cli_warn(c(
      "Quantile extrapolation is not possible with fewer than",
      "3 quantiles or when the probs don't span [.25, .75]"
    ))
    return(qvals_out)
  }

  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr

  if (middle == "cubic") {
    method <- "cubic"
    result <- tryCatch(
      {
        Q <- stats::splinefun(tau, qvals, method = "hyman")
        qvals_out[indm] <- Q(tau_out[indm])
        quartiles <- Q(c(.25, .5, .75))
      },
      error = function(e) {
        return(NA)
      }
    )
  }
  if (middle == "linear" || any(is.na(result))) {
    method <- "linear"
    quartiles <- stats::approx(tau, qvals, c(.25, .5, .75))$y
  }


  if (any(indm)) {
    qvals_out[indm] <- switch(method,
      linear = stats::approx(tau, qvals, tau_out[indm])$y,
      cubic = Q(tau_out[indm])
    )
  }
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(
      tau_out[indl], quartiles, "left", left_tail
    )
  }
  if (any(indr)) {
    qvals_out[indr] <- tail_extrapolate(
      tau_out[indr], quartiles, "right", right_tail
    )
  }
  qvals_out
}

tail_extrapolate <- function(tau_out, quartiles, tail, type) {
  if (tail == "left") {
    p <- c(.25, .5)
    par <- quartiles[1:2]
  }
  if (tail == "right") {
    p <- c(.75, .5)
    par <- quartiles[3:2]
  }
  if (type == "normal") {
    return(norm_tail_q(p, par, tau_out))
  }
  if (type == "exponential") {
    return(exp_tail_q(p, par, tau_out))
  }
}


exp_q_par <- function(q) {
  # tau should always be c(.75, .5) or c(.25, .5)
  iqr <- 2 * abs(diff(q))
  s <- iqr / (2 * log(2))
  m <- q[2]
  return(list(m = m, s = s))
}

exp_tail_q <- function(p, q, target) {
  ms <- exp_q_par(q)
  qlaplace(target, ms$m, ms$s)
}

qlaplace <- function(p, centre = 0, b = 1) {
  # lower.tail = TRUE, log.p = FALSE
  centre - b * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

norm_q_par <- function(q) {
  # tau should always be c(.75, .5) or c(.25, .5)
  iqr <- 2 * abs(diff(q))
  s <- iqr / 1.34897950039 # abs(diff(qnorm(c(.75, .25))))
  m <- q[2]
  return(list(m = m, s = s))
}

norm_tail_q <- function(p, q, target) {
  ms <- norm_q_par(q)
  stats::qnorm(target, ms$m, ms$s)
}

#' @method Math dist_quantiles
#' @export
Math.dist_quantiles <- function(x, ...) {
  quantile_levels <- field(x, "quantile_levels")
  values <- field(x, "values")
  values <- vctrs::vec_math(.Generic, values, ...)
  new_quantiles(values = values, quantile_levels = quantile_levels)
}

#' @method Ops dist_quantiles
#' @export
Ops.dist_quantiles <- function(e1, e2) {
  is_quantiles <- c(
    inherits(e1, "dist_quantiles"),
    inherits(e2, "dist_quantiles")
  )
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  tau1 <- tau2 <- NULL
  if (is_quantiles[1]) {
    q1 <- field(e1, "values")
    tau1 <- field(e1, "quantile_levels")
  }
  if (is_quantiles[2]) {
    q2 <- field(e2, "values")
    tau2 <- field(e2, "quantile_levels")
  }
  tau <- union(tau1, tau2)
  if (all(is_dist)) {
    cli::cli_abort(
      "You can't perform arithmetic between two distributions like this."
    )
  } else {
    if (is_quantiles[1]) {
      q2 <- e2
    } else {
      q1 <- e1
    }
  }
  q <- vctrs::vec_arith(.Generic, q1, q2)
  new_quantiles(values = q, quantile_levels = tau)
}

#' @method is.na distribution
#' @export
is.na.distribution <- function(x) {
  sapply(vec_data(x), is.na)
}

#' @method is.na dist_quantiles
#' @export
is.na.dist_quantiles <- function(x) {
  q <- field(x, "values")
  all(is.na(q))
}
