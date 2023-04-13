#' Cut a numeric variable into a factor
#'
#' `step_epi_cut()` creates a *specification* of a recipe step that cuts a numeric
#'  variable into a factor based on provided boundary values
#'
#'  This version is different from [recipes::step_cut()]. Typical usage here,
#'  is to, at train time, add `(-Inf, Inf)` as lower and upper limits to the
#'  range so that only *internal* breaks are necessary.
#'  One could use `step_cut()` by adding these yourself, but for the moment,
#'  that implementation doesn't handle `NA`'s in the variable, nor are the
#'  `prep()` or `bake()` methods exported.
#'
#' @inheritParams recipes::step_center
#' @param breaks A numeric vector with at least one cut point.
#' @param extend_range_to_inf Logical (or a vector of 2 logicals). Should the
#'   breaks be extended to include the whole real line. Can be different, that
#'   is passing `c(TRUE, FALSE)` will extend only the lower range to `-Inf` but
#'   not the upper range.
#' @param include_outside_range Logical, indicating if values outside the
#'  range in the train set should be included in the lowest or highest bucket.
#'  Defaults to `FALSE`, values outside the original range will be set to `NA`.
#' @template step-return
#' @export
#' @details If `extend_range_to_inf` is FALSE, then unlike the `base::cut()`
#'  function there is no need to specify the
#'  min and the max values in the breaks. All values before the lowest break
#'  point will end up in the first bucket, all values after the last break
#'  points will end up in the last.
#'
#'  If `extend_range_to_inf` is TRUE, then there is no difference.
#'
#'  `step_cut()` will call `base::cut()` in the baking step with
#'  `include.lowest` set to `TRUE`.
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:10, y = 5:14)
#' rec <- recipe(df)
#'
#' # The min and max of the variable are used as boundaries
#' # if they exceed the breaks (when `extend_range_to_inf = FALSE`)
#' rec %>%
#'   step_epi_cut(x, breaks = 5, extend_range_to_inf  = FALSE) %>%
#'   prep() %>%
#'   bake(df)
#'
#' rec %>%
#'   step_epi_cut(x, breaks = 5) %>%
#'   prep() %>%
#'   bake(df)
step_epi_cut <- function(recipe,
                         ...,
                         role = NA,
                         trained = FALSE,
                         breaks,
                         extend_range_to_inf = TRUE,
                         include_outside_range = FALSE,
                         skip = FALSE,
                         id = rand_id("epi_cut")) {
  arg_is_lgl_scalar(trained, skip, include_outside_range)
  arg_is_numeric(breaks)
  arg_is_chr_scalar(id)
  arg_is_lgl(extend_range_to_inf)
  if (length(extend_range_to_inf) == 1L)
    extend_range_to_inf <- rep(extend_range_to_inf, 2)
  if (length(extend_range_to_inf) != 2L)
    rlang::abort("`extend_range_to_inf` must be a length 1 or 2 logical vector.")

  add_step(
    recipe,
    step_epi_cut_new(
      terms = enquos(...),
      trained = trained,
      role = role,
      breaks = breaks,
      extend_range_to_inf  = extend_range_to_inf,
      include_outside_range = include_outside_range,
      skip = skip,
      id = id
    )
  )
}

step_epi_cut_new <-
  function(terms, role, trained,
           breaks, extend_range_to_inf,
           include_outside_range, skip, id) {
    step(
      subclass = "epi_cut",
      terms = terms,
      role = role,
      trained = trained,
      breaks = breaks,
      extend_range_to_inf  = extend_range_to_inf,
      include_outside_range = include_outside_range,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_epi_cut <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  check_type(training[, col_names], types = c("double", "integer"))

  all_breaks <- vector("list", length(col_names))
  names(all_breaks) <- col_names
  for (col_name in col_names) {
    ab <- create_full_breaks(
      var = training[, col_name, drop = TRUE],
      breaks = x$breaks,
      extend_range = x$extend_range_to_inf)
    check_empty_buckets(col_name, training[, col_name, drop = TRUE], ab)
    full_breaks_check(ab)
    all_breaks[[col_name]] <- ab
  }

  step_epi_cut_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    breaks = all_breaks,
    extend_range_to_inf  = x$extend_range_to_inf,
    include_outside_range = x$include_outside_range,
    skip = x$skip,
    id = x$id
  )
}

create_full_breaks <- function(var, breaks, extend_range) {
  stopifnot(is.numeric(var), is.numeric(breaks))
  minv <- min(var, na.rm = TRUE)
  if (extend_range[1]) minv <- -Inf
  maxv <- max(var, na.rm = TRUE)
  if (extend_range[2]) maxv <- Inf
  if (minv < min(breaks)) breaks <- c(minv, breaks)
  if (maxv > max(breaks)) breaks <- c(maxv, breaks)
  sort(breaks)
}

check_empty_buckets <- function(name, x, breaks) {
  tt <- table(cut(x, breaks))
  if (any(tt < 1)) {
    cli::cli_abort(
      c("Variable {name} results in some empty buckets.",
        i = "{names(tt)[tt < 1]}")
    )
  }
}

full_breaks_check <- function(breaks) {
  if (length(breaks) == 1) {
    rlang::abort("In step_cut: variable is invariant and equal to break point.")
  }
  if (length(breaks) == 2) {
    rlang::warn("In step_cut: this will create a factor with one value only.")
  }
}

#' @export
bake.step_epi_cut <- function(object, new_data, ...) {
  check_new_data(names(object$breaks), object, new_data)

  for (col_name in names(object$breaks)) {
    res <- cut_var(
      new_data[, col_name, drop = TRUE],
      object$breaks[[col_name]],
      object$include_outside_range
    )
    new_data[, col_name] <- res
  }
  new_data
}

cut_var <- function(var, breaks, include_outside_range) {
  if (include_outside_range) {
    minv <- min(var, na.rm = TRUE)
    maxv <- max(var, na.rm = TRUE)
    if (minv < min(breaks)) breaks[1] <- minv
    if (maxv > max(breaks)) breaks[length(breaks)] <- max(var)
  }
  cutted_var <- cut(var, breaks, include.lowest = TRUE)
  if (include_outside_range) {
    cutted_var <- adjust_levels_min_max(cutted_var)
  }
  cutted_var
}

# this is necessary because bake.recipe does first learn
# original levels when prep.recipe is called and then reverts
# the levels when bake.recipe itself is called. Moreover,
# it is cleaner to show it in this way.
adjust_levels_min_max <- function(x) {
  stopifnot(is.factor(x))
  levs <- levels(x)
  if (length(levs) == 1) {
    return(factor(rep("[min,max]", length(x))))
  }
  first_level <- sub("(?<=\\[)(.*?)(?=,)", "min", levs[1], perl = TRUE)
  last_level <-
    sub("(?<=,)(.+?)(?=\\])", "max", levs[length(levs)], perl = TRUE)
  remaining_levs <- levs[-c(1, length(levs))]
  new_levs <- c(first_level, remaining_levs, last_level)
  names(new_levs) <- levs
  new_x <- new_levs[x]
  names(new_x) <- NULL
  names(new_levs) <- NULL
  factor(new_x, levels = new_levs)
}
