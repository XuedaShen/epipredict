sliding_index <- function(data,
                          ...,
                          lookback = 0L,
                          assess_start = 1L,
                          assess_stop = 1L,
                          complete = TRUE,
                          step = 1L,
                          skip = 0L) {
  rlang::check_dots_empty()

  if (!epiprocess::is_epi_df(data)) {
    rlang::abort("`data` must be an epi_df.")
  }

  step <- rsample:::check_step(step)
  skip <- rsample:::check_skip(skip)

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  id_out <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (!identical(skip, 0L)) {
    indices <- slice_skip(indices, skip)
  }

  if (!identical(step, 1L)) {
    indices <- slice_step(indices, step)
  }

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_index_split")
  )

  ids <- names0(length(indices), prefix = "Slice")

  attrib <- list(
    index = index_attrib,
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete,
    step = step,
    skip = skip
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_index", "rset")
  )
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname slide-resampling
sliding_period <- function(data,
                           index,
                           period,
                           ...,
                           lookback = 0L,
                           assess_start = 1L,
                           assess_stop = 1L,
                           complete = TRUE,
                           step = 1L,
                           skip = 0L,
                           every = 1L,
                           origin = NULL) {
  rlang::check_dots_empty()

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")
  step <- check_step(step)

  if (assess_start > assess_stop) {
    rlang::abort("`assess_start` must be less than or equal to `assess_stop`.")
  }

  index <- rlang::enexpr(index)
  loc <- tidyselect::eval_select(index, data)

  if (length(loc) != 1L) {
    rlang::abort("`index` must specify exactly one column in `data`.")
  }

  index_attrib <- index
  index <- data[[loc]]

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  id_out <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (!identical(skip, 0L)) {
    indices <- slice_skip(indices, skip)
  }

  if (!identical(step, 1L)) {
    indices <- slice_step(indices, step)
  }

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_period_split")
  )

  ids <- names0(length(indices), prefix = "Slice")

  attrib <- list(
    index = index_attrib,
    period = period,
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete,
    step = step,
    skip = skip,
    every = every,
    origin = origin
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_period", "rset")
  )
}

# ------------------------------------------------------------------------------

check_lookback <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`lookback` must have size 1."))
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`lookback` must be an integer of size 1, or `Inf`."))
  }

  if (x < 0L) {
    rlang::abort(paste0("`lookback` must be positive, or zero."))
  }

  vctrs::vec_cast(x, integer(), x_arg = "lookback")
}

check_assess <- function(x, arg) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`", arg, "` must have size 1."))
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`", arg, "` must be an integer of size 1, or `Inf`."))
  }

  if (x <= 0L) {
    rlang::abort(paste0("`", arg, "` must be positive."))
  }

  vctrs::vec_cast(x, integer(), x_arg = arg)
}

check_step <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`step` must have size 1."))
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`step` must be an integer of size 1."))
  }

  if (x <= 0L) {
    rlang::abort(paste0("`step` must be positive."))
  }

  vctrs::vec_cast(x, integer(), x_arg = "step")
}

check_skip <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`skip` must have size 1."))
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`skip` must be an integer of size 1."))
  }

  if (x < 0L) {
    rlang::abort(paste0("`skip` must be positive, or zero."))
  }

  vctrs::vec_cast(x, integer(), x_arg = "skip")
}

compute_complete_indices <- function(id_in, id_out) {
  # Remove where either list has a `NULL` element.
  # These are incomplete windows.
  id_in_na <- vctrs::vec_detect_missing(id_in)
  id_out_na <- vctrs::vec_detect_missing(id_out)

  id_either_na <- id_in_na | id_out_na

  id_in <- vctrs::vec_slice(id_in, !id_either_na)
  id_out <- vctrs::vec_slice(id_out, !id_either_na)

  purrr::map2(id_in, id_out, merge_lists)
}

slice_skip <- function(indices, skip) {
  n_indices <- length(indices)
  slicer <- rlang::seq2(skip + 1L, n_indices)
  vctrs::vec_slice(indices, slicer)
}

slice_step <- function(indices, step) {
  n_indices <- length(indices)
  slicer <- seq2_by(1L, n_indices, by = step)
  indices <- vctrs::vec_slice(indices, slicer)
}

seq2_by <- function(from, to, by) {
  if (length(from) != 1) {
    rlang::abort("`from` must be length one")
  }
  if (length(to) != 1) {
    rlang::abort("`to` must be length one")
  }

  by <- as.integer(by)

  if (length(by) != 1) {
    rlang::abort("`by` must be length one")
  }
  if (by <= 0L) {
    rlang::abort("`by` must be positive")
  }

  if (from > to) {
    integer()
  } else {
    seq.int(from, to, by = by)
  }
}
