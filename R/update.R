has_unique_names <- function(x) {
  nms <- names(x)
  if (length(nms) != length(x)) return(FALSE)
  if (any(is.na(nms)) | any(nms == "")) return(FALSE)
  !anyDuplicated(nms)
}

validate_has_unique_names <- function(x) {
  if (! has_unique_names(x)) {
    cli_stop("All of the changes supplied in `...` must be uniquely named.")
  }
  invisible(x)
}

#' @importFrom stats update
#' @export
stats::update

#' Update a frosting layer
#'
#' This `layer` method for `update()` takes named arguments as `...` who's
#' values will replace the elements of the same name in the actual `layer`.
#'
#' @param object A frosting `layer`.
#' @param ... Key-value pairs where the keys match up with names of elements
#' in the layer, and the values are the new values to update the layer with.
#'
#' @export
#' @examples
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred) %>%
#'   layer_add_forecast_date("2021-01-01")
#' f$layers[[2]]$id
#' f$layers[[3]]$forecast_date
#'
#' update(f$layers[[2]], id = "kill_all_the_nas")
update.layer <- function(object, ...) {
  changes <- list(...)
  validate_has_unique_names(changes)
  new_nms <- names(changes)
  old_nms <- names(object)
  layer_type <- class(object)[1]
  for (nm in new_nms) {
    if (! (nm %in% old_nms)) {
      cli_stop("The layer you are trying to update, ",
               "'{layer_type}', does not have the '{nm}' field.")
    }
    object[[nm]] <- changes[[nm]]
  }
  reconstruct_layer(object)
}

# The goal is to rebuild the layer so that it runs through validation
# However, this only checks the args, not whether those args are meaningful
# This mimics recipes, but I think we should fix it.
# Note that recipes typically validates args at bake time. I'd rather do it
# immediately.
reconstruct_layer <- function(x) {
  # Collect the subclass of the layer to use when recreating it
  subclass <- setdiff(class(x), "layer")
  args <- unclass(x) # A layer is just a list of its arguments
  # Construct the call and splice in the args
  # no prefix is needed because we know the full subclass
  call_step <- rlang::call2(.fn = "layer", subclass = subclass, !!!args,
                            .prefix = "", .ns = "epipredict")
  rlang::eval_tidy(call_step)
}

#' Update frosting layers
#'
#' Given an existing frosting object, update some (or all) of the layers
#' with different arguments.
#'
#' Note that not all layers or arguments can
#' be updated easily or at all. This function only accomodates named
#' arguments. Furthermore, frosting objects with multiple layers of the same
#' type can not be updated using this function.
#'
#' @param x A frosting object
#' @param ... named lists of layers to update with changes. Passed as
#'   `layer_name = list(parm1 = updated_pars1, parm2 = updated_pars2)`.
#'
#' @return An updated frosting object
#' @export
#'
#' @examples
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred) %>%
#'   layer_add_forecast_date("2021-01-01")
#' f$layers[[2]]$id
#' f$layers[[3]]$forecast_date
#'
#' f2 <- update_frosting(f, layer_naomit = list(id = "kill_all_the_nas!"),
#'         layer_add_forecast_date = list(forecast_date = "2022-01-01"))
#' f2$layers[[2]]$id
#' f2$layers[[3]]$forecast_date
update_frosting_layers <- function(x, ...) {
  validate_frosting(x)
  changes <- list(...)
  validate_has_unique_names(changes)
  if (! all(map_lgl(changes, is.list)))
    cli_stop("Updates to frosting must all be lists.")
  new_layers <- names(changes)
  old_layers <- map_chr(x$layers, ~ class(.x)[1])
  for (l in new_layers) {
    if (! (l %in% old_layers))
      cli_stop("The frosting object does not contain {l}.")
    loc <- which(l == old_layers)
    if (length(loc) > 1) {
      cli_stop("The frosting object contains multiple {l} layers.",
               "Unable to disambiguate. Try using `update()` manually.")
    }
    x$layers[[loc]] <- do.call(
      "update",
      c(list(object = x$layers[[loc]]), changes[[l]])
    )
  }
  x
}


#' Update recipe steps
#'
#' Given an existing recipe or epi_recipe, update some (or all) of the steps
#' with different arguments.
#'
#' Note that not all steps or arguments can
#' be updated easily or at all. This function only accomodates named
#' arguments. Furthermore, recipes with multiple steps of the same
#' type can not be updated using this function.
#'
#' @inheritParams update_frosting_layers
#'
#' @return An updated recipe.
#' @export
#'
#' @examples
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_naomit()
#'
#' r$steps[[2]]$ahead
#' r2 <- update_recipe_steps(r, step_epi_ahead = list(ahead = 14))
#' r2$steps[[2]]$ahead
#'
#' \dontrun{
#' update_recipe_steps(r, step_epi_lag = list(lag = 1:8))
#' r$steps[[3]] <- update(r$steps[[3]], lag = 1:8) # checks update for validity
#' r$steps[[3]]
#'
#' r$steps[[1]] <- update(r$steps[[1]], lag = -5) # shouldn't work but does
#' ## recipes::update is wrong / or arg validation happens somewhere else
#' r$steps[[1]]$lag <- -5:10 # works, but isn't checked for validity
#' }
update_recipe_steps <- function(x, ...) {
  if (! inherits(x, "recipe"))
    cli_stop("`x` must be a recipe or epi_recipe.")
  ## Issue: somehow detect if it has been prepped
  changes <- list(...)
  validate_has_unique_names(changes)
  if (! all(map_lgl(changes, is.list)))
    cli_stop("Updates to recipe must all be lists.")
  new_steps <- names(changes)
  old_steps <- map_chr(x$steps, ~ class(.x)[1])
  for (s in new_steps) {
    if (! (s %in% old_steps))
      cli_stop("The recipe does not contain {s}.")
    loc <- which(s == old_steps)
    if (length(loc) > 1) {
      cli_stop("The recipe contains multiple {s} layers.",
               "Unable to disambiguate. Try using `update()` manually.")
    }
    x$steps[[loc]] <- do.call(
      "update",
      c(list(object = x$steps[[loc]]), changes[[s]])
    )
  }
  x
}

