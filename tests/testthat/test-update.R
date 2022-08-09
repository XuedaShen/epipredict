test_that("update utils / validators work", {
  expect_true(has_unique_names(list(x=1, y=2)))
  expect_false(has_unique_names(list(x=1, y=2, x=4)))
  expect_false(has_unique_names(list(x=1, 2, z=4)))

  expect_error(validate_has_unique_names(list(x=1, y=2, x=4)))
})

f <- frosting() %>%
  layer_predict() %>%
  layer_naomit(.pred) %>%
  layer_add_forecast_date("2021-01-01")

test_that("update method for frosting works", {
  expect_error(update(f$layers[[2]], id = "blah", id = "blahblah"))
  expect_error(update(f$layers[[2]], ahead = "blah"))

  expect_identical(update(f$layers[[2]], id = "kill_the_nas")$id, "kill_the_nas")

})

test_that("we can also update the frosting", {
  expect_error(update_frosting_layers(f, layer_oopsie = list(id = "blah")))
  expect_error(update_frosting_layers(f, layer_naomit = c(id = "blah")))
  expect_identical(
    update_frosting_layers(f, layer_naomit = list(id = "blah"))$layers[[2]]$id,
    "blah")
  nf <- update_frosting_layers(
    f, layer_add_forecast_date = list(id = "blah", forecast_date = "2022")
  )
  expect_identical(nf$layers[[3]]$forecast_date, "2022")
  expect_identical(nf$layers[[3]]$id, "blah")
  nf <- update_frosting_layers(
    f,
    layer_add_forecast_date = list(id = "blah", forecast_date = "2022"),
    layer_naomit = list(id = "kill_the_nas")
  )
  expect_identical(nf$layers[[3]]$forecast_date, "2022")
  expect_identical(nf$layers[[3]]$id, "blah")
  expect_identical(nf$layers[[2]]$id, "kill_the_nas")
})

r <- epi_recipe(case_death_rate_subset) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_epi_naomit()


test_that("we can also update the recipe", {
  expect_error(update_recipe_steps(r, step_lag = list(id = "blah")))
  expect_error(update_recipe_steps(r, step_epi_lag = c(id = "blah")))
  expect_error(update_recipe_steps(r, step_epi_lag = list(lag = 1)))
  expect_identical(
    update_recipe_steps(r, step_epi_ahead = list(ahead = 4L))$steps[[2]]$ahead,
    4L)

  nr <- update_recipe_steps(r, step_epi_ahead = list(id = "blah", ahead = 4L))
  expect_identical(nr$steps[[2]]$ahead, 4L)
  expect_identical(nr$steps[[2]]$id, "blah")
  expect_error(update_recipe_steps(
    r,
    step_epi_ahead = list(id = "blah"),
    step_epi_ahead = list(id = "kill_the_nas")
  ))
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_naomit()
  nr <- update_recipe_steps(
    r,
    step_epi_lag = list(lag = 1:5, id = "we're_lagging"),
    step_epi_ahead = list(ahead = 2, id = "we're_leading")
  )
  expect_identical(nr$steps[[1]]$lag, 1:5)
  expect_identical(nr$steps[[1]]$id, "we're_lagging")
  expect_identical(nr$steps[[2]]$id, "we're_leading")
})
