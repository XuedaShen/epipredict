test_that("collapse factors", {

  # edges
  breaks <- 1:5
  counts <- c(0, 1, 2, 3)
  expect_identical(collapse_fct(breaks, counts), c(1L, 3:5))
  counts <- c(0, 0, 2, 3)
  expect_identical(collapse_fct(breaks, counts), c(1L, 4L, 5L))
  counts <- c(0, 1, 2, 0)
  expect_identical(collapse_fct(breaks, counts), c(1L, 3L, 5L))
  breaks <- 1:6
  counts <- c(0, 0, 1, 2, 0)
  expect_identical(collapse_fct(breaks, counts), c(1L, 4L, 6L))

  # middle
  breaks <- 1:5
  counts <- c(1, 0, 2, 4)
  expect_identical(collapse_fct(breaks, counts), c(1L, 3L, 4L, 5L))
  counts <- c(2, 0, 1, 4)
  expect_identical(collapse_fct(breaks, counts), c(1L, 2L, 4L, 5L))
  counts <- c(2, 0, 0, 4)
  expect_identical(collapse_fct(breaks, counts), c(1L, 4L, 5L))

  # both
  breaks <- 1:10
  counts <- c(0, 1, 0, 0, 2, 1, 2, 1, 0)
  expect_identical(collapse_fct(breaks, counts), c(1L, 5L, 6L, 7L, 8L, 10L))

  # return empty
  breaks <- 1:3
  counts <- c(0, 0)
  expect_identical(collapse_fct(breaks, counts), 1L)
})
