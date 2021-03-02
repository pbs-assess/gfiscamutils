test_that("mean_na", {
  expect_equal(mean_na(x = NA), NA)
  expect_equal(mean_na(x = c(1:3, NA)), 2)
  expect_true(is.na(mean_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("sum_na", {
  expect_equal(sum_na(x = NA), NA)
  expect_equal(sum_na(x = c(1:3, NA)), 6)
  expect_true(is.na(sum_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("unique_na", {
  expect_equal(unique_na(x = NA), NA)
  expect_equal(unique_na(x = c(1:3, NA)), c(1:3, NA))
})

test_that("wt_mean_na", {
  expect_equal(wt_mean_na(x = NA), NA)
  expect_equal(wt_mean_na(x = c(1:3, NA)), 2)
  expect_equal(wt_mean_na(x = c(1:3, NA), w=c(1, 1, 2, NA)), 2.25)
  expect_true(is.na(wt_mean_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("max_na", {
  expect_equal(max_na(x = NA), NA)
  expect_equal(max_na(x = c(1:3, NA)), 3)
  expect_true(is.na(max_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("min_na", {
  expect_equal(min_na(x = NA), NA)
  expect_equal(min_na(x = c(1:3, NA)), 1)
  expect_true(is.na(min_na(x = c(1:3, NA), omit_na = FALSE)))
})

test_that("roll_mean_na", {
  expect_equal(roll_mean_na(dat = c(NA, NA), n=2), c(NA, NA))
  expect_equal(roll_mean_na(dat = c(1:3, NA), n=2), c(1.0, 2.0, 3.0, 2.5))
})
