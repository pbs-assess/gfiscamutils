test_that("MeanNA", {
  expect_equal(MeanNA(x = NA), NA)
  expect_equal(MeanNA(x = c(1:3, NA)), 2)
  expect_true(is.na(MeanNA(x = c(1:3, NA), omitNA = FALSE)))
})

test_that("SumNA", {
  expect_equal(SumNA(x = NA), NA)
  expect_equal(SumNA(x = c(1:3, NA)), 6)
  expect_true(is.na(SumNA(x = c(1:3, NA), omitNA = FALSE)))
})

test_that("UniqueNA", {
  expect_equal(UniqueNA(x = NA), NA)
  expect_equal(UniqueNA(x = c(1:3, NA)), c(1:3, NA))
})

test_that("WtMeanNA", {
  expect_equal(WtMeanNA(x = NA), NA)
  expect_equal(WtMeanNA(x = c(1:3, NA)), 2)
  expect_equal(WtMeanNA(x = c(1:3, NA), w=c(1, 1, 2, NA)), 2.25)
  expect_true(is.na(WtMeanNA(x = c(1:3, NA), omitNA = FALSE)))
})

test_that("MaxNA", {
  expect_equal(MaxNA(x = NA), NA)
  expect_equal(MaxNA(x = c(1:3, NA)), 3)
  expect_true(is.na(MaxNA(x = c(1:3, NA), omitNA = FALSE)))
})

test_that("MinNA", {
  expect_equal(MinNA(x = NA), NA)
  expect_equal(MinNA(x = c(1:3, NA)), 1)
  expect_true(is.na(MinNA(x = c(1:3, NA), omitNA = FALSE)))
})
