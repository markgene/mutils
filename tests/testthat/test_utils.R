library(testthat)
library(mutils)
context("Utilities")


test_that("Pipe operator", {
  expect_silent(iris %>% head())
})


test_that("load_object()", {
  x <- seq(10)
  tmpfile <- tempfile(fileext = ".Rda")
  save(list = "x", file = tmpfile)
  expect_silent(load_object(rda = tmpfile, name = "x"))
  expect_error(load_object(rda = tmpfile, name = "y"))
})
