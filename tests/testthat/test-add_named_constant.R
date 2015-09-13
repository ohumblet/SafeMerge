##### Introduction ####
#
#  Unit testing for the function "add_named_constant"
#
#  Inputs:
#     "add_named_constant", a function in the SafeMerge package
#
#  Output:
#     1) N/A
#
#  Author: Olivier Humblet
#
#-----------------------------------------------------------------#

library(testthat)

# # Source the file where the function is saved (with working directory set to that file's directory)
#
# source("OH_functions.R") # Perhaps no longer necessary if used as part of a package?

# Testing

context("Test add_named_constant")

df.test <- data.frame(v1 = 1:3)


test_that("It works for logical, numeric, and character inputs", {

  expect_true( all(add_named_constant(df.test, "in.x", TRUE)["in.x"] == TRUE) )
  expect_true( all(add_named_constant(df.test, "in.x", 1)["in.x"] == 1) )
  expect_true( all(add_named_constant(df.test, "in.x", "a")["in.x"] == "a") )

})

test_that("The new variable has the correct length", {

  expect_true( nrow(add_named_constant(df.test, "in.x", TRUE)["in.x"]) == 3 )

})

test_that("It works if we specify multiple variable names", {

  expect_true( all(add_named_constant(df.test, c("in.x", "in.y"), FALSE)["in.x"] == FALSE) )
  expect_true( all(add_named_constant(df.test, c("in.x", "in.y"), FALSE)["in.y"] == FALSE) )

})


