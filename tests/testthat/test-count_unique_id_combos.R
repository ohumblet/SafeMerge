##### Introduction ####
#
#  Unit testing for the function "count_unique_id_combos"
#
#  Inputs:
#     "count_unique_id_combos", a function in the SafeMerge package
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

context("Test count_unique_id_combos")

# experimenting with unique and length
df.test <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
                      v2 = rep(c(1,2,3), 3),
                      v3 = runif(9)
                      )


test_that("It throws an error if no by.i variables are specified", {

  expect_error( count_unique_id_combos(df.test), "One or more by.i variables must be specified." )

})

test_that("It throws an error when by.i variables are specified that are not part of the input data frame", {

  expect_error( count_unique_id_combos(df.test, "v4"), "by.i must specify variable names that are all part of the input data frame." )

})


test_that("The correct number of combinations is returned with 1 by.i variable", {

  expect_true( count_unique_id_combos(df.test, "v1") == 3 )

})

test_that("The correct number of combinations is returned with 2 by.i variables", {

  expect_true( count_unique_id_combos(df.test, c("v1", "v2")) == 9 )

})

