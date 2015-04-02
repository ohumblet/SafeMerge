##### Introduction ####
#
#  Unit testing for the function "return_by_variables"
#
#  Inputs:
#     "return_by_variables", a function in the SafeMerge package
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

context("Test return_by_variables")


test_that("The appropriate by variables are returned.", {

  expect_true( return_by_variables("by", "by.x") == "by" )

  expect_true( return_by_variables(NULL, "by.x") == "by.x" )

  expect_error( return_by_variables(NULL, NULL),
                "Both by and by.i were NULL." )

})


