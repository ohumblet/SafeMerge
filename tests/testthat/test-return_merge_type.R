##### Introduction ####
#
#  Unit testing for the function "return_merge_type"
#
#  Inputs:
#     "return_merge_type", a function in the SafeMerge package
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

context("Test return_merge_type")


test_that("The appropriate merge types are returned.", {

  expect_true( return_merge_type(all = FALSE,
                                 all.x = FALSE,
                                 all.y = FALSE) == "INNER" )

  expect_true( return_merge_type(all = TRUE,
                                 all.x = TRUE,
                                 all.y = TRUE) == "FULL OUTER" )

  expect_true( return_merge_type(all = FALSE,
                                 all.x = TRUE,
                                 all.y = FALSE) == "LEFT OUTER" )

  expect_true( return_merge_type(all = FALSE,
                                 all.x = FALSE,
                                 all.y = TRUE) == "RIGHT OUTER" )

  expect_error( return_merge_type(all = FALSE,
                                   all.x = TRUE,
                                   all.y = TRUE),
                "A combination of 'all' variables was specified that is unexpected for SafeMerge." )

})


