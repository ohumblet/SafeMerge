##### Introduction ####
#
#  Unit testing for the function "SafeMerge"
#
#  Author: Olivier Humblet
#
#-----------------------------------------------------------------#


# Testing

context("Test SafeMerge")



test_that("It throws an error if no by variables are provided", {

  expect_error( SafeMerge(mtcars, mtcars) )

})
