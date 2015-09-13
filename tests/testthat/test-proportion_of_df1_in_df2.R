##### Introduction ####
#
#  Unit testing for the function "proportion_of_df1_in_df2"
#
#  Inputs:
#     "proportion_of_df1_in_df2", a function in the SafeMerge package
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

context("Test proportion_of_df1_in_df2")

  df.x <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
                     v2 = rep(c(1,2,3), 3),
                     v3 = runif(9)
  )
#   df.x

  df.y <- data.frame(id = c(1,2,3,4),
                     v4 = runif(4)
  )
#   df.y


test_that("The appropriate proportions are calculated.", {

  expect_true( proportion_of_df1_in_df2(df.x, df.y, "v1", "id") == 1 )

  expect_true( proportion_of_df1_in_df2(df.y, df.x, "id", "v1") == 0.75 )

})


