##### Introduction ####
#
#  Unit testing for the function "Add_corrected_role_variables"
#
#  Inputs:
#     "Add_corrected_role_variables", a function stored in OH_functions
#
#  Output:
#     1) N/A
#
#  Author: Olivier Humblet
#
#-----------------------------------------------------------------#


# Source the file where the function is saved (with working directory set to that file's directory)

source("OH_functions.R")

# Testing

context("Test Add_corrected_role_variables")


test_that("Specifying an object via ... that is not a data frame will draw an error.", {

  Not.a.dataframe <- NULL
  expect_error( Add_corrected_role_variables("fake.path", Not.a.dataframe), "An additional object was specified, but it is not a data frame." )

})


# test_that("Multiple NA values are correctly imputed if not in first position", {
#
#   # Create input data frame for testing
#   df.input <- data.frame(uid = c(1,1,1,1), v2 = c(1,NA,2,NA))
#   # Create output data frame for testing
#   df.output <- Carry_forward_last_value(df.input, "v2")
#
#   expect_equal(df.output$v2_lvcf, c(1,1,2,2))
#
# })
#
#
# test_that("An initial NA is left unchanged, but a later one is filled-in", {
#
#   # Create input data frame for testing
#   df.input <- data.frame(uid = c(1,1,1,1), v2 = c(NA,1,2,NA))
#   # Create output data frame for testing
#   df.output <- Carry_forward_last_value(df.input, "v2")
#
#   expect_equal(df.output$v2_lvcf, c(NA,1,2,2))
#
# })
#
#
# test_that("Single and multiple NAs are filled-in, and initial NAs are not over-written, even with multiple UIDs", {
#
#   # Create input data frame for testing
#   df.input <- data.frame(uid = c(1,1,1,1,2,2,3,3), v1 = c(1,NA,2,NA,3,NA,NA,1))
#   # Create output data frame for testing
#   df.output <- Carry_forward_last_value(df.input, "v1", "uid")
#
#   expect_equal(df.output$v1_lvcf, c(1,1,2,2,3,3,NA,1))
#
# })
