return_list_merged_dataframes <- function(x, y,
                                          by = NULL,
                                          by.x = by,
                                          by.y = by,
                                          all = FALSE,
                                          all.x = all,
                                          all.y = all,
                                          sort = TRUE,
                                          suffixes = c(".x",".y"),
                                          incomparables = NULL)
{

  # create empty list to store data frames

  list_out <- list()

  # Do the merge, after adding "in.x" and "in.y" variables, then add the merged data frame to the list

  x <- add_in.x(x)
  y <- add_in.y(y)

  dMerge <- merge(x = x,
                  y = y,
                  by = by,
                  by.x = by.x,
                  by.y = by.y,
                  all = all,
                  all.x = all.x,
                  all.y = all.y,
                  sort = sort,
                  suffixes = suffixes,
                  incomparables = incomparables)

  # Create a subset of the merged data frame that came from x

  dX_merged <- return_merged_subset(dIn = dMerge,
                                    "in.x")
#   dX_merged <- remove_new_variables(dX_merged, x, x)
#
  list_out[["merged_from_x"]] <- dX_merged

  # Create a subset of the merged data frame that came from y

  dY_merged <- return_merged_subset(dIn = dMerge,
                                    "in.y")
#   dY_merged <- remove_new_variables(dY_merged, y, y)

  list_out[["merged_from_y"]] <- dY_merged

  # Add the merged data frame to the list
  dMerge <- remove_new_variables(dMerge)
  list_out[["merged"]] <- dMerge

  return(list_out)

}


# # # Interactive testing
# dX <- data.frame(uid = 1, vx = 1)
# dY <- data.frame(uid = 1, vy = 1)
# debugonce(return_list_merged_dataframes)
# debugonce(return_merged_subset)
# return_list_merged_dataframes(dX, dY, by = "uid")


#' @title Add a constant (TRUE) variable 'in.x' to a data frame.
#'
#' @description Returns the input data frame after adding the TRUE variable in.x.
#'
#' @param df The input data frame.
#'
#' @return The input data frame with the new variable in.x added.
#'
#' @details Simple wrapper for add_named_constant.
#'
#' @examples
#' df.test <- data.frame(v1 = 1:3)
#' df.test
#' add_in.x(df.test)
#'
#' @author Olivier Humblet

add_in.x <- function(df) {

  add_named_constant(df, "in.x", TRUE)

}




#' @title Add a constant (TRUE) variable 'in.y' to a data frame.
#'
#' @description Returns the input data frame after adding the TRUE variable in.y.
#'
#' @param df The input data frame.
#'
#' @return The input data frame with the new variable in.y added.
#'
#' @details Simple wrapper for add_named_constant.
#'
#' @examples
#' df.test <- data.frame(v1 = 1:3)
#' df.test
#' add_in.y(df.test)
#'
#' @author Olivier Humblet

add_in.y <- function(df) {

  add_named_constant(df, "in.y", TRUE)

}




#' @title Add a constant variable to a data frame.
#'
#' @description Returns the input data frame after adding one (or several) new variable(s) with specified name(s) and a single specified value.
#'
#' @param df The input data frame.
#' @param new_varname The name(s) of the new variable(s): a character vector of names, each of which will be assigned to a new variable, all with the same value.
#' @param value The single value that will be taken by all the new variables. Can be char, logical, or numeric.
#'
#' @return The input data frame with the new variable(s) added.
#'
#' @details Does not check the type of the value variable.
#'
#' @examples
#' df.test <- data.frame(v1 = 1:3)
#' df.test
#' add_named_constant(df.test, "in.x", TRUE)
#'
#' @author Olivier Humblet


add_named_constant <- function(df, new_varname, value) {

  if(length(value)>1) stop("Unexpected behavior can occur if a the value input has length>0 (this function is designed to create constants!).")
  if(!is.character(new_varname) & length(new_varname == 1)) stop("new_varname must be a character vector of length 1")

  df[,new_varname] <- value

  return(df)

}





#' @title Return the subset of a merged data frame from the specified input data frame.
#'
#' @description Return the subset of a merged data frame from the specified input data frame, based on the presence of a boolean variable.
#'
#' @param dIn The input data frame. It is assumed that dIn was created by merging together two data drames. dIn must contain the boolean constant variable whose name is specified by varname. It is assumed that the varname variable was added to oe of the input data frames, with the value TRUE. Therefore all occurrences of a value TRUE in dIn indicate a row that came from the desired input data frame.
#' @param varname The name of the variable in dIn that is descibed above.
#'
#' @return Returns the subset of dIn for which the value of varname is TRUE. This should indicate the rows of dIn that were merged-in the desired input data frame. Also rrmoves the variable varname from the dataframe.
#'
#' @details None.
#'
#' @examples
#' d <- data.frame(id = 1:2, in.x = c(TRUE, NA), in.y = NA)
#' return_merged_subset(d, "from.x")
#'
#' @author Olivier Humblet

return_merged_subset <- function(dIn, varname) {

  stopifnot(varname %in% names(dIn))

  dOut <- dIn[dIn[[varname]] %in% TRUE, ] # return subset for which the varname variable is TRUE

  # remove the identifiers of the input dataframes, which now are no longer necessary.
  dOut <- remove_new_variables(dOut)

  return(dOut)

}

# # Interactive testing
# d <- data.frame(id = 1:2, in.x = c(TRUE, NA), in.y = NA)
# d
# return_merged_subset(d, "in.x")




#' @title Remove dataset identifiers.
#'
#' @description Remove the variables that were added in order to mark the input data frames.
#'
#' @param dIn The data frame from which the variables will be removed.
#'
#' @return Returns dIn, after removing the variables.
#'
#' @details Tryto find a more elegant solution than hard-coding the variable names.
#'
#' @examples
#' dIn <- data.frame(id = 1, in.x = TRUE, in.y = TRUE)
#' remove_new_variables(dIn)
#'
#' @author Olivier Humblet

remove_new_variables <- function(dIn) {

    dOut <- dplyr::select(dIn, -in.x, -in.y)

    return(dOut)

}

# # Interactive testing
# dIn <- data.frame(id = 1, in.x = TRUE, in.y = TRUE)
# remove_new_variables(dIn)




# #' @title Remove new  variables.
# #'
# #' @description Remove variables from the merged data frame that are ot present in either input data frame.
# #'
# #' @param d_merged The merged data frame.
# #' @param d_x The name of the x input data frame.
# #' @param d_y The name of the y input data frame.
# #'
# #' @return Returns d_merged, after removing all variables that weren't in either of the input data frames (which presumably must be all other convenience variables that were added, for example to identify which data frame each row came from).
# #'
# #' @details None.
# #'
# #' @examples
# #' d_merged <- data.frame(id = 1, v1 = 1, v2 = 2, v_new = NA)
# #' d_x <- data.frame(id = 1, v1 = 1)
# #' d_y <- data.frame(id = 1, v2 = 2)
# #' remove_new_variables(d_merged, d_x, d_y)
# #'
# #' @author Olivier Humblet
#
# remove_new_variables <- function(d_merged, d_x, d_y) {
#
#   # identify variables that weren't in either input data frame
#   vars_to_remove <- setdiff(names(d_merged),
#                             c(names(d_x),
#                               names(d_y)))
#
#   if( length(vars_to_remove) > 1) {
#     d_out <- dplyr::select_(d_merged,
#                             .dots = paste0("-", vars_to_remove))
#
#     return(d_out)
#
#   } else return(d_merged)
#
# }
#
# # # Interactive testing
# # d_merged <- data.frame(id = 1, v1 = 1, v2 = 2, v_new1 = NA, v_new2 = NA)
# # d_x <- data.frame(id = 1, v1 = 1)
# # d_y <- data.frame(id = 1, v2 = 2)
# # remove_new_variables(d_merged, d_x, d_y)
