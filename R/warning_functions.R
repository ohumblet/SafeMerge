
#' @title Generate warnings.
#'
#' @description Generate warnings.
#'
#' @param x Same as base merge.
#' @param y Same as base merge.
#' @param by.x Same as base merge.
#' @param by.y Same as base merge.
#' @param suffixes Same as base merge.
#'
#' @return N/A. Generate warnings.
#'
#' @details None.
#'
#' @examples
#' generate_warnings(x, y, by.x, by.y, suffixes) # Not functional
#'
#' @author Olivier Humblet

generate_warnings <- function(x, y, by.x, by.y, suffixes) {

  # warning if variable names (aside from by vars) overlap and some will be renamed
  variable_names_overlap(x = x,
                         y = y,
                         by.x = by.x,
                         by.y = by.y,
                         suffixes = suffixes)

  # if by.x and by.y are used, print a warning that the by.y varname has been replaced

  by_variables_renamed(by.x = by.x, by.y = by.y)

}




#' @title Warning if same variable name(s) present in both data frames.
#'
#' @description Warning if same variable name(s) present in both data frames.
#'
#' @param x Same as base merge.
#' @param y Same as base merge.
#' @param by.x Same as base merge.
#' @param by.y Same as base merge.
#' @param suffixes Same as base merge.
#'
#' @return N/A. Warning if same variable name(s) present in both data frames.
#'
#' @details None.
#'
#' @examples
#' x <- data.frame(uid = 1, v_overlap = 1)
#' y <- data.frame(uid = 1, v_overlap = 1)
#' by.x <- "uid"
#' by.y <- "uid"
#' suffixes = c(".x",".y")
#' paste(suffixes, collapse = " ")
#' variable_names_overlap(x, y, by.x, by.x, suffixes)
#'
#' @author Olivier Humblet

variable_names_overlap <- function(x, y, by.x, by.y, suffixes) {

  non_by_names_x <- setdiff(names(x),
                            by.x)
  non_by_names_y <- setdiff(names(y),
                            by.y)

  overlapping_names <- intersect(non_by_names_x,
                                 non_by_names_y)

  if( length(overlapping_names) > 0) {
    warning(paste0("The following non-by-variables are present in both data frames: ",
                overlapping_names,
                ". They will be renamed with the suffixes: ",
                paste(suffixes, collapse = " ")))
  } else {invisible("no action taken if there is no overlap.")}

}

# # interactive testing
# x <- data.frame(uid = 1,
#                 v_overlap = 1)
# y <- data.frame(uid = 1,
#                 v_overlap = 1)
# by.x <- "uid"
# by.y <- "uid"
# suffixes = c(".x",".y")
# paste(suffixes, collapse = " ")
#
# variable_names_overlap(x, y, by.x, by.x, suffixes)
# variable_names_overlap(x, y[,"uid"], by.x, by.x, suffixes)




#' @title Warning if by.y variables are renamed.
#'
#' @description Warning if by.y variables are renamed due to by.x and by.y being specified.
#'
#' @param by.x Same as base merge.
#' @param by.y Same as base merge.
#'
#' @return N/A. Warning generated if by.y variables are renamed due to by.x and by.y being specified.
#'
#' @details None.
#'
#' @examples
#' by_variables_renamed("v1", "v2")
#'
#' @author Olivier Humblet

by_variables_renamed <- function(by.x, by.y) {

  if( !identical(by.x, by.y) ) {

    warning(paste("by.x and by.y both specified. by.y variable names are overwritten, from",
                by.y,
                "to",
                by.x))

  } else {invisible("No action taken.")}

}


# #  interactive testing
#   by_variables_renamed("v1", "v2")
