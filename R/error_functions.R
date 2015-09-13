
#' @title Generate errors.
#'
#' @description Generate errors.
#'
#' @param by The 'by' input to the merge function.
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#' @param list_in A list containing 3 named data frames:  merged is the data frame that resulted from merging x and y, merged_from_x is the rows of merged that came from x, and merged_from_y is the rows of merged that came from y.
#'
#' @return None, generates errors.
#'
#' @details Generates errors for the following conditions: no by variables are provided.
#'
#' @examples
#' generate_errors(NULL, NULL, NULL, list())
#'
#' @author Olivier Humblet



generate_errors <- function(by,
                            by.x,
                            by.y,
                            list_in) {

  stop_if_no_by_vars(by = by,
                     by.x = by.x,
                     by.y = by.y)


  stop_if_many_to_many(x = list_in[["merged_from_x"]],
                       y = list_in[["merged_from_y"]],
                       by.x = by.x,
                       by.y = by.y)

}




#' @title Generate error if no by variables were provided.
#'
#' @description Generates error if no by variables were provided.
#'
#' @param by The 'by' input to the merge function.
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#'
#' @return None, throws error.
#'
#' @details None.
#'
#' @examples
#' stop_if_no_by_vars(NULL, NULL, NULL)
#'
#' @author Olivier Humblet

stop_if_no_by_vars <- function(by, by.x, by.y) {

  if(is.null(by) && is.null(by.x) && is.null(by.y)) {
    stop("No by variables have been provided.")
  }

}




#' @title Generate error if a many-to-many merge is detected.
#'
#' @description Generates error if both input data frames have >1 observation for any unique combination of by variable(s).
#'
#' @param x Typically the merged portion of the final merged dataset that came from x. (We only care about the merged portion.)
#' @param y Typically the merged portion of the final merged dataset that came from y. (We only care about the merged portion.)
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#'
#' @return None, throws error.
#'
#' @details A many-to-many merge will cause R to generated all combinations of the non-uniquely-matched rows. This is almost never the intended behavior, so it is not allowed by SafeMerge.
#'
#' @examples
#' d1 <- data.frame(id = rep(1:2, times = 2:1), v1 = 1:3)
#' d2 <- data.frame(id = rep(1:2, times = 2:1), v2 = 4:6)
#' stop_if_many_to_many(d1, d2, "id", "id")
#'
#' @author Olivier Humblet

stop_if_many_to_many <- function(x, y, by.x, by.y) {

  if( !(all(by.x %in% names(x))) | !(all(by.y %in% names(y))) ) {
    stop("by.x and by.y must be variables in x and y, respectively.")
  }

  list_ratios <- return_list_ratios_xy(x = x,
                                       y = y,
                                       by.x = by.x,
                                       by.y = by.y)

  if(list_ratios[["ratio_in_x"]] > 1 && list_ratios[["ratio_in_y"]] > 1) {
    stop("This is a many-to-many-merge. By variables with more levels must be specified, so as to uniquely identify all observations in at least one of the data frames.")
  }

}
