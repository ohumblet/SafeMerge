
#' @title Returns a list of the ratios of observations to rows in x and y.
#'
#' @description Returns a list of the ratios of observations to rows in both x and y..
#'
#' @param x Typically the merged portion of the final merged dataset that came from x. (We only care about the merged portion.)
#' @param y Typically the merged portion of the final merged dataset that came from y. (We only care about the merged portion.)
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#'
#' @return Returns a list whose two entries are the ratios of observations to unique by-variable combinations, for the two datasets (i.e. ratio_in_x and ratio_in_y, respectively).
#'
#' @details None.
#'
#' @examples
#' d1 <- data.frame(id = rep(1:2, times = 2:1), v1 = 1:3)
#' d2 <- data.frame(id = rep(1:2, times = 2:1), v2 = 4:6)
#' return_list_ratios_xy(d1, d2, "id", "id")
#'
#' @author Olivier Humblet


return_list_ratios_xy <- function(x, y, by.x, by.y) {

  ratio_in_x <- nrow(x) / count_unique_id_combos(x, by.x)
  ratio_in_y <- nrow(y) / count_unique_id_combos(y, by.y)

  list_ratios <- list(ratio_in_x = ratio_in_x,
                      ratio_in_y = ratio_in_y)

  return(list_ratios)

}




#' @title Count data frame rows with unique combinations of the specified variables.
#'
#' @description Count data frame rows with unique combinations of the specified variables.
#'
#' @param df The input data frame.
#' @param by.i A character vector of the names of by variables for that data frame. For example, by.x if the specified data frame is 'x' and by.x  is specified.
#'
#' @return A single number representing the count of unique rows.
#'
#' @details None.
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' df.test <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
#' v2 = rep(c(1,2,3), 3),
#' v3 = runif(9))
#' df.test
#'
#' count_unique_id_combos(df.test, "v1")
#'
#' @author Olivier Humblet

count_unique_id_combos <- function(df, by.i = NULL) {

  if(is.null(by.i)) {stop("One or more by.i variables must be specified.")}

  if(!all(by.i %in% names(df))) {stop("by.i must specify variable names that are all part of the input data frame.")}

  # collapse each data frame into unique combinations of the by variables, then count the rows and return the number.
  unique(df[,
            by.i, # the by variables for this data frame
            drop = FALSE] # need drop = FALSE otherwise will become a vector when there is only one by variable.
  ) %>% nrow %>%
    return

}
