#' @title Print information for every merge.
#'
#' @description Print information for every merge.
#'
#' @param all   Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y Identical to base merge function.
#'
#' @param x     Identical to base merge function.
#' @param y     Identical to base merge function.
#' @param by    Identical to base merge function.
#' @param by.x  Identical to base merge function.
#' @param by.y  Identical to base merge function.
#' @param all   Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y Identical to base merge function.
#'
#'
#' @return Print the appropriate information.
#'
#' @details None.
#'
#' @export
#'
#' @examples
#' print_always(x, y, by = "id") # not functional
#'
#' @author Olivier Humblet

print_always <- function(x, y, by, by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all, list_in = NULL) {

  # print the join type
  print_join_type(all = all,
                  all.x = all.x,
                  all.y = all.y)

  # print the by variables
  print_by_vars(by = by,
                by.x = by.x,
                by.y = by.y)

  # print the merge ratio (e.g. 1:1, 1:many...)
  print_merge_ratio(x = x,
                    y = y,
                    by.x = by.x,
                    by.y = by.y)

  # print fraction of the IDs merged from each input data frame
  print_fraction_merged(x = x, y = y, by.x = by.x, by.y = by.y, list_in = list_in)

}

# # Interactive testing
# d_x <- data.frame(id = 1, v1 = 1)
# d_y <- data.frame(id = 1, v2 = 2)
# print_always(d_x, d_y, by = "id")


#' @title Print join type.
#'
#' @description Print join type.
#'
#' @param all   Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y Identical to base merge function.
#'
#' @return Print the appropriate join type, along with associated information.
#'
#' @details Introduces a behavior that differs from that of base merge, i.e. that ir returns an error for combinations of 'all' values that are odd but legal. For example return_merge_type(all = "FALSE", all.x = "TRUE", all.y = TRUE) returns an error because it was probably unintentional, even though this seems to produce a functional merge (presumably a FULL OUTER merge)
#'
#' @export
#'
#' @examples
#' print_join_type(all = FALSE, all.x = FALSE, all.y = FALSE)
#'
#' @author Olivier Humblet

print_join_type <- function(all = NULL, all.x = NULL, all.y = NULL) {

  join_type <- return_join_type(all = all,
                                all.x = all.x,
                                all.y = all.y)

  if( join_type == "INNER" ) {
    cat(paste(join_type,
                "merge. All unmatched observations will be lost.\n"))
  } else if( join_type == "FULL OUTER") {
    cat(paste(join_type,
                "merge. All observations will be kept.\n"))
  } else if( join_type == "LEFT OUTER") {
    cat(paste(join_type,
                "merge. Observations not in x will be lost.\n"))
  } else if( join_type == "RIGHT OUTER") {
    cat(paste(join_type,
                "merge. Observations not in y will be lost.\n"))
  } else {stop("Unexpected join type returned.")}

}

# Interactive testing

# print_join_type(all = FALSE, all.x= FALSE, all.y = FALSE) # "INNER"
# print_join_type(all = TRUE, all.x= TRUE, all.y = TRUE) # "FULL OUTER"
# print_join_type(all = FALSE, all.x= TRUE, all.y = FALSE) # "LEFT OUTER"
# print_join_type(all = FALSE, all.x= FALSE, all.y = TRUE) # "RIGHT OUTER"
# print_join_type(all = FALSE, all.x= TRUE, all.y = TRUE) # Error "A combination of 'all' variables was specified that is unexpected for SafeMerge."





#' @title Print the ratio of (obs/unique id) in each data frame.
#'
#' @description Print the ratio, for each input data frame, of the quotient of the number of observations divided by the number of unique by-variable-combinations.
#'
#' @param x Typically the merged portion of the final merged dataset that came from x. (We only care about the merged portion.)
#' @param y Typically the merged portion of the final merged dataset that came from y. (We only care about the merged portion.)
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#'
#' @return None, output is printed (or throws an error), specifically whether the merge is 1:1, 1:many, many:1, many:many (throw error), or something unexpected (throw error).
#'
#' @details None.
#'
#' @examples
#' d1 <- data.frame(id = 1, v1 = 1)
#' d2 <- data.frame(id = rep(1, 2), v2 = 2:3)
#' print_merge_ratio(d1, d2, "id", "id")
#'
#' @author Olivier Humblet


print_merge_ratio <- function(x, y, by.x, by.y) {

  if( !(all(by.x %in% names(x))) | !(all(by.y %in% names(y))) ) {
    stop("by.x and by.y must be variables in x and y, respectively.")
  }

  list_ratios <- return_list_ratios_xy(x = x,
                                       y = y,
                                       by.x = by.x,
                                       by.y = by.y)

  if(list_ratios[["ratio_in_x"]] == 1 && list_ratios[["ratio_in_y"]] == 1) {
    cat("1:1 merge.\n")
  } else if(list_ratios[["ratio_in_x"]] == 1 && list_ratios[["ratio_in_y"]] > 1) {
    cat("1:many merge.\n")
  } else if(list_ratios[["ratio_in_x"]] > 1 && list_ratios[["ratio_in_y"]] == 1) {
    cat("many:1 merge.\n")
  } else if(list_ratios[["ratio_in_x"]] > 1 && list_ratios[["ratio_in_y"]] > 1) {
    stop("many:many merge.")
  } else stop("Unexpected merge type.")

}



#' @title Print fraction merged.
#'
#' @description Prints the fraction of ids that are merged into the final data frame from each of the nput data frames.
#'
#' @param x Typically the merged portion of the final merged dataset that came from x. (We only care about the merged portion.)
#' @param y Typically the merged portion of the final merged dataset that came from y. (We only care about the merged portion.)
#' @param by.x From base::merge.
#' @param by.y From base::merge.
#' @param list_in A list containing 3 named data frames:  merged is the data frame that resulted from merging x and y, merged_from_x is the rows of merged that came from x, and merged_from_y is the rows of merged that came from y.
#'
#' @return Prints information on the number and percent of IDs that are merged from each input data frame.
#'
#' @details None.
#'
#' @examples
#' print_fraction_merged <- function(x=x, y=y, by.x=by.x, by.y=by.y, list_in = list_in) # not functional
#'
#' @author Olivier Humblet

print_fraction_merged <- function(x,
                                  y,
                                  by.x,
                                  by.y,
                                  list_in = NULL) {

  if( !(all(by.x %in% names(x))) | !(all(by.y %in% names(y))) ) {
    stop("by.x and by.y must be variables in x and y, respectively.")
  }

  if( !(all(by.x %in% names(list_in[["merged_from_x"]]))) | !(all(by.x %in% names(list_in[["merged_from_y"]]))) ) {
    stop("by.x must be named a variable(s) in the merged subsets of x and y, respectively.")
  }

  # Count the unique ids merged from each input data frame
  n_merged_ids_from_x <- count_unique_id_combos(list_in[["merged_from_x"]],
                                                by.x)
  n_total_ids_from_x <- count_unique_id_combos(x,
                                               by.x)
  pct_merged_ids_from_x <- 100*round(n_merged_ids_from_x / n_total_ids_from_x,
                                     digits = 2)

  n_merged_ids_from_y <- count_unique_id_combos(list_in[["merged_from_y"]],
                                                by.x) # by.x is used because this is a subset of the merged data frame, where by.y has been renamed.
  n_total_ids_from_y <- count_unique_id_combos(y,
                                               by.y)
  pct_merged_ids_from_y <- 100*round(n_merged_ids_from_y / n_total_ids_from_y,
                                     digits = 2)

  # Print the text:
  # Example: "X: 34 of 37 (92%) individual ids were merged."

  cat(paste0("X: ",
             n_merged_ids_from_x,
             " of ",
             n_total_ids_from_x,
             " (",
             pct_merged_ids_from_x,
             "%) individual ids were merged.\n"))

  cat(paste0("Y: ",
             n_merged_ids_from_y,
             " of ",
             n_total_ids_from_y,
             " (",
             pct_merged_ids_from_y,
             "%) individual ids were merged.\n"))

}

# # Interactive testing
# (dBP <- data.frame(id = c("1abc", "2efg", "3hij"),
#                   bp = c("hi", "lo", "hi")))
# (dStress <- data.frame(uid = c("1abc", "2efg"),
#                       stress = c("yes", "no")))
# (df_list <- return_list_merged_dataframes(dBP,
#                                         dStress,
#                                         by.x = "id",
#                                         by.y = "uid"))
# print_fraction_merged(x = dBP,
#                       y = dStress,
#                       by.x = "id",
#                       by.y = "uid",
#                       list_in = df_list)
# (df_list <- return_list_merged_dataframes(dBP,
#                                         dStress,
#                                         all.x = TRUE,
#                                         by.x = "id",
#                                         by.y = "uid"))
# print_fraction_merged(x = dBP,
#                       y = dStress,
#                       by.x = "id",
#                       by.y = "uid",
#                       list_in = df_list)





#' @title Print the by variables.
#'
#' @description Print the by variables.
#'
#' @param by The 'by' input to the merge function.
#' @param by.x The 'by.x' input to the merge function.
#' @param by.y The 'by.y' input to the merge function.
#'
#' @return None, output is printed, specifically what the by variables are.
#'
#' @details Assumes it cannot be passed null values for all arguments, no check for this is done.
#'
#' @examples
#' print_by_vars(NULL, c("v1.x", "v2.x"), c("v1.y", "v2.y"))
#'
#' @author Olivier Humblet


print_by_vars <- function(by, by.x, by.y) {

  if( !is.null(by) ) {
    cat(paste0("The by variables are: ", paste(by, collapse = " "), ".\n"))
  } else if( !is.null(by.x) && !is.null(by.y)) {
    cat(paste0("The by variables in x are: ", paste(by.x, collapse = " "), ".\n",
              "The by variables in y are: ", paste(by.y, collapse = " "), ".\n"))
  }

}

# # interactive testing
#
# debugonce(print_by_vars)
# print_by_vars(c("v1", "v2"), NULL, NULL)
# print_by_vars(NULL, c("v1.x", "v2.x"), c("v1.y", "v2.y"))



