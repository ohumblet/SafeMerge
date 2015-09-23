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

print_always <- function(x, y, by, by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all) {

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


print_fraction_merged <- function() {
  # FRAGMENT

        # Print fraction merged (make into separate function?)

unique_by_merge_from_x <- count_unique_id_combos(subset(df.merge,
                                                        in.x == TRUE),
                                                 by.vars.in.merge)
unique_by_merge_from_y <- count_unique_id_combos(subset(df.merge,
                                                        in.y == TRUE),
                                                 by.vars.in.merge)

        paste("The percent of unique ID combos in the final merged data frame from x = ",
              round(100*unique_by_merge_from_x / unique_by_x),
              "%",
              ", and from y = ",
              round(100*unique_by_merge_from_x / unique_by_y),
              "%.",
              "\n", sep = "") %>% cat
        paste("Observations in x matching observations in y: ",
              round(100*proportion_of_df1_in_df2(x, y, by.vars.in.x, by.vars.in.y)),
              "%.",
              "\n", sep = "") %>% cat
        paste("Observations in y matching observations in x: ",
              round(100*proportion_of_df1_in_df2(y, x, by.vars.in.y, by.vars.in.x)),
              "%.",
              "\n", sep = "") %>% cat

}

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
