#' @title Safer merging of two data frames.
#'
#' @description A wrapper for the merge function, for interactive use. Prints information and warnings about the merge, then conducts the merge in question (with identical functionality, except as described in 'Details'.
#'
#' @param x Identical to base merge function.
#' @param y Identical to base merge function.
#' @param by specifications of the columns used for merging (See Details), but differs from base function in that default is NULL, to prevent inadvertent merging without a by variable.
#' @param by.x Identical to base merge function.
#' @param by.y Identical to base merge function.
#' @param all Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y Identical to base merge function.
#' @param sort Identical to base merge function.
#' @param suffixes Identical to base merge function.
#' @param incomparables Identical to base merge function.
#' @param verbose Logical; default = FALSE; if TRUE then more diagnostic information is printed (under development).
#'
#' @return Returns the data frame resulting from the merge of the two input data frames.  The functionality is mostly identical to 'merge', except as described in 'Details'.
#'
#' @details The functionality is mostly identical to 'merge', except that the by variable is set to NULL by default, and there is an additional parameter 'verbose'). The effect of the by variable being set to NULL as a default means that failure to specify a by variable will result in an error. This function will not seek to guess which variables you mean to merge on, i.e. by checking which variables are present in both data frames. The by variable must be specified. \cr
#' Assumes that either a 'by' variable is specified, or that both 'by.x' and 'by.y' are specified. May not handle the edge case where 'by' and one of 'by.x' or 'by.y' are provided.
#'
#' @importFrom dplyr "%>%"
#' @export
#' @examples
#' x <- data.frame(id = 1:10, xval = 10:1)
#' y <- data.frame(id = 1:5, yval = 15:11)
#' SafeMerge(x, y, by = "id")
#'
#' @author Olivier Humblet

SafeMerge <- function(x, y, by = NULL,
      by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"),
      incomparables = NULL, verbose = FALSE, ...) {

  # Generate list of merged dataframes: merged, merged_from_x, and merged_from_y.
  df_list <-
    return_list_merged_dataframes(x = x, y = y,
                                  by = by, by.x = by.x, by.y = by.y,
                                  all = all, all.x = all.x, all.y = all.y,
                                  sort = sort, suffixes = suffixes,
                                  incomparables = incomparables)

  # Generate errors
  generate_errors(by = by,
                  by.x = by.x,
                  by.y = by.y,
                  list_in = df_list)

  # Generate warnings
  generate_warnings(x = x,
                    y = y,
                    by.x = by.x,
                    by.y = by.y,
                    suffixes = suffixes)

  # Print output that we always want to see
  print_always(x = x,
               y = y,
               by = by,
               by.x = by.x,
               by.y = by.y,
               list_in = df_list)

  # Print additional output only if verbose = TRUE
  if(verbose == TRUE) {
    print_verbose(x = x,
                  y = y,
                  list_in = df_list)
  }

  # remove extraneous variables from the merged data frame
  dOut <- df_list[["merged"]]

  # return the merged data frame
  return(dOut)

}

#
#
#
# df.x <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
#                    v2 = rep(c(1,2,3), 3),
#                    v3 = runif(9)
# )
# df.x
#
# df.y <- data.frame(id = c(1,2,3,4),
#                    v4 = runif(4)
# )
# df.y
#
# df.SM <- SafeMerge(df.x,
#                    df.y,
#                    by.x = "v1",
#                    by.y = "id")
# df.SM
#
# ## many to many merge
#
# df.x <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
#                    v2 = rep(c(1,2,3), 3),
#                    v3 = runif(9)
# )
# df.x
#
# df.y <- data.frame(id = rep(c(1,2,3), c(2,2,2)),
#                    v4 = runif(6)
# )
# df.y
#
# df.SM <- SafeMerge(df.x,
#                    df.y,
#                    by.x = "v1",
#                    by.y = "id")
# df.SM
#
# ## does by variable order matter?
#
# df.x <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
#                    v2 = rep(c(1,2,3), 3),
#                    v3 = runif(9)
# )
# df.x
#
# df.y <- data.frame(id = rep(c(1,2,3), c(2,2,2)),
#                    v2 = rep(c(1,2), 3),
#                    v4 = runif(6)
# )
# df.y
#
# df.SM <- SafeMerge(df.x,
#                    df.y,
#                    by.x = c("v1", "v2"),
#                    by.y = c("id", "v2"))
# df.SM
#
# df.SM <- SafeMerge(df.x,
#                    df.y,
#                    by.x = c("v2", "v1"),
#                    by.y = c("id", "v2"))
# df.SM
#
#
