#' @title Safer  merging of two data frames.
#'
#' @description A wrapper for the merge function, for interactive use. Prints information and warnings about the merge, then conducts the merge in question (with identical functionality, except that the by variable is set to NULL by default, and no dots parameters can be specified).
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
#' @param verbose Logical; if FALSE then less information is provided.
#'
#' @return Returns the data frame resulting from the merge of the two input data frames.  The functionality is mostly identical to 'merge', except that the by variable is set to NULL by default, no dots parameters can be specified, and there is an additional parameter verbose). See 'Details'.
#'
#' @details The effect of the by variable being set to NULL as a default means that failure to specify a by variable will result in an error. This function will not seek to guess which variables you mean to merge on, i.e. by checking which variables are present in both data frames. The by variable must be specified.
#' In addition the ability to add ... is gone, because I never use this option (and at this time I don't feel like writing the additional code needed to pass this to the merge function).
#'
#' @keywords merge
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
      incomparables = NULL, verbose = TRUE) {

  # Load libraries
  library(magrittr)

  # Stop if no by variables are provided

  if( is.null(by) &&  is.null(by.x) && is.null(by.y) ) stop("No by variables have been provided.")

  # Print important information.

  cat("I'm just a placeholder for future important information!\n")

## placeholders for future code
# # functionality:
# # report what type of join was requested (INNER, LEFT OUTER, RIGHT OUTER, FULL OUTER)
#
# # What do I want to know:
# # number of unique ID combos in each df
# # nrow in each df
# # did either one have multiple non-identical obs/id var? [if yes for both, we have a problem and the by vars should be changed]
# # what is the final # of IDs and observations?
# # what fraction of the observations were merged? For left and for right.
# # what % did not match and are partially blank?
# # what % did not match and therefore are excluded?
#
# # INNER: summary
# # report total row numbers of each, and distribution of IDs for each, and final row #s
# # LEFT OUTER: report row numbers of left, and distribution of IDs for the left and for the matching portion of the right, and final row #s
# # RIGHT OUTER
# # FULL OUTER
#
# merge.type <- return_type_of_merge(all, all.x, all.y)
#
# # Print values of by, by.x, by.y, variable(s), or throw warning if there are none
# # Or only if they're different from by?
#
# cat("By variables:\n")
# cat(paste("by =", by,
#       "| by.x =", by.x,
#       "| by.y =", by.y,
#       "\n"))




  # Do the merge.
  merge(x, y,
        by = by,
        by.x = by.x,
        by.y = by.y,
        all = all,
        all.x = all.x,
        all.y = all.y,
        sort = sort,
        suffixes = suffixes,
        incomparables = incomparables) %>% return

}


return_by_variables <- function(by, by.i) {

  if( !is.null(by) ) {
    return(by)
    } else if( !is.null(by.i) ) {
      return(by.i)
    } else stop("BOth by and by,i were NULL.")

}

# manual testing- to delete
return_by_variables("by", "by.x")
return_by_variables(NULL, "by.x")
return_by_variables(NULL, NULL)

# experimenting with unique and length
df.test <- data.frame(v1 = rep(c(1,2,3), c(3,3,3)),
                      v2 = rep(c(1,2,3), 3)
                      )

length(unique(df.test$v1))

unique(df.test)

?unique
?duplicated

?unique
