#' @title Return the join type.
#'
#' @description Take as inputs the three 'all' variables, and determines what merge type was intended, i.e. one of "INNER", "FULL OUTER", "LEFT OUTER", "RIGHT OUTER".
#'
#' @param all   Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y specifications of the columns used for merging (See Details), but differs from base function in that default is NULL, to prevent inadvertent merging without a by variable.
#'
#' @return Returns a character value equal to one of "INNER", "FULL OUTER", "LEFT OUTER", "RIGHT OUTER". Or an error if an unexpected combination of values is specified.
#'
#' @details Introduces a behavior that differs from that of base merge, i.e. that ir returns an error for combinations of 'all' values that are odd but legal. For example return_merge_type(all = "FALSE", all.x = "TRUE", all.y = FALSE) returns an error because it was probably unintentional, even though this seems to produce a functional merge (presumably a FULL OUTER merge)
#'
#' @keywords merge
#' @examples
#' return_merge_type(all = "FALSE", all.x = "FALSE", all.y = FALSE)
#'
#' @author Olivier Humblet

return_join_type <- function(all, all.x, all.y) {

  if( all %in% FALSE &
      all.x %in% FALSE &
      all.y %in% FALSE ) {
    merge_type <- "INNER"
  } else if ( all %in% TRUE &
              all.x %in% TRUE &
              all.y %in% TRUE ) {
            merge_type <- "FULL OUTER"
  } else if ( all %in% FALSE &
              all.x %in% TRUE &
              all.y %in% FALSE ) {
            merge_type <- "LEFT OUTER"
  } else if ( all %in% FALSE &
              all.x %in% FALSE &
              all.y %in% TRUE ) {
            merge_type <- "RIGHT OUTER"
  } else {stop("A combination of 'all' variables was specified that is unexpected for SafeMerge.")
  }

  return(merge_type)

}

