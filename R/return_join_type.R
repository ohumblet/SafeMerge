#' @title Return the join type.
#'
#' @description Take as inputs the three 'all' variables, and determines what merge type was intended, i.e. one of "INNER", "FULL OUTER", "LEFT OUTER", "RIGHT OUTER".
#'
#' @param all   Identical to base merge function.
#' @param all.x Identical to base merge function.
#' @param all.y Identical to base merge function.
#'
#' @return Returns a character value equal to one of "INNER", "FULL OUTER", "LEFT OUTER", "RIGHT OUTER". Or an error if an unexpected combination of values is specified.
#'
#' @details Introduces a behavior that differs from that of base merge, i.e. that ir returns an error for combinations of 'all' values that are odd but legal. For example return_merge_type(all = "FALSE", all.x = "TRUE", all.y = TRUE) returns an error because it was probably unintentional, even though this seems to produce a functional merge (presumably a FULL OUTER merge)
#'
#' @keywords merge
#' @export
#' @examples
#' return_merge_type(all = FALSE, all.x = FALSE, all.y = FALSE)
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

# # Interactive testing
# return_join_type(all = FALSE, all.x= FALSE, all.y = FALSE) # "INNER"
# return_join_type(all = TRUE, all.x= TRUE, all.y = TRUE) # "FULL OUTER"
# return_join_type(all = FALSE, all.x= TRUE, all.y = FALSE) # "LEFT OUTER"
# return_join_type(all = FALSE, all.x= FALSE, all.y = TRUE) # "RIGHT OUTER"
# return_join_type(all = FALSE, all.x= TRUE, all.y = TRUE) # "A combination of 'all' variables was specified that is unexpected for SafeMerge."
