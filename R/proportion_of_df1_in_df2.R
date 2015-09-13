#' @title Assess overlap between data frames, for specified variables.
#'
#' @description Returns the proportion of the observations in one data frame that match a row in another data frame, for specified variables.
#'
#' @param df1 The data frame whose observations will be checked for their presence in the other data frame.
#' @param df2 The data frame that the observations in the other dta frame will be checked against.
#' @param by1 The by variable(s) for df1.
#' @param by2 The by variable(s) for df1.
#'
#' @return The proportion of the first data frame whose observations are present in the second data frame, for the specified variables.
#'
#' @details The by1 and by2 variable vectors must contain the corresponding variables in the same order.
#'
#' @keywords SafeMerge
#' @examples
#' df.1 <- data.frame(id = c(1, 1, 2, 3),
#' value1 = c("a", "a", "b", "c"),
#' value2 = c(1, 1, 2, NA))
#' df.1
#'
#' df.2 <- data.frame(id = 1:2,
#' value2 = c(1, 2))
#' df.2
#'
#' proportion_of_df1_in_df2(df.1, df.2, "id", "id")
#' proportion_of_df1_in_df2(df.2, df.1, "id", "id")
#'
#' @author Olivier Humblet

proportion_of_df1_in_df2 <- function(df1, df2, by1, by2) {

  # select just the by variables from df1
  df1 <- df1[,
             by1,
             drop=FALSE]
  # create a single vector by pasting the by variable values.
  byvars.paste.1 <- apply(df1, 1, paste, collapse="")

  # select just the by variables from df2
  df2 <- df2[,
             by2,
             drop=FALSE]
  # create a single vector by pasting the by variable values.
  byvars.paste.2 <- apply(df2, 1, paste, collapse="")

  return(length(byvars.paste.1[byvars.paste.1 %in% byvars.paste.2]) /
           length(byvars.paste.1) )

}
