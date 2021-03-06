% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/proportion_of_df1_in_df2.R
\name{proportion_of_df1_in_df2}
\alias{proportion_of_df1_in_df2}
\title{Assess overlap between data frames, for specified variables.}
\usage{
proportion_of_df1_in_df2(df1, df2, by1, by2)
}
\arguments{
\item{df1}{The data frame whose observations will be checked for their presence in the other data frame.}

\item{df2}{The data frame that the observations in the other dta frame will be checked against.}

\item{by1}{The by variable(s) for df1.}

\item{by2}{The by variable(s) for df1.}
}
\value{
The proportion of the first data frame whose observations are present in the second data frame, for the specified variables.
}
\description{
Returns the proportion of the observations in one data frame that match a row in another data frame, for specified variables.
}
\details{
The by1 and by2 variable vectors must contain the corresponding variables in the same order.
}
\examples{
df.1 <- data.frame(id = c(1, 1, 2, 3),
value1 = c("a", "a", "b", "c"),
value2 = c(1, 1, 2, NA))
df.1

df.2 <- data.frame(id = 1:2,
value2 = c(1, 2))
df.2

proportion_of_df1_in_df2(df.1, df.2, "id", "id")
proportion_of_df1_in_df2(df.2, df.1, "id", "id")
}
\author{
Olivier Humblet
}
\keyword{SafeMerge}

