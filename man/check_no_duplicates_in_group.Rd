% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_no_duplicates_in_group.R
\name{check_no_duplicates_in_group}
\alias{check_no_duplicates_in_group}
\title{Check for No Duplicates in Group}
\usage{
check_no_duplicates_in_group(
  dataframe,
  group_variables = NULL,
  assertion_fail = "stop"
)
}
\arguments{
\item{dataframe}{The dataframe to be checked.}

\item{group_variables}{The group variables as a character vector. The default is NULL.}

\item{assertion_fail}{How the function reacts to a failure. This can be a "warning", where only a warning is given on the failure,
or a "stop", where the function execution is stopped and the message is displayed, or an "AssertCollection", where the failure message is added to an assertion collection.}
}
\description{
This function checks if there is exactly one row per group in the provided dataframe.
If there are multiple rows per group, the assertion fails.
}
\examples{
# Create a dataframe with some groups having more than one row
dataframe <- data.frame(a = c(1, 1, 2), b = c(2, 2, 3), c = c("x", "x", "y"))
# Check the uniqueness of rows per group
check_no_duplicates_in_group(dataframe)
}
\seealso{
Other assertions: 
\code{\link{check_numeric_or_integer_type}()},
\code{\link{check_posixct_type}()}

Other tests: 
\code{\link{check_double_columns}()},
\code{\link{check_numeric_or_integer_type}()},
\code{\link{check_posixct_type}()},
\code{\link{duplicates_in_column}()},
\code{\link{test_all_equal}()}
}
\concept{assertions}
\concept{tests}
