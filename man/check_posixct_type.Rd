% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_posixct_type.R
\name{check_posixct_type}
\alias{check_posixct_type}
\title{Check for POSIXct Type}
\usage{
check_posixct_type(column_name, dataframe, column_prefix = NULL, ...)
}
\arguments{
\item{column_name}{A character vector or string with the column name to be tested.}

\item{dataframe}{The dataframe that contains the column.}

\item{column_prefix}{Default is NULL. If provided, this text is prepended to the variable name in the assertion message.}

\item{...}{The remaining parameters are passed to the function assert_posixct.}
}
\description{
This function checks if the specified column in the provided dataframe has a POSIXct type.
It uses the checkmate::assert_posixct function to perform the assertion.
}
\examples{
# Create a dataframe with a POSIXct column
dataframe <- data.frame(date = as.POSIXct("2023-10-04"))
# Check the POSIXct type of the 'date' column
check_posixct_type("date", dataframe)
}
\seealso{
Other assertions: 
\code{\link{check_no_duplicates_in_group}()},
\code{\link{check_numeric_or_integer_type}()}

Other tests: 
\code{\link{check_double_columns}()},
\code{\link{check_no_duplicates_in_group}()},
\code{\link{check_numeric_or_integer_type}()},
\code{\link{duplicates_in_column}()},
\code{\link{test_all_equal}()}
}
\concept{assertions}
\concept{tests}
