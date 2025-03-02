% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_data_types.R
\name{create_data_types}
\alias{create_data_types}
\title{Create data types tibble}
\usage{
create_data_types(data, mapping)
}
\arguments{
\item{data}{A dataframe for which to create a data types csv.}

\item{mapping}{A dataframe containing a mapping named vector, containing preferred fieldnames
Example:
column_names <- c(
mpg = "mpg", cyl = "cyl", disp = "disp", hp = "hp",
drat = "drat", wt = "wt", qsec = "qsec", vs = "vs",
am = "am", gear = "gear", carb = "carb", spare_tire = "spare_tire"
)}
}
\value{
Tibble containing data_types
}
\description{
This function returns a data types tibble. Containing type information about the dataset.
}
