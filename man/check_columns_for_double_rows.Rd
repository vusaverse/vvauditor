% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_columns_for_double_rows.R
\name{check_columns_for_double_rows}
\alias{check_columns_for_double_rows}
\title{Check columns for double rows}
\usage{
check_columns_for_double_rows(
  data,
  group_vars = c("INS_Studentnummer", "INS_Opleidingsnaam_2002", "INS_Inschrijvingsjaar")
)
}
\arguments{
\item{data}{Data frame.}

\item{group_vars}{a vector containing the column names to group by.
In the scope of the vu student analytics project, these columns are:
c(INS_Studentnummer, INS_Opleidingsnaam_2002, INS_Inschrijvingsjaar)}
}
\value{
table with the results.
}
\description{
Check which columns lead to double rows in a data frame.
}
