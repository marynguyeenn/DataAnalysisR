% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DM2DD_function.R
\name{update_DD_date}
\alias{update_DD_date}
\title{Updating to Decimal Degrees and \%Y-\%m-\%d Date format}
\usage{
update_DD_date(df, Longitude, Latitude, Date)
}
\arguments{
\item{df}{The name of the specified data frame}

\item{Longitude}{Longitude in decimal minute format}

\item{Latitude}{Latitude in decimal minute format}

\item{Date}{Date in "\%d-\%b-\%y" format}
}
\description{
This function takes the input of a specified data frame and it's respective Longitude, Latitude, and Date values. It changes Decimal Minute Longitude and Latitude values to Decimal Degrees. It also changes dates in the \%d-\%b-\%y format to \%Y-\%m-\%d.
}
