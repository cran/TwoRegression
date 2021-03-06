% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Sensor_Functions.R
\name{get_VM}
\alias{get_VM}
\title{Calculate vector magnitude}
\usage{
get_VM(triaxial, verbose = FALSE)
}
\arguments{
\item{triaxial}{a dataframe of triaxial data on which to calculate vector magnitude}

\item{verbose}{print information about variable search criteria?}
}
\value{
a vector of vector magnitude values
}
\description{
Calculate vector magnitude
}
\examples{
\dontrun{
data(imu_to_collapse)

vm_columns <-
    grepl("accelerometer",
        names(imu),
        ignore.case = T)

TwoRegression:::get_VM(imu[, vm_columns])
}

}
\keyword{internal}
