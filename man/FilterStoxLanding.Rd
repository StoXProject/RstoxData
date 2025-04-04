% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Filter.R
\name{FilterStoxLanding}
\alias{FilterStoxLanding}
\title{Filter StoxLanding data}
\usage{
FilterStoxLanding(StoxLandingData, FilterExpression = list())
}
\arguments{
\item{StoxLandingData}{Input \code{\link{StoxLandingData}} data.}

\item{FilterExpression}{Filter expression given as a list of strings. The name of the list and structures should be identical to the names of the input data list. To extract or exclude missing values (NAs) use the operator \code{\%in\%} or the special operator \code{\%notin\%}, which is defined in RstoxData. For a numeric filter, in the current StoX GUI, NA must be added manually in the text field of the "Expression table dialog" (the "Query builder dialog" will only show numeric values and will show an empty fields if the filter contains NA). Possible values are provided in the filter expression builder in the StoX GUI for character values, integer values, and numeric values that are all whole numbers, but not for keys (CruiseKey, StationKey, HaulKey, SpeciesCategoryKey, SampleKey and IndividualKey in \code{\link{FilterStoxBiotic}}, and CruiseKey, LogKey, BeamKey, AcousticCategoryKey, ChannelReferenceKey and NASCKey in \code{\link{FilterStoxAcoustic}}).}
}
\value{
An object of filtered data in the same format as the input data.
}
\description{
Filters \code{\link{StoxLandingData}}.
}
