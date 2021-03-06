% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{StoxLandingData}
\alias{StoxLandingData}
\title{StoxLandingData}
\description{
Contains a list with one element 'Landing', described below.
}
\details{
'Landing' is a \code{\link[data.table]{data.table}} with aggregated weight of landings from landing records.
Columns are specified in the section Column definitions Landing
}
\section{Column definitions Landing}{

 \describe{
  \item{Species}{character() code for species category (species identified by market or regulation standards. Several codes may code the same species or stock, and some catch may be recorded only at higher taxonomic classifications)}
  \item{Year}{integer() Year of catch}
  \item{CatchDate}{POSIXct() Date of catch (last catch on trip) in UTC}
  \item{Gear}{character() Code for gear used for catch (dominant gear for trip)}
  \item{Area}{character() Area code for the position where the catch was caught (dominant area for trip)}
  \item{SubArea}{character() Subdivision of area code for the position where the catch was caught (dominant area for trip)}
  \item{Coastal}{character() Code indicating whether catch was taken within coastal delimitation line (dominant side for trip)}
  \item{N62Code}{character() Code indicating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
  \item{VesselLength}{character() Length of vessel in m}
  \item{CountryVessel}{character() Country of the vessel that caught the catch}
  \item{LandingSite}{character() Code identifying landing site (buyer of catch)}
  \item{CountryLanding}{character() Country where catch was landed}
  \item{Usage}{character() Code for market usage of catch.}
  \item{RoundWeight}{numeric() Weight of round catch in kg.}
 }
}

\seealso{
\code{\link{DataTypes}} for a list of all StoX data types produced by \code{\link{RstoxData}}
}
