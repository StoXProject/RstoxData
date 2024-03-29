% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{generalSamplingHierarhcy}
\alias{generalSamplingHierarhcy}
\title{General sampling hierarchy of StoX}
\description{
The general sampling hierarchy of StoX defines a common hierarchy of sampling levels for the StoxBiotic and StoxAcoustic data formats.
}
\details{
The general sampling hierarchy of StoX is defined by 6 levels (tables) as shown alongside the levels of the StoxcBiotic and StoxAcoustic format in the following table:

\tabular{lll}{
General level \tab StoxBiotic level \tab StoxAcoustic level\cr
Cruise \tab Cruise \tab Cruise\cr
Station \tab Station \tab Log\cr
Equipment \tab Haul \tab Beam\cr
Species \tab SpeciesCategory \tab AcousticCategory\cr
Sample \tab Sample \tab ChannelReference\cr
Individual \tab Individual \tab NASC
}

The levels can be interpreted as follows: 

(1) The Cruise level is the entire trip or mission conducted by a platform, such as a research vessel. 

(2) The Station level is a geographical position at a specific point in time where sampling is conducted. 

(3) The Equipment level specifies the equipment used to sample, possibly several equipments at the same station, such as two different trawls or  different acoustic instruments or acoustic frequencies. 

(4) The Species level is the biological species or acoustic category (normally reflecting one or more biological species) sampled by the equipment. 

(5) The Sample level is the specific sample of the Species, such as herring or cod for StoxBiotic. For StoxAcoustic the Sample level denotes different coordinate systems in which the acoustic data are defined, with possible values "P" for pelagic channels defined by origin at the surface and z axis pointing vertically downwards, and "B" for bottom referenced channels with origin on the seabed and z axis pointing vertically upwards. 

(6) The Individual level contains for the StoxBiotic format the individuals selected for specific measurements of individual properties such as length, weight and gender, whereas for StoxAcoustic the indiivdual samples along an acouostic beam.
}
\seealso{
The general sampling hierarchy is used in the format \code{\link{StoxBioticFormat}} and \code{\link{StoxAcousticFormat}}.
}
