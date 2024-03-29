% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeXmlFile.R
\name{WriteAcoustic}
\alias{WriteAcoustic}
\title{Write Acoustic}
\usage{
WriteAcoustic(
  AcousticData,
  FileNames = character(),
  namespaces = character(),
  encoding = "UTF-8",
  overwrite = FALSE
)
}
\arguments{
\item{AcousticData}{\code{\link[RstoxData]{AcousticData}} data to write.}

\item{FileNames}{paths to files that should be written to}

\item{namespaces}{XML namespaces to use for formatting.}

\item{encoding}{encoding to use for writing files}

\item{overwrite}{whether to overwrite any existing file(s)}
}
\description{
Write acoustic data as XML file(s).
}
\details{
\code{\link[RstoxData]{AcousticData}} may contain several data sets.
 In that case the parameters 'FileNames' and 'namespaces' must be provided for each
 in the order they appear in 'AcousticData'.

Supports writing to namespaces:
  http://www.imr.no/formats/nmdechosounder/v1
}
\examples{
exampleFile <- system.file(
    "testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")
acousticData <- ReadAcoustic(exampleFile)
newFile <- tempfile(fileext = ".xml")
WriteAcoustic(acousticData, newFile, namespaces = "http://www.imr.no/formats/nmdechosounder/v1")
acousticDataReadBackIn <- ReadAcoustic(newFile)
all.equal(
  acousticData[[1]][names(acousticData[[1]]) != "metadata"], 
  acousticDataReadBackIn[[1]][names(acousticDataReadBackIn[[1]]) != "metadata"]
)

}
