# data-raw/process.R
# XSD data pre-processing

library(xml2)
library(usethis)

source("R/xsdUtils.R")

xsdFiles <- list.files("data-raw/", pattern="*.xsd",  full.names = TRUE)

xsdObjects <- lapply(xsdFiles, createXsdObject)

names(xsdObjects) <- basename(xsdFiles)

use_data(xsdObjects, overwrite = TRUE)


