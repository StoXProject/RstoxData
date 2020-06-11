# data-raw/process.R
# XSD data pre-processing

library(xml2)
library(usethis)

source("R/xsdUtils.R")

xsdFiles <- list.files("data-raw/", pattern="*.xsd",  full.names = TRUE)

xsdObjects <- lapply(xsdFiles, createXsdObject)

names(xsdObjects) <- basename(xsdFiles)

# Result ordering
xsdObjects[["nmdbioticv1.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.1.xsd"]]$tableOrder <- c("missions", "mission",  "missionlog", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.2.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.3.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv1.4.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylength", "copepodedevstage", "tag")
xsdObjects[["nmdbioticv3.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylengthfrequencytable", "copepodedevstagefrequencytable", "tag")
xsdObjects[["nmdbioticv3.1.xsd"]]$tableOrder <- c("missions", "mission", "fishstation", "catchsample", "individual", "prey", "agedetermination", "preylengthfrequencytable", "copepodedevstagefrequencytable", "tag")


xsdObjects[["nmdechosounderv1.xsd"]]$tableOrder <- c("echosounder_dataset", "distance_list", "distance", "frequency", "ch_type", "sa_by_acocat", "sa", "acocat_list", "acocat")
xsdObjects[["landingerv2.xsd"]]$tableOrder <- c("Landingsdata", "Seddellinje", "Art", "Produkt", "Dellanding", "Redskap", "Kvote", "Mottakendefartøy", "Fartøy", "Fisker", "Fangstdata", "Produksjon", "Mottaker", "Salgslagdata")

xsdObjects[["icesAcoustic.xsd"]]$tableOrder <- c("Acoustic", "Instrument", "Calibration", "DataAcquisition", "DataProcessing", "Cruise", "Survey", "Log", "Sample", "Data")
xsdObjects[["icesBiotic.xsd"]]$tableOrder <- c("Biotic", "Cruise", "Survey", "Haul", "Catch", "Biology")

use_data(xsdObjects, overwrite = TRUE)


