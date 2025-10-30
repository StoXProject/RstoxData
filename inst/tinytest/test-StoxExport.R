# Satisfy R CMD check


example <- system.file("testresources", "biotic_v3_example.xml", package="RstoxData")	
data <- RstoxData:::ReadBiotic(example)
#
data[[1]]$fishstation[, stationstartdate := stationstopdate]
datras <- RstoxData::ICESDatras(data)
expect_true(all(c("HH", "HL", "CA") %in% names(datras)))
expect_equal(nrow(datras$HH), 2)


example <- system.file("testresources", "biotic_v3_example.xml", package="RstoxData")
data <- RstoxData::ReadBiotic(example)

data[[1]]$fishstation[, stationstartdate := stationstopdate]
# TEMPORARILY DISABLED DUE TO ICES NOT ACCEPTING MACOS "x86_64". UN-COMMENT THIS WHEN THE ICES PROBLEM IS FIXED:
if(Sys.info()["machine"] == "arm64") {
	ICESBioticData <- RstoxData::ICESBiotic(data, SurveyName = "NONE", Country = "No", Organisation = 612)
	ICESBioticData <- RstoxData::WriteICESBiotic(ICESBioticData)
	expect_equal(dim(ICESBioticData), c(96, 45))
}



example <- system.file("testresources", "ICES_Acoustic_1.xml", package="RstoxData")
data <- RstoxData::ReadAcoustic(example)
#modify to get rid of warnings
data$ICES_Acoustic_1.xml$Survey$Code <- data$ICES_Acoustic_1.xml$Survey$Code[1]
data$ICES_Acoustic_1.xml$Data$SaCategory <- "MAC"

# TEMPORARILY DISABLED DUE TO ICES NOT ACCEPTING MACOS "x86_64". UN-COMMENT THIS WHEN THE ICES PROBLEM IS FIXED:
if(Sys.info()["machine"] == "arm64") {
	ICESAcoustic2 <- RstoxData::ICESAcoustic(data)
	ICESAcousticCSV2 <- RstoxData::WriteICESAcoustic(ICESAcoustic2)
	expect_equal(dim(ICESAcousticCSV2), c(19, 28))
}



example <- system.file("testresources", "ICES_Acoustic_2.xml", package="RstoxData")
data <- RstoxData::ReadAcoustic(example)
#modify to get rid of warnings
data$ICES_Acoustic_2.xml$Survey$Code <- data$ICES_Acoustic_2.xml$Survey$Code[1]

# TEMPORARILY DISABLED DUE TO ICES NOT ACCEPTING MACOS "x86_64". UN-COMMENT THIS WHEN THE ICES PROBLEM IS FIXED:
if(Sys.info()["machine"] == "arm64") {
	ICESAcoustic2 <- RstoxData::ICESAcoustic(data)
	ICESAcousticCSV2 <- RstoxData::WriteICESAcoustic(ICESAcoustic2)
	expect_equal(dim(ICESAcousticCSV2), c(23, 28))
}
