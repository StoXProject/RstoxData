# Satisfy R CMD check


#context("test-StoxExport: DATRAS export")
#example <- system.file("testresources", "biotic_v3_example.xml", package="RstoxData")	
#data <- ReadBiotic(example)
#
#data[[1]]$fishstation[, stationstartdate := stationstopdate]
#datras <- RstoxData::ICESDatras(data)
#expect_equal(nrow(datras[[1]]$HH), 2)

context("test-StoxExport: ICES biotic export")
example <- system.file("testresources", "biotic_v3_example.xml", package="RstoxData")
data <- RstoxData::ReadBiotic(example)

data[[1]]$fishstation[, stationstartdate := stationstopdate]
ICESBiotic <- RstoxData::ICESBiotic(data, SurveyName = "NONE", Country = "No", Organisation = 612)
ICESBiotic <- RstoxData::WriteICESBiotic(ICESBiotic)
expect_equal(dim(ICESBiotic[[1]]), c(96, 45))


context("test-StoxExport: ICES acoustic export #1")
example <- system.file("testresources", "ICES_Acoustic_1.xml", package="RstoxData")
data <- RstoxData::ReadAcoustic(example)
ICESAcoustic2 <- RstoxData::ICESAcoustic(data)
ICESAcousticCSV2 <- RstoxData::WriteICESAcoustic(ICESAcoustic2)
expect_equal(dim(ICESAcousticCSV2[[1]]), c(19, 25))



context("test-StoxExport: ICES acoustic export #2")
example <- system.file("testresources", "ICES_Acoustic_2.xml", package="RstoxData")
data <- RstoxData::ReadAcoustic(example)
ICESAcoustic2 <- RstoxData::ICESAcoustic(data)
ICESAcousticCSV2 <- RstoxData::WriteICESAcoustic(ICESAcoustic2)
expect_equal(dim(ICESAcousticCSV2[[1]]), c(23, 25))

