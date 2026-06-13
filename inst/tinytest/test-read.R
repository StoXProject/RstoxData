zippedICESAcousticXMLFile <- system.file("testresources","ICES_Acoustic_1.zip", package = "RstoxData")

a <- RstoxData::ReadAcoustic(zippedICESAcousticXMLFile)
expect_true(a$ICES_Acoustic_1.zip$Data[3, Value] == 46.45)


