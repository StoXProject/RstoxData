exampleFile <- system.file("testresources","ICES_Acoustic_2.xml", package="RstoxData")
acousticData <- ReadAcoustic(exampleFile)
newFilePath <- tempfile(fileext = ".xml")
croppedAcousticData <- ReadAcoustic(cropAcoustic(exampleFile, logsToKeep = 2, newFilePath = newFilePath))

expect_true(identical(acousticData[[1]]$Log[2, ], croppedAcousticData[[1]]$Log))
expect_true(identical(data.table::fsetdiff(acousticData[[1]]$Log, croppedAcousticData[[1]]$Log), acousticData[[1]]$Log[1, ]))
