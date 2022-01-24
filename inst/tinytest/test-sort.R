#context("test-sort")

exampleFile <- system.file("testresources","biotic_2020821.zip", package="RstoxData")
exampleData <- StoxBiotic(ReadBiotic(exampleFile))

expecteFile <- system.file("testresources","biotic_2020821.rds", package="RstoxData")
expectedData <- readRDS(expecteFile)

expect_equal(exampleData, expectedData)
