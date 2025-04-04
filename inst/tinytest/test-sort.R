#context("test-sort")

exampleFile <- system.file("testresources","biotic_2020821.zip", package="RstoxData")
suppressWarnings(exampleData <- RstoxData:::StoxBiotic(RstoxData:::ReadBiotic(exampleFile)))
# Only get down to the Individual table:
exampleData <- exampleData[seq_len(which(names(exampleData) == "Individual"))]


expecteFile <- system.file("testresources","biotic_2020821.rds", package="RstoxData")
expectedData <- readRDS(expecteFile)

expect_equal(exampleData, expectedData)
