#context("test-icesParsing")

#context("parseInterCatch: normal run")
data <- RstoxData:::parseInterCatch(system.file("testresources","intercatch_example.csv", package="RstoxData"))
expect_equal(length(data),3)
expect_equal(nrow(data$HI), 12)
expect_equal(ncol(data$HI), 11)
expect_equal(ncol(data$SI), 23)
expect_equal(nrow(data$SD), 20)
expect_equal(ncol(data$SD), 32)

comb <- merge(data$SI, data$SD)
expect_equal(ncol(comb), 43)
expect_equal(nrow(comb), 20)

dataVariant <- RstoxData:::parseInterCatch(system.file("testresources","intercatch_variant.csv", package="RstoxData"))
expect_equal(ncol(dataVariant$HI), ncol(data$HI))
expect_equal(ncol(dataVariant$SI), ncol(data$SI))
expect_equal(ncol(dataVariant$SD), ncol(data$SD))

expect_equal(length(dataVariant),3)
expect_equal(nrow(dataVariant$HI), 2)
expect_equal(ncol(dataVariant$HI), 11)
expect_equal(ncol(dataVariant$SI), 23)
expect_equal(nrow(dataVariant$SD), 30)
expect_equal(ncol(dataVariant$SD), 32)

expect_error(RstoxData:::parseInterCatch(system.file("testresources","intercatch_error.csv", package="RstoxData")), "Malformed InterCatch 1.0 file.")