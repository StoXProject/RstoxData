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