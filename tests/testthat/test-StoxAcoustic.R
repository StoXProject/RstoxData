# StoxAcoustic
example <- system.file("testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")

context("test-StoxAcoustic: DOM echosounder")
defaultParseEchosounder <- readXmlFile(example, stream = F)
sa1 <- StoxAcoustic(list(defaultParseEchosounder))
expect_equal(nrow(sa1$Beam), 2)

context("test-StoxAcoustic: stream parse echosounder")
streamParseEchosounder <- readXmlFile(example, stream = T)
sa2 <- StoxAcoustic(list(streamParseEchosounder))
expect_equal(nrow(sa2$Beam), 2)

context("test-StoxAcoustic: all.equal")
expect_true(all.equal(sa1, sa2))



