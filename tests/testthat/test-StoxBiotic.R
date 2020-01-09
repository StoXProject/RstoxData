# Satisfy R CMD check
options("mc.cores" = 2)

# StoxBiotic
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

context("test-StoxBiotic: using DOM biotic")
defaultParseBiotic <- readXmlFile(example, stream = F)
sb1 <- StoxBiotic(list(defaultParseBiotic))
expect_equal(nrow(sb1$Haul), 2)

context("test-StoxBiotic: using stream parse biotic")
streamParseBiotic <- readXmlFile(example, stream = T)
sb2 <- StoxBiotic(list(streamParseBiotic))
expect_equal(nrow(sb2$Haul), 2)

context("test-StoxBiotic: all.equal")
expect_true(all.equal(sb1, sb2))

