# Satisfy R CMD check
options("mc.cores" = 2)

# StoxBiotic
## NMD Biotic v3.1
example <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")

#context("test-StoxBiotic: using DOM biotic v3.1")
defaultParseBiotic <- RstoxData::readXmlFile(example, stream = F)
suppressWarnings(sb1 <- RstoxData::StoxBiotic(list(defaultParseBiotic)))
expect_equal(nrow(sb1$Haul), 2)

p2 <- defaultParseBiotic

#context("test-StoxBiotic: using stream parse biotic v3.1")
streamParseBiotic <- RstoxData::readXmlFile(example, stream = T)
suppressWarnings(sb2 <- RstoxData::StoxBiotic(list(streamParseBiotic)))
expect_equal(nrow(sb2$Haul), 2)

#context("test-StoxBiotic: all.equal")
expect_true(all.equal(sb1, sb2))

## NMD Biotic v3
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

#context("test-StoxBiotic: using DOM biotic v3")
defaultParseBiotic <- RstoxData::readXmlFile(example, stream = F)
suppressWarnings(sb1 <- RstoxData::StoxBiotic(list(defaultParseBiotic)))
expect_equal(nrow(sb1$Haul), 2)

#context("test-StoxBiotic: using stream parse biotic v3.1")
streamParseBiotic <- RstoxData::readXmlFile(example, stream = T)
suppressWarnings(sb2 <- RstoxData:::StoxBiotic(list(streamParseBiotic)))
expect_equal(nrow(sb2$Haul), 2)

#context("test-StoxBiotic: all.equal")
expect_true(all.equal(sb1, sb2))

## ICES Biotic
icesFiles <- c("ICES_Biotic_1.xml", "ICES_Biotic_2.xml")
exampleDir <- system.file("testresources","", package="RstoxData")

for(item in icesFiles) {
	#context(paste("test-StoxBiotic: ICES biotic data", item, "to StoxBiotic: DOM"))
    icesDataA <- RstoxData::StoxBiotic(list(RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = F)))
	expect_equal(nrow(icesDataA$Individual), 4)

	#context(paste("test-StoxBiotic: ICES biotic data", item, "to StoxBiotic: Stream"))
    icesDataB <- RstoxData::StoxBiotic(list(RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = T)))
	expect_equal(nrow(icesDataB$Individual), 4)

	#context(paste("test-StoxBiotic: ICES biotic data", item, "to StoxBiotic DOM == stream"))
    expect_true(all.equal(icesDataA, icesDataB))
}

## ICES Biotic (missing individual)
item <- "ICES_Biotic_1_missingind.xml"
exampleDir <- system.file("testresources","", package="RstoxData")

#context(paste("test-StoxBiotic: ICES biotic data (with missing individual)", item, "to StoxBiotic: DOM"))
icesDataA <- RstoxData::StoxBiotic(list(RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = F)))
expect_equal(nrow(icesDataA$Individual), 6)
expect_equal(icesDataA$Sample[1, ]$SampleNumber, 6)


#context(paste("test-StoxBiotic: ICES biotic data (with missing individual)", item, "to StoxBiotic: Stream"))
icesDataB <- RstoxData::StoxBiotic(list(RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = T)))
expect_equal(nrow(icesDataB$Individual), 6)
expect_equal(icesDataB$Sample[1, ]$SampleNumber, 6)

#context(paste("test-StoxBiotic: ICES biotic data (with missing individual)", item, "to StoxBiotic DOM == stream"))
expect_true(all.equal(icesDataA, icesDataB))

#context("Test repeating catchpartnumbers")
example <- system.file("testresources","repeatingcp.xml", package="RstoxData")
biotic <- RstoxData::ReadBiotic(example)
suppressWarnings(sb <- RstoxData:::StoxBiotic(biotic))
expect_equal(nrow(biotic$repeatingcp.xml$catchsample), nrow(sb$Sample))

