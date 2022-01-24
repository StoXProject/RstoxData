nmdbiotixsd <- system.file("testresources", "nmdbioticv3.1.xsd", package="RstoxData")
icesbiotixsd <- system.file("testresources", "icesAcoustic.xsd", package="RstoxData")

xsdObjNmdBiotic <- createXsdObject(nmdbiotixsd)
expect_equal(xsdObjNmdBiotic$targetNamespace, "http://www.imr.no/formats/nmdbiotic/v3.1")
xsdObjIcesBiotic <- createXsdObject(icesbiotixsd)
expect_true(is.na(xsdObjIcesBiotic$targetNamespace))
