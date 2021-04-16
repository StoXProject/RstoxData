context("Test write XML")

#' writes file with writeXmlFile
#' read back in with readXmlFile
#' asserts that data is equal to what was read back
expect_equal_read_back_in_xml <- function(data, xsdObject, namespace, writer="writeXmlFile"){
  
  #force ordering by keys
  data <- setKeysDataTables(data, xsdObject)
  
  tmp <- tempfile(fileext = "xml")
  if (writer == "writeXmlFile"){
    writeXmlFile(tmp, data, xsdObject, namespace)    
  }
  else if (writer == "fWriteLandings"){
    fWriteLandings(tmp, data, namespace)
  }

  backIn <- readXmlFile(tmp)
  unlink(tmp)
  
  # force filename to be the same
  backIn$metadata$file <- data$metadata$file
  
  #set keys again (for expect_equal)
  backIn <- setKeysDataTables(backIn, xsdObject)
  expect_equal(data, backIn)
}

context("test writing biotic v.3.1")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$nmdbioticv3.1.xsd, "http://www.imr.no/formats/nmdbiotic/v3.1")

context("test writing landinger v.2")
example <- RstoxData::readXmlFile(system.file("testresources","landing.xml", package="RstoxData"))
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$landingerv2.xsd, "http://www.imr.no/formats/landinger/v2")

context("test biotic 3.1 to 3.0 conversion")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
tmp <- tempfile(fileext = "xml")
writeXmlFile(tmp, example, xsdObjects$nmdbioticv3.xsd, "http://www.imr.no/formats/nmdbiotic/v3")
backIn <- readXmlFile(tmp)
unlink(tmp)

expect_true(sum(!is.na(example$fishstation$fishingbait))>0)
expect_true(is.null(backIn$fishstation$fishingbait))

#check that read back in is equal if format differences are removed
example$fishstation$fishingbait <- NULL
example$individual$morphologytype <- NULL
example$individual$rightclawheight <- NULL
example$prey$preyforeignobject <- NULL
example$metadata <- backIn$metadata
expect_equal(example, backIn)


context("test writing landinger v.2 fwrite")
example <- RstoxData::readXmlFile(system.file("testresources","landing.xml", package="RstoxData"))
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$landingerv2.xsd, "http://www.imr.no/formats/landinger/v2", writer="fWriteLandings")

