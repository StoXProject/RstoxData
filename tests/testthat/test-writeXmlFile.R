context("Test write XML")

#' writes file with writeXmlFile
#' read back in with readXmlFile
#' asserts that data is equal to what was read back
expect_equal_read_back_in_xml <- function(data, xsdObject, namespace, writer="writeXmlFile"){
  
  #force ordering by keys
  data <- setKeysDataTables(data, xsdObject)
  
  tmp <- tempfile(fileext = ".xml")
  if (writer == "writeXmlFile"){
    writeXmlFile(tmp, data, xsdObject, namespace)    
  }
  else if (writer == "fWriteLandings"){
    if (l10n_info()[["UTF-8"]]){
      fWriteLandings(tmp, data, namespace)
    }
    else{
      expect_error(fWriteLandings(tmp, data, namespace))
    }
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


context("Test public functions Landing. Single file")
tmp <- tempfile(fileext = ".xml")
example <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxData"))
WriteLanding(example, tmp)
backIn <- RstoxData::ReadLanding(tmp)
names(backIn) <- names(example)
backIn$landing.xml$metadata$file <- example$landing.xml$metadata$file
expect_equal(example, backIn)


context("Test overwrite check")
expect_error(WriteLanding(example, tmp))
WriteLanding(example, tmp, overwrite = T)

unlink(tmp)

context("Test public functions Landing. Multiple files")
tmp <- tempfile(fileext = ".xml")
tmp2 <- tempfile(fileext = ".xml")
example$l2 <- example$landing.xml
WriteLanding(example, c(tmp, tmp2))
backIn <- RstoxData::ReadLanding(tmp2)
example$landing.xml <- NULL
names(backIn) <- names(example)
backIn$l2$metadata$file <- example$l2$metadata$file
expect_equal(example, backIn)

unlink(tmp)
unlink(tmp2)

context("Test public functions Biotic Single file")
tmp <- tempfile(fileext = ".xml")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
WriteBiotic(example, tmp)
backIn <- RstoxData::ReadBiotic(tmp)
names(backIn) <- names(example)
backIn$biotic3.1_example.xml$metadata$file <- example$biotic3.1_example.xml$metadata$file
expect_equal(example, backIn)
unlink(tmp)

context("Test conversion biotic")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
WriteBiotic(example, tmp, namespaces = "http://www.imr.no/formats/nmdbiotic/v3")
backIn <- RstoxData::ReadBiotic(tmp)
unlink(tmp)
expect_true(!is.null(example$biotic3.1_example.xml$fishstation$fishingbait))
expect_true(is.null(backIn[[1]]$fishstation$fishingbait))
expect_equal(backIn[[1]]$metadata$useXsd, "nmdbioticv3")
expect_equal(example$biotic3.1_example.xml$metadata$useXsd, "nmdbioticv3.1")

context("Test public functions Biotic Multiple files")
tmp <- tempfile(fileext = ".xml")
tmp2 <- tempfile(fileext = ".xml")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
example$b2 <- example$biotic3.1_example.xml
WriteBiotic(example, c(tmp, tmp2))
unlink(tmp)
unlink(tmp2)
