#context("Test write XML")

#' writes file with writeXmlFile
#' read back in with readXmlFile
#' asserts that data is equal to what was read back
expect_equal_read_back_in_xml <- function(data, xsdObject, namespace, writer="writeXmlFile"){
  
  #force ordering by keys
  data <- RstoxData:::setKeysDataTables(data, xsdObject)
  
  tmp <- tempfile(fileext = ".xml")
  if (writer == "writeXmlFile"){
    RstoxData:::writeXmlFile(tmp, data, xsdObject, namespace)    
  }
  else if (writer == "fWriteLandings"){
    RstoxData:::fWriteLandings(tmp, data, namespace)
  }

  backIn <- RstoxData::readXmlFile(tmp)
  unlink(tmp)
  
  # force filename to be the same
  backIn$metadata$file <- data$metadata$file
  
  #set keys again (for expect_equal)
  backIn <- RstoxData:::setKeysDataTables(backIn, xsdObject)
  expect_equal(data, backIn)
}

#context("test writing biotic v.3.1")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$nmdbioticv3.1.xsd, "http://www.imr.no/formats/nmdbiotic/v3.1")

#context("test writing landinger v.2")
example <- RstoxData::readXmlFile(system.file("testresources","landing.xml", package="RstoxData"))
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$landingerv2.xsd, "http://www.imr.no/formats/landinger/v2")

#context("test writing cdata landing")
example <- RstoxData::readXmlFile(system.file("testresources","landing.xml", package="RstoxData"))
example$Fisker$Fiskerkommune[1] <- "<Fisk/>"
expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$landingerv2.xsd, "http://www.imr.no/formats/landinger/v2")


#context("test biotic 3.1 to 3.0 conversion")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
tmp <- tempfile(fileext = ".xml")
RstoxData:::writeXmlFile(tmp, example, RstoxData::xsdObjects$nmdbioticv3.xsd, "http://www.imr.no/formats/nmdbiotic/v3")
backIn <- RstoxData::readXmlFile(tmp)
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

#context("test convertBioticFile")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
tmp <- tempfile(fileext = ".xml")
expect_error(RstoxData::convertBioticFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"), tmp, "nmdbioticv3.0.xsd"))
RstoxData::convertBioticFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"), tmp, "nmdbioticv3.xsd")
backIn <- RstoxData::readXmlFile(tmp)
unlink(tmp)
expect_equal(backIn$metadata$useXsd, "nmdbioticv3")
expect_true(is.null(backIn$fishstation$fishingbait))
expect_true(!is.null(example$fishstation$fishingbait))
expect_true(all(backIn$fishstation$serialnumber == example$fishstation$serialnumber))

example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
tmp <- tempfile(fileext = ".xml")
RstoxData::convertBioticFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"), tmp, "nmdbioticv3.1.xsd")
backIn <- RstoxData::readXmlFile(tmp)
unlink(tmp)
expect_equal(backIn$metadata$useXsd, "nmdbioticv3.1")
expect_true(!is.null(backIn$fishstation$fishingbait))
expect_true(!is.null(example$fishstation$fishingbait))
expect_true(all(backIn$fishstation$serialnumber == example$fishstation$serialnumber))



#context("test writing landinger v.2 fwrite")
if (l10n_info()[["UTF-8"]]){
  example <- RstoxData::readXmlFile(system.file("testresources","landing.xml", package="RstoxData"))
  expect_equal_read_back_in_xml(example, RstoxData::xsdObjects$landingerv2.xsd, "http://www.imr.no/formats/landinger/v2", writer="fWriteLandings")
} else{
  tmp <- tempfile(fileext = ".xml")
  expect_error(RstoxData:::fWriteLandings(tmp, example, "http://www.imr.no/formats/landinger/v2"))
  unlink(tmp)
}

#context("Test public functions Landing. Single file")
tmp <- tempfile(fileext = ".xml")
example <- RstoxData::ReadLanding(system.file("testresources","landing.xml", package="RstoxData"))

#force ordering
example$landing.xml <- RstoxData:::setKeysDataTables(example$landing.xml, RstoxData::xsdObjects$landingerv2.xsd)
RstoxData:::WriteLanding(example, tmp)
backIn <- RstoxData::ReadLanding(tmp)
names(backIn) <- names(example)
backIn$landing.xml$metadata$file <- example$landing.xml$metadata$file
#set keys again (for expect_equal)
backIn$landing.xml <- RstoxData:::setKeysDataTables(backIn$landing.xml, RstoxData::xsdObjects$landingerv2.xsd)
expect_equal(example, backIn)


#context("Test overwrite check")
expect_error(RstoxData:::WriteLanding(example, tmp))
RstoxData:::WriteLanding(example, tmp, overwrite = T)

unlink(tmp)

#context("Test public functions Landing. Multiple files")
tmp <- tempfile(fileext = ".xml")
tmp2 <- tempfile(fileext = ".xml")
example$l2 <- example$landing.xml
RstoxData:::WriteLanding(example, c(tmp, tmp2))
backIn <- RstoxData::ReadLanding(tmp2)
example$landing.xml <- NULL
names(backIn) <- names(example)
backIn$l2$metadata$file <- example$l2$metadata$file
#set keys again (for expect_equal)
backIn$l2 <- RstoxData:::setKeysDataTables(backIn$l2, RstoxData::xsdObjects$landingerv2.xsd)
expect_equal(example, backIn)

unlink(tmp)
unlink(tmp2)

#context("Test public functions Biotic Single file")
tmp <- tempfile(fileext = ".xml")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
example$biotic3.1_example.xml$catchsample$catchcomment <- "2\u00C5\u008cEBROSMESLE"
RstoxData:::WriteBiotic(example, tmp)
backIn <- RstoxData::ReadBiotic(tmp)
expect_true(all(backIn[[1]]$catchsample$catchcomment == "2\u00C5\u008cEBROSMESLE"))
expect_equal(example$biotic3.1_example.xml$catchsample$commonname[example$biotic3.1_example.xml$catchsample$catchsampleid==1],
backIn[[1]]$catchsample$commonname[backIn[[1]]$catchsample$catchsampleid==1])
names(backIn) <- names(example)
backIn$biotic3.1_example.xml$metadata$file <- example$biotic3.1_example.xml$metadata$file
expect_equal(example, backIn)
unlink(tmp)

#context("Test conversion biotic")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
RstoxData:::WriteBiotic(example, tmp, namespaces = "http://www.imr.no/formats/nmdbiotic/v3")
backIn <- RstoxData::ReadBiotic(tmp)
unlink(tmp)
expect_true(!is.null(example$biotic3.1_example.xml$fishstation$fishingbait))
expect_true(is.null(backIn[[1]]$fishstation$fishingbait))
expect_equal(backIn[[1]]$metadata$useXsd, "nmdbioticv3")
expect_equal(example$biotic3.1_example.xml$metadata$useXsd, "nmdbioticv3.1")

#context("Test public functions Biotic Multiple files")
tmp <- tempfile(fileext = ".xml")
tmp2 <- tempfile(fileext = ".xml")
example <- RstoxData::ReadBiotic(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
example$b2 <- example$biotic3.1_example.xml
RstoxData:::WriteBiotic(example, c(tmp, tmp2))
unlink(tmp)
unlink(tmp2)
