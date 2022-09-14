# NMD Biotic v3.1
example <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
example_cdata <- system.file("testresources","landing_cdata.xml", package="RstoxData")
example_wo_cdata <- system.file("testresources","landing.xml", package="RstoxData")
example_malformed <- system.file("testresources","landing_nonl.xml", package="RstoxData")

# test encoding of commonname column by filtering
enctest <- RstoxData:::readXmlFile(example, stream = F)
expect_equal(nrow(enctest$catchsample), 4)
expect_equal(nrow(enctest$catchsample[enctest$catchsample$commonname == "sn\u00F8krabbe"]), 2)

enctest <- RstoxData:::readXmlFile(example, stream = T)
expect_equal(nrow(enctest$catchsample), 4)
expect_equal(nrow(enctest$catchsample[enctest$catchsample$commonname == "sn\u00F8krabbe"]), 2)


#context("test-readXmlFile: DOM parse NMD Biotic v3.1")
defaultParseBiotic <- RstoxData:::readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParseBiotic)))
expect_equal(nrow(defaultParseBiotic$fishstation), 2)
expect_equal(defaultParseBiotic$fishstation$fishingbait[[1]], "1")

#context("Test readXmlFile with cdata")
dd<-RstoxData:::readXmlFile(example_cdata, stream = F)
comp<-RstoxData:::readXmlFile(example_wo_cdata, stream = F)
expect_equal(length(all.equal(comp, dd)),2) #should differ in one row and in metadata
expect_true("BAKE & SHAKE" %in% dd[["Fart\u00F8y"]][["Fart\u00F8ynavn"]])
expect_equal(nrow(dd$Seddellinje), 20)

#context("Test readXmlFile with cdata streaming")
dd<-RstoxData:::readXmlFile(example_cdata, stream = T)
comp<-RstoxData:::readXmlFile(example_wo_cdata, stream = F)
expect_equal(length(all.equal(comp, dd)),2) #should differ in one row and in metadata
expect_true("BAKE & SHAKE" %in% dd[["Fart\u00F8y"]][["Fart\u00F8ynavn"]])
expect_equal(nrow(dd$Seddellinje), 20)

#context("Test readXmlFile malformed xml")
expect_error(RstoxData:::readXmlFile(example_malformed, stream = F))

#non utf8-path
utf8offensiveFiles <- system.file("testresources","utf8offensive", package="RstoxData")
zipfile <- system.file("testresources","utf8offensive", "files.zip", package="RstoxData")
paths <- unzip(zipfile, exdir = utf8offensiveFiles)
filepaths <- list.files(utf8offensiveFiles, full.names = T)
#
# zipfile contains only two files. One zipped (within zip), one flat.
# These are both the same data, hence the check on number of fishstations below.
#
for (f in filepaths){
  if (f != zipfile){
    if (grepl(".zip", f)){
      contentStream <- RstoxData::readXmlFile(f, stream = T)
      expect_equal(nrow(contentStream$fishstation), 2)
    }
    else{
      contentDom <- RstoxData::readXmlFile(f, stream = F)
      contentStream <- RstoxData::readXmlFile(f, stream = T)
      expect_true(length(all.equal(contentStream, contentDom))<=1) 
      expect_equal(nrow(contentStream$fishstation), 2)
    }
  }
}
#remove extracted files
for (f in filepaths){
  if (f != zipfile){
    unlink(f)
  }
}


#context("test-readXmlFile: DOM parse NMD Biotic v3.1")
defaultParseBiotic <- RstoxData:::readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParseBiotic)))
expect_equal(nrow(defaultParseBiotic$fishstation), 2)
expect_equal(defaultParseBiotic$fishstation$fishingbait[[1]], "1")

#context("test-readXmlFile: useXsd-option")
RstoxData:::readXmlFile(example, stream = T, useXsd = "nmdbioticv3.1")
expect_error(RstoxData:::readXmlFile(example, stream = T, useXsd = "unkown"), "useXsd=unkown is not supported. Supported values:")
expect_warning(RstoxData:::readXmlFile(example, stream = T, useXsd = "nmdbioticv3"))
expect_error(RstoxData:::readXmlFile(example, stream = F, useXsd = "nmdbioticv3"))


#context("test-readXmlFile: stream parse NMD Biotic v3.1")
streamParseBiotic <- RstoxData:::readXmlFile(example, stream = T)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(streamParseBiotic)))
expect_equal(nrow(streamParseBiotic$fishstation), 2)
expect_equal(streamParseBiotic$fishstation$fishingbait[[1]], "1")

# NMD Biotic v3
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

#context("test-readXmlFile: DOM parse NMD biotic v3")
defaultParseBiotic <- RstoxData:::readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParseBiotic)))
expect_equal(nrow(defaultParseBiotic$fishstation), 2)

#context("test-readXmlFile: stream parse NMD biotic v3")
streamParseBiotic <- RstoxData:::readXmlFile(example, stream = T)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(streamParseBiotic)))
expect_equal(nrow(streamParseBiotic$fishstation), 2)

# NMD Echosounder
example <- system.file("testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")

#context("test-readXmlFile: DOM echosounder")
defaultParseEchosounder <- RstoxData:::readXmlFile(example, stream = F)
expect_equal(nrow(defaultParseEchosounder$distance), 2)

#context("test-readXmlFile: stream parse echosounder")
streamParseEchosounder <- RstoxData:::readXmlFile(example, stream = T)
expect_equal(nrow(streamParseEchosounder$distance), 2)

# NMD Landing
example <- system.file("testresources","landing.xml", package="RstoxData")

#context("test-readXmlFile: stream parse landing")
streamParse <- RstoxData:::readXmlFile(example, stream = T)
expect_true(all(c("Art", "Dellanding", "Fangstdata", "Landingsdata", "Seddellinje") %in% names(streamParse)))
expect_false(any(is.na(streamParse$Produkt$Rundvekt)))
expect_false(all(is.na(streamParse$Produkt$Registreringsmerke_seddel)))

# Encodings
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

#context("test-readXmlFile: Data text encoding")
encParse <- RstoxData:::readXmlFile(example, stream = F)
expect_true(encParse$mission$missiontypename[1] == "Pr\u00F8veb\u00E5t")
encParse <- RstoxData:::readXmlFile(example, stream = T)
expect_true(encParse$mission$missiontypename[1] == "Pr\u00F8veb\u00E5t")

#context("test-readXmlFile: Path encoding")
testing <- paste0(tempfile(pattern=""), "_bio_\u00E5_pr\u00F8ve.xml")
file.copy(example, testing)
encParse <- RstoxData:::readXmlFile(testing, stream = F)
expect_true(encParse$mission$missiontypename[1] == "Pr\u00F8veb\u00E5t")
encParse <- RstoxData:::readXmlFile(testing, stream = T)
expect_true(encParse$mission$missiontypename[1] == "Pr\u00F8veb\u00E5t")
unlink(testing)

# ICES data
#context("test-readXmlFile: ICES acoustic and biotic data reading")
icesFiles <- c("ICES_Acoustic_1.xml", "ICES_Biotic_1.xml", "ICES_Acoustic_2.xml", "ICES_Biotic_2.xml")
exampleDir <- system.file("testresources","", package="RstoxData")

for(item in icesFiles) {
	icesDataA <- RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = T)
	icesDataB <- RstoxData:::readXmlFile(paste0(exampleDir, "/", item), stream = F)
	# There should be minimal differences (in the Survey table only)
	expect_true(length(all.equal(icesDataA, icesDataB)) <= 1)
}

#context("Test BOM")
icesDataA <- RstoxData:::readXmlFile(paste0(exampleDir, "/", "ICES_Biotic_2.xml"), stream = T)
icesDataB <- RstoxData:::readXmlFile(paste0(exampleDir, "/", "ICES_Biotic_2_BOM.xml"), stream = T)
expect_equal(icesDataA$Haul$LocalID, icesDataB$Haul$LocalID)
# should be exactly the same except metadata (filename)
expect_true(length(all.equal(icesDataA, icesDataB)) == 1) 
icesDataA$metadata$file <- icesDataB$metadata$file
expect_true(all.equal(icesDataA, icesDataB)) 

# Zipped files:
#context("test-readXmlFile: Zipped acoustic file")
example <- system.file("testresources", "echosounder_2020821.zip", package="RstoxData")
parsedAcousticZip <- RstoxData:::readXmlFile(example)
expect_equal(parsedAcousticZip$sa$sa[1], 67.7185200)

#context("test-readXmlFile: Zipped biotic file")
example <- system.file("testresources", "biotic_2020821.zip", package="RstoxData")
parsedBioticZip <- RstoxData:::readXmlFile(example)
expect_equal(parsedBioticZip$individual$individualweight[1], 0.022)

