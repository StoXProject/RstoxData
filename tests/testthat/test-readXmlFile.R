# NMD Biotic
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

context("test-readXmlFile: DOM biotic")
defaultParseBiotic <- readXmlFile(example, stream = F)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(defaultParseBiotic)))
expect_equal(nrow(defaultParseBiotic$fishstation), 2)

context("test-readXmlFile: stream parse biotic")
streamParseBiotic <- readXmlFile(example, stream = T)
expect_true(all(c("mission", "fishstation", "catchsample", "individual", "agedetermination") %in% names(streamParseBiotic)))
expect_equal(nrow(streamParseBiotic$fishstation), 2)

# NMD Echosounder
example <- system.file("testresources","libas_ListUserFile20__L40.0-2259.9_small.xml", package="RstoxData")

context("test-readXmlFile: DOM echosounder")
defaultParseEchosounder <- readXmlFile(example, stream = F)
expect_equal(nrow(defaultParseEchosounder$distance), 2)

context("test-readXmlFile: stream parse echosounder")
streamParseEchosounder <- readXmlFile(example, stream = T)
expect_equal(nrow(streamParseEchosounder$distance), 2)

# NMD Landing
example <- system.file("testresources","landing.xml", package="RstoxData")

context("test-readXmlFile: stream parse landing")
streamParse <- readXmlFile(example, stream = T)
expect_true(all(c("Art", "Dellanding", "Fangstdata", "Landingsdata", "Seddellinje") %in% names(streamParse)))
expect_false(any(is.na(streamParse$Produkt$Rundvekt)))
expect_false(all(is.na(streamParse$Produkt$Registreringsmerke_seddel)))

# Encodings
example <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

context("test-readXmlFile: Data text encoding")
encParse <- readXmlFile(example, stream = F)
expect_true(encParse$mission$missiontypename[1] == "Prøvebåt")
encParse <- readXmlFile(example, stream = T)
expect_true(encParse$mission$missiontypename[1] == "Prøvebåt")

context("test-readXmlFile: Path encoding")
testing <- paste0(dirname(example), "/", "bio_å_prøve.xml")
file.copy(example, testing)
encParse <- readXmlFile(testing, stream = F)
expect_true(encParse$mission$missiontypename[1] == "Prøvebåt")
encParse <- readXmlFile(testing, stream = T)
expect_true(encParse$mission$missiontypename[1] == "Prøvebåt")
