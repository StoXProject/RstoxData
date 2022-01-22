context("test-fdirParsing")

context("readLssFile: normal run")
data <- readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxData"))
expect_true(data.table::is.data.table(data))
expect_true("Bruttovekt" %in% names(data))
expect_true("Bruttovekt" %in% names(data))
expect_true("Redskap" %in% names(data))
expect_true("POSIXct" %in% class(data$`Siste fangstdato`))
expect_true(is.numeric(data$Rundvekt))
expect_true("Snurpenot/ringnot" %in% data$Redskap)
expect_true(all(!is.na(data$`Art - FDIR`)))
expect_equal(nrow(data),9)

suppressMessages(nonstrictdata <- readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxData"), strict=F))
expect_equal(nrow(data), nrow(nonstrictdata))
expect_equal(ncol(data), ncol(nonstrictdata))
expect_true(all(names(data) == names(nonstrictdata)))

context("readLssFile: wrong headers")
expect_warning(wh <- readLssFile(system.file("testresources","landings_trimmed_2018_alt_header.lss", package="RstoxData"), strict=T))
expect_equal(nrow(data), nrow(wh))
expect_equal(ncol(data), ncol(wh))
expect_true(all(names(data) == names(wh))) #make sure names are the same even if they are not in the data file

context("Test convertLandings")
landings <- convertToLandingData(data)
expect_true(RstoxData::is.LandingData(landings))
expect_equal(sum(landings$ConvertedData$Produkt$Rundvekt), sum(data$Rundvekt))

context("readErsFile: normal run")
data <- readErsFile(system.file("testresources","logbooks_trimmed_2015.psv", package="RstoxData"))
expect_true(data.table::is.data.table(data))
expect_true("RC" %in% names(data))
expect_true(is.numeric(data$RUNDVEKT))
expect_true(is.numeric(data[["ST\u00D8RSTE_LENGDE"]]))
expect_equal(nrow(data),9)
expect_true("UTC" %in% attr(as.POSIXlt(data$STARTTIDSPUNKT), "tzone"))

