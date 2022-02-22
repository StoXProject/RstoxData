#context("test-fdirParsing")

#context("readLssFile: normal run")
data <- RstoxData:::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxData"))
data[["Fart\u00F8ykommune (kode)"]] <- "5444" #error in test file landings_trimmed_2018.lss
expect_true(data.table::is.data.table(data))
expect_true("Bruttovekt" %in% names(data))
expect_true("Bruttovekt" %in% names(data))
expect_true("Redskap" %in% names(data))
expect_true("POSIXct" %in% class(data$`Siste fangstdato`))
expect_true(is.numeric(data$Rundvekt))
expect_true("Snurpenot/ringnot" %in% data$Redskap)
expect_true(all(!is.na(data$`Art - FDIR`)))
expect_equal(nrow(data),9)

suppressMessages(nonstrictdata <- RstoxData:::readLssFile(system.file("testresources","landings_trimmed_2018.lss", package="RstoxData"), strict=F))
expect_equal(nrow(data), nrow(nonstrictdata))
expect_equal(ncol(data), ncol(nonstrictdata))
expect_true(all(names(data) == names(nonstrictdata)))

#context("readLssFile: wrong headers")
expect_warning(wh <- RstoxData:::readLssFile(system.file("testresources","landings_trimmed_2018_alt_header.lss", package="RstoxData"), strict=T))
expect_equal(nrow(data), nrow(wh))
expect_equal(ncol(data), ncol(wh))
expect_true(all(names(data) == names(wh))) #make sure names are the same even if they are not in the data file
expect_equal(check_tzones(wh$`Siste fangstdato`), "CET")

#context("Test convertLandings")
landings <- RstoxData:::convertToLandingData(data)
expect_true(all(landings$ConvertedData[["Fart\u00F8y"]][["Fart\u00F8ynasjonalitet_kode.Fart\u00F8y"]] == landings$ConvertedData[["Fart\u00F8y"]][["Fart\u00F8ynasjonalitet_kode"]]))
expect_true(RstoxData::is.LandingData(landings))
expect_equal(sum(landings$ConvertedData$Produkt$Rundvekt), sum(data$Rundvekt))
sl <- StoxLanding(landings)
expect_true(RstoxData::is.StoxLandingData(sl))

#context("Test convertToLssData")
lss <- RstoxData:::convertToLssData(landings)
lss <- lss[order(lss$Dokumentnummer),]
data <- data[order(data$Dokumentnummer),]
expect_true(all(is.na(lss$`Dokument salgsdato`)))
expect_true(all(is.na(lss$`Dokument versjonstidspunkt`)))
expect_equal(check_tzones(lss$`Siste fangstdato`), "CET")
# put in columns not converted
lss$`Dokument salgsdato` <- data$`Dokument salgsdato`
lss$`Dokument versjonstidspunkt` <- data$`Dokument versjonstidspunkt`

expect_true(all.equal(lss, data))

#context("readErsFile: normal run")
data <- RstoxData:::readErsFile(system.file("testresources","logbooks_trimmed_2015.psv", package="RstoxData"))
expect_true(data.table::is.data.table(data))
expect_true("RC" %in% names(data))
expect_true(is.numeric(data$RUNDVEKT))
expect_true(is.numeric(data[["ST\u00D8RSTE_LENGDE"]]))
expect_equal(nrow(data),9)
expect_true("UTC" %in% attr(as.POSIXlt(data$STARTTIDSPUNKT), "tzone"))

