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
#expect_equal(check_tzones(wh$`Siste fangstdato`), "CET") # Removed since R 4.3 changed check_tzones to .check_tzones

#context("Test convertLandings")
landings <- RstoxData:::convertToLandingData(data)
expect_true(all(landings$ConvertedData[["Fart\u00F8y"]][["Fart\u00F8ynasjonalitet_kode.Fart\u00F8y"]] == landings$ConvertedData[["Fart\u00F8y"]][["Fart\u00F8ynasjonalitet_kode"]]))
expect_true(RstoxData::is.LandingData(landings))
expect_equal(sum(landings$ConvertedData$Produkt$Rundvekt), sum(data$Rundvekt))
sl <- RstoxData::StoxLanding(landings)
expect_true(RstoxData::is.StoxLandingData(sl))

#context("Test convertLandings with duplicates")
lss_w_duplicates <- system.file("testresources","lss_duplicates.psv", package="RstoxData")
dupdata <- RstoxData::readLssFile(lss_w_duplicates)
expect_warning(landings_dup<-RstoxData:::convertToLandingData(dupdata))
landings_fixed <- RstoxData:::convertToLandingData(dupdata, ForceUnique = T)
expect_equal(sum(landings_fixed$ConvertedData$Produkt$Rundvekt), sum(landings_dup$ConvertedData$Produkt$Rundvekt))
expect_equal(sum(duplicated(paste(landings_fixed$ConvertedData$Seddellinje$Dokumentnummer, landings_fixed$ConvertedData$Seddellinje$Linjenummer))),0)
expect_equal(sum(duplicated(paste(landings_dup$ConvertedData$Seddellinje$Dokumentnummer, landings_dup$ConvertedData$Seddellinje$Linjenummer))),1)


#context("Test convertToLssData")
lss <- RstoxData:::convertToLssData(landings)
lss <- lss[order(lss$Dokumentnummer),]
data <- data[order(data$Dokumentnummer),]
expect_true(all(is.na(lss$`Dokument salgsdato`)))
expect_true(all(is.na(lss$`Dokument versjonstidspunkt`)))
#expect_equal(check_tzones(lss$`Siste fangstdato`), "CET") # Removed since R 4.3 changed check_tzones to .check_tzones
# put in columns not converted
lss$`Dokument salgsdato` <- data$`Dokument salgsdato`
lss$`Dokument versjonstidspunkt` <- data$`Dokument versjonstidspunkt`

expect_true(all.equal(lss, data))

#context("readErsFile: normal run")
data <- RstoxData:::readErsFile(system.file("testresources","logbooks_trimmed_2015.psv", package="RstoxData"))
expect_equal(sum(is.na(data$LOKASJON_START)), 2) # one coded as |""| one coded as ||
expect_equal(sum(is.na(data$LOKASJON_STOP)), 2) # one coded as |""| one coded as ||
expect_true(data.table::is.data.table(data))
expect_true("RC" %in% names(data))
expect_true(is.numeric(data$RUNDVEKT))
expect_true(is.numeric(data[["ST\u00D8RSTE_LENGDE"]]))
expect_equal(nrow(data),9)
expect_true("UTC" %in% attr(as.POSIXlt(data$STARTTIDSPUNKT), "tzone"))

expect_warning(dataMissingColumns <- RstoxData:::readErsFile(system.file("testresources","logbooks_trimmed_2011.psv", package="RstoxData")))
expect_true(all(names(dataMissingColumns) == names(data)))
expect_true(all(!is.na(dataMissingColumns$RUNDVEKT)))
expect_true(all(!is.na(dataMissingColumns$RC)))
expect_true(all(is.na(dataMissingColumns$MASKEVIDDE)))
expect_true(all(is.na(dataMissingColumns$PUMPET_FRA)))
expect_true(all(is.na(dataMissingColumns$AKTIVITET)))
expect_true(all(is.na(dataMissingColumns$AKTIVITET_KODE)))

