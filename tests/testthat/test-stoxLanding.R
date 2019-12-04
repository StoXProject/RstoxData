context("test-stoxLanding test resource file gear")
gear <- loadResource("gear")
expect_false(any(is.na(gear$gearDescription)))

context("test-stoxLanding test resource file coastal")
coastal <- loadResource("coastal")
expect_false(any(is.na(coastal$coastalDescription)))

context("test-stoxLanding test resource file n62")
n62 <- loadResource("n62")
expect_false(any(is.na(n62$n62Description)))

context("test-stoxLanding test resource file nusage")
usage <- loadResource("usage")
expect_false(any(is.na(usage$usageDescription)))

context("test-stoxLanding")
landingXML <- readXmlFile(system.file("testresources", "landing.xml", package="RstoxData"), stream = T)
flatSL <- StoxLanding(landingXML)
expected_colums <- c("speciesFAOCommercial",
                     "speciesCategoryCommercial",
                     "commonNameCommercial",
                     "year",
                     "catchDate",
                     "gear",
                     "gearDescription",
                     "area",
                     "location",
                     "icesAreaGroup",
                     "coastal",
                     "coastalDescription",
                     "n62Code",
                     "n62Description",
                     "vesselLength",
                     "countryVessel",
                     "landingSite",
                     "countryLanding",
                     "usage",
                     "usageDescription",
                     "weight"
                     )
expect_equivalent(expected_colums, names(flatSL))
expect_true(is.numeric(flatSL$vesselLength))
expect_true(is.numeric(flatSL$weight))
expect_true(is.numeric(flatSL$year))
expect_true(is.character(flatSL$countryVessel))

context("test-stoxLanding missing values in aggColumns")
weightPre <- sum(flatSL$weight)
landingXML$Mottaker$Mottaksstasjon[2] <- NA
flatSL <- StoxLanding(landingXML)
expect_equal(sum(is.na(flatSL$landingSite)), 1)
weightPost <- sum(flatSL$weight)
expect_equal(weightPre, weightPost)

context("test-stoxLanding is.StoxLandingData")
landingXML <- readXmlFile(system.file("testresources", "landing.xml", package="RstoxData"), stream = T)
flatSL <- StoxLanding(landingXML)
expect_true(is.StoxLandingData(flatSL))
expect_false(is.StoxLandingData(landingXML))

expect_false(is.LandingData(flatSL))
expect_true(is.LandingData(landingXML))
