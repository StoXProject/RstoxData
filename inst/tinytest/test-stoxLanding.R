
#context("test-stoxLanding")
landingXML <- ReadLanding(c(system.file("testresources", "landing.xml", package="RstoxData"),system.file("testresources", "landing2.xml", package="RstoxData")))
flatSL <- StoxLanding(landingXML)$Landing
expected_colums <- c("Species",
                     "Year",
                     "CatchDate",
                     "Gear",
                     "Area",
                     "Location",
                     "Coastal",
                     "N62Code",
                     "VesselLength",
                     "CountryVessel",
                     "LandingSite",
                     "CountryLanding",
                     "Usage",
                     "RoundWeight"
                     )
expect_equivalent(expected_colums, names(flatSL))
expect_true(is.numeric(flatSL$RoundWeight))
expect_true(is.numeric(flatSL$Year))
expect_true(is.character(flatSL$CountryVessel))
expect_true(length(flatSL$CatchDate) > 1 & "POSIXct" %in% class(flatSL$CatchDate))

#context("test-stoxLanding missing values in aggColumns")
weightPre <- sum(flatSL$RoundWeight)
landingXML$landing.xml$Mottaker$Mottaksstasjon[2] <- NA
flatSL <- StoxLanding(landingXML)$Landing
expect_equal(sum(is.na(flatSL$LandingSite)), 1)
weightPost <- sum(flatSL$RoundWeight)
expect_equal(weightPre, weightPost)

#context("test-stoxLanding is.StoxLandingData")
landingXML <- ReadLanding(system.file("testresources", "landing.xml", package="RstoxData"))
SL <- StoxLanding(landingXML)
expect_true(is.StoxLandingData(SL))
expect_false(is.StoxLandingData(landingXML))

expect_false(is.LandingData(SL))
expect_true(is.LandingData(landingXML))
