example <- system.file("testresources","landing.xml", package="RstoxData")
example_duplicates <- system.file("testresources","landing_duplicates.xml", package="RstoxData")
example_duplicates_several <- system.file("testresources","landing_duplicates_several.xml", package="RstoxData")

#Regular read
land <- RstoxData:::ReadLanding(example)

#check that duplicates give warning
expect_warning(landDup <- RstoxData:::ReadLanding(example_duplicates), "Landings in landing_duplicates.xml contain duplicate key records. Consider the option ForceUnique or correct this in some other way.")
totalWeigth <- sum(landDup$landing_duplicates.xml$Produkt$Rundvekt)

#check that duplicate correction works
expect_equal(nrow(merge(landDup$landing_duplicates.xml$Produkt, landDup$landing_duplicates.xml$Seddellinje, by=RstoxData::xsdObjects$landingerv2.xsd$tableHeaders$Seddellinje[1:13])), nrow(landDup$landing_duplicates.xml$Seddellinje)+2)
land2 <- RstoxData:::ReadLanding(example_duplicates, ForceUnique=T)
expect_equal(nrow(merge(land2$landing_duplicates.xml$Produkt, land2$landing_duplicates.xml$Seddellinje, by=RstoxData::xsdObjects$landingerv2.xsd$tableHeaders$Seddellinje[1:13])), nrow(land2$landing_duplicates.xml$Seddellinje))
expect_equal(sum(land2$landing_duplicates.xml$Produkt$Rundvekt), totalWeigth)

#check that duplicate correction works with several duplicates, and triples as well as doubles.
expect_warning(landDup <- RstoxData:::ReadLanding(example_duplicates_several), "Landings in landing_duplicates_several.xml contain duplicate key records. Consider the option ForceUnique or correct this in some other way.")
totalWeigth <- sum(landDup$landing_duplicates_several.xml$Produkt$Rundvekt)
expect_equal(nrow(merge(landDup$landing_duplicates_several.xml$Produkt, landDup$landing_duplicates_several.xml$Seddellinje, by=RstoxData::xsdObjects$landingerv2.xsd$tableHeaders$Seddellinje[1:13])), nrow(landDup$landing_duplicates_several.xml$Seddellinje)+8)
land2 <- RstoxData:::ReadLanding(example_duplicates_several, ForceUnique=T)
expect_equal(nrow(merge(land2$landing_duplicates_several.xml$Produkt, land2$landing_duplicates_several.xml$Seddellinje, by=RstoxData::xsdObjects$landingerv2.xsd$tableHeaders$Seddellinje[1:13])), nrow(land2$landing_duplicates_several.xml$Seddellinje))
expect_equal(sum(land2$landing_duplicates_several.xml$Produkt$Rundvekt), totalWeigth)