# check wrong format
example <- system.file("testresources","biotic_v1.4_example.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
expect_error(RstoxData::PrepareNmdBioticTable(biodata, "Mission"))

# check multiple files
example <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
example2 <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(FileNames = c(example, example2))
Mission <- RstoxData::PrepareNmdBioticTable(biodata, "Mission")
expect_equal(nrow(biodata$biotic3.1_example.xml$mission) + nrow(biodata$biotic_v3_example.xml$mission), nrow(Mission))

# check multiple files with duplicates
example <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
example2 <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(FileNames = c(example, example2))
expect_error(RstoxData::PrepareNmdBioticTable(biodata, "Mission"), "'BioticData' contains duplicate missions: 9-2020-9000-1, 9-2020-9000-1")

# test prep mission
example <- system.file("testresources","biotic3.1_example.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
Mission <- RstoxData::PrepareNmdBioticTable(biodata, "Mission")
expect_equal(nrow(Mission), nrow(biodata$biotic3.1_example.xml$mission))

# test prep station
Station <- RstoxData::PrepareNmdBioticTable(biodata, "Station")
expect_equal(nrow(Station), nrow(biodata$biotic3.1_example.xml$fishstation))
expect_true(ncol(Station)>ncol(biodata$biotic3.1_example.xml$fishstation))

# test prep sample
Sample <- RstoxData::PrepareNmdBioticTable(biodata, "Sample")
expect_equal(nrow(Sample), nrow(biodata$biotic3.1_example.xml$catchsample))
expect_true(ncol(Sample)>ncol(biodata$biotic3.1_example.xml$catchsample))

# test prep individual
Individual <- RstoxData::PrepareNmdBioticTable(biodata, "Individual")
expect_equal(nrow(Individual), nrow(biodata$biotic3.1_example.xml$individual))
expect_true(ncol(Individual)>ncol(biodata$biotic3.1_example.xml$individual))
expect_true("age" %in% names(Individual))
expect_equal(sum(is.na(Individual$age)),4)

#test with ages and tags
example <- system.file("testresources","biotic3.1_w_ageandprey_tags.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
Individual <- RstoxData::PrepareNmdBioticTable(biodata, "Individual")
expect_equal(nrow(Individual), nrow(biodata$biotic3.1_w_ageandprey_tags.xml$individual))
expect_equal(sum(!is.na(Individual$tagid)), 1)

#test with ages and multiple tags
bioseveraltags <- RstoxData::ReadBiotic(example)
bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag <- rbind(bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag[1,], bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag)
bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag$tagid[1]<-5
expect_warning(Individual <- RstoxData::PrepareNmdBioticTable(bioseveraltags, "Individual"), "Data contains individuals with several tags. Tags have not been added for these individuals.")
expect_equal(nrow(Individual), nrow(bioseveraltags$biotic3.1_w_ageandprey_tags.xml$individual))
expect_equal(sum(!is.na(Individual$tagid)), 0)

#test with actual ages
example <- system.file("testresources","biotic3.1_w_ageandprey.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
Individual <- RstoxData::PrepareNmdBioticTable(biodata, "Individual")
expect_equal(nrow(Individual), nrow(biodata$biotic3.1_w_ageandprey.xml$individual))
expect_true(ncol(Individual)>ncol(biodata$biotic3.1_w_ageandprey.xml$individual))
expect_true("age" %in% names(Individual))
expect_equal(sum(!is.na(Individual$age)),2)

#test with several ages
bioseveralages <- biodata
bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination <- rbind(bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination[1,], bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination)
expect_error(RstoxData::PrepareNmdBioticTable(bioseveralages, "Individual"), "Data contains individuals with several age readings, but without indication of preferredage. E.g. age:  9-2020-9000-1-1-1-1 ...")
bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination$agedeterminationid[1]<-5
Individual <- RstoxData::PrepareNmdBioticTable(bioseveralages, "Individual")
expect_equal(nrow(Individual), nrow(bioseveralages$biotic3.1_w_ageandprey.xml$individual))
expect_equal(nrow(Individual), 2)
expect_equal(nrow(bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination), 3)

# test prep MultipleReadings
MultipleReadings <- RstoxData::PrepareNmdBioticTable(bioseveralages, "MultipleReadings")
expect_equal(nrow(MultipleReadings), nrow(bioseveralages$biotic3.1_w_ageandprey.xml$agedetermination))

# test prep Prey
example <- system.file("testresources","biotic3.1_w_ageandprey.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
Prey <- RstoxData::PrepareNmdBioticTable(biodata, "Prey")
expect_equal(nrow(Prey), nrow(biodata$biotic3.1_w_ageandprey.xml$prey))
expect_true(ncol(Prey)>ncol(biodata$biotic3.1_w_ageandprey.xml$prey))

# test prep MultipleTags
example <- system.file("testresources","biotic3.1_w_ageandprey_tags.xml", package="RstoxData")
bioseveraltags <- RstoxData::ReadBiotic(example)
bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag <- rbind(bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag[1,], bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag)
bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag$tagid[1]<-5
Tags <- RstoxData::PrepareNmdBioticTable(bioseveraltags, "MultipleTags")
expect_equal(nrow(Tags), nrow(bioseveraltags$biotic3.1_w_ageandprey_tags.xml$tag))
expect_equal(sum(is.na(Tags$tagid)), 0)

# test prep PreyLengthFrequencies
example <- system.file("testresources","biotic3.1_w_preyfreqTables.xml", package="RstoxData")
biodata <- RstoxData::ReadBiotic(example)
PreyLengthFreq <- RstoxData::PrepareNmdBioticTable(biodata, "PreyLengthFrequencies")
expect_equal(nrow(PreyLengthFreq), nrow(biodata$biotic3.1_w_preyfreqTables.xml$preylengthfrequencytable))
expect_true(ncol(PreyLengthFreq)>ncol(biodata$biotic3.1_w_preyfreqTables.xml$preylengthfrequencytable))

# test prep PreyDevstageFrequencies
PreyDsFreq <- RstoxData::PrepareNmdBioticTable(biodata, "PreyDevstageFrequencies")
expect_equal(nrow(PreyDsFreq), nrow(biodata$biotic3.1_w_preyfreqTables.xml$copepodedevstagefrequencytable))
expect_true(ncol(PreyDsFreq)>ncol(biodata$biotic3.1_w_preyfreqTables.xml$copepodedevstagefrequencytable))

#test adding Ids
warning("Test adding ids")
Station <- RstoxData::PrepareNmdBioticTable(biodata, "Station", addIds=T)
Individual <- RstoxData::PrepareNmdBioticTable(biodata, "Individual", addIds=T)

warning("Add test that checks that ids match what StoxBiotic assigns")

warning("Check that no reserved names occur as column names, if addIds is true")