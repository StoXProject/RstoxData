# Satisfy R CMD check
options("mc.cores" = 2)

# NMD Biotic
filenames <- system.file("testresources","biotic_v3_example.xml", package="RstoxData")

inputData <- RstoxData:::ReadBiotic(filenames)

# test a wrong expression
filterExpression <- list()
filterExpression$`biotic_v3_example.xml`$individual <- c(
	'weight > 2'
)

#context("test-Filter: Invalid column")
# Should be warning but can proceed)
expect_warning(out <- RstoxData:::filterData(inputData, filterExpression))

# Therefore should be equal
expect_equal(inputData, out)


# fix and add expressions
filterExpression$`biotic_v3_example.xml`$individual <- c(
	'individualweight > 2'
)
filterExpression$`biotic_v3_example.xml`$agedetermination <- c(
	'age < 10',
	'serialnumber %notin% c(99484)'
)

# Should be OK
out <- RstoxData:::filterData(inputData, filterExpression)

#context("test-Filter: Filtering Biotic Data")
# Should be TRUE
expect_equal(any(out$`biotic_v3_example.xml`$individual$individualweight <= 2), FALSE)
expect_equal(any(out$`biotic_v3_example.xml`$agedetermination$age >= 10), FALSE)
expect_equal(any(out$`biotic_v3_example.xml`$agedetermination$serialnumber == 99484), FALSE)
expect_equal(all(out$`biotic_v3_example.xml`$individual$individualweight > 2), TRUE)
expect_equal(all(out$`biotic_v3_example.xml`$agedetermination$age < 10), TRUE)
expect_equal(all(out$`biotic_v3_example.xml`$agedetermination$serialnumber != c(99484)), TRUE)


## StoxBiotic
suppressWarnings(st <- RstoxData:::StoxBiotic(inputData))

filterExpressionSt <- list()
filterExpressionSt$Individual <- c(
	"IndividualAge >= 10"
)

# Should be OK
outst <-RstoxData:::filterData(st, filterExpressionSt)

#context("test-Filter: Filtering StoxBiotic data")
# Should be TRUE
expect_equal(any(outst$Individual$IndividualAge < 10), FALSE)
expect_equal(all(outst$Individual$IndividualAge >= 10), TRUE)

# Filter result consistency between level
inputData1 <- RstoxData:::ReadBiotic(filenames)
suppressWarnings(inputData2 <- RstoxData:::StoxBiotic(RstoxData:::ReadBiotic(filenames)))

filterExpression1 <- list()
filterExpression1$`biotic_v3_example.xml`$fishstation <- c(
	'serialnumber %notin% c(99483)'
)

filterExpression2 <- list()
filterExpression2$Haul <- c(
	"HaulKey %notin% c(99483)"
)

suppressWarnings(out1 <- RstoxData:::StoxBiotic(RstoxData:::filterData(inputData1, filterExpression1)))
out2 <- RstoxData:::filterData(inputData2, filterExpression2)

#context("test-Filter: Filter downward propagation")
comparison <- all.equal(out1, out2)

# Should be one difference in the Station table
expect_equal(length(comparison), 1)


# Propagate Upwards
inputData <- RstoxData:::ReadBiotic(filenames)

filterExpression <- list()
filterExpression$`biotic_v3_example.xml`$catchsample <- c(
	'commonname %notin% c("torsk", "sei", "hyse", "lange", "lysing", "gr\u00E5steinbit", "kveite")'
)

#context("test-Filter: Filter downward propagation with blank record tables in between")
outPrup <- RstoxData:::filterData(inputData, filterExpression, propagateUpwards = TRUE)
expect_equal(nrow(outPrup$`biotic_v3_example.xml`$agedetermination), 0)

#context("test-Filter: Filter upward propagation (BioticData)")
expect_equal(nrow(outPrup$biotic_v3_example.xml$fishstation), 1)


# Propagate upwards with StoxBiotic
suppressWarnings(inputData <- RstoxData:::StoxBiotic(RstoxData:::ReadBiotic(filenames)))

filterExpression <- list()
filterExpression$Sample <- c(
	'SpeciesCategoryKey %like% "breiflabb|lyr"'
)

#context("test-Filter: Filter downward + upward propagation (StoxBiotic)")
outPrup <- RstoxData:::filterData(inputData, filterExpression, propagateUpwards = TRUE)
expect_equal(nrow(outPrup$SpeciesCategory), 2)
expect_equal(nrow(outPrup$Sample), 2)
expect_equal(nrow(outPrup$Haul), 1)
expect_equal(nrow(outPrup$Station), 1)
expect_equal(nrow(outPrup$Individual), 0)


# Landing
#context("test-Filter: Landings")
landingfile <- system.file("testresources","landing.xml", package="RstoxData")
Landings <- RstoxData:::ReadLanding(landingfile)

filterExpressionL <- list()
filterExpressionL$`landing.xml`$Fangstdata <- c(
  'Hovedomr\u00E5de_kode %in% c("37", "08")'
)
filteredL <- RstoxData:::FilterLanding(Landings, filterExpressionL)
expect_equal(nrow(filteredL$landing.xml$Fangstdata), sum(Landings$landing.xml$Fangstdata[["Hovedomr\u00E5de_kode"]] %in% c("37", "08")))
expect_equal(nrow(filteredL$landing.xml$Art), nrow(Landings$landing.xml$Art))
expect_true(nrow(filteredL$landing.xml$Fangstdata) < nrow(Landings$landing.xml$Fangstdata))

filterExpressionSL <- list()
filterExpressionSL$Landing <- c(
  'Area %in% c("37", "08")'
)

filteredLprop <- RstoxData:::FilterLanding(Landings, filterExpressionL, FilterUpwards = T)
expect_equal(nrow(filteredLprop$landing.xml$Fangstdata), sum(Landings$landing.xml$Fangstdata[["Hovedomr\u00E5de_kode"]] %in% c("37", "08")))
expect_true(nrow(filteredLprop$landing.xml$Art) < nrow(Landings$landing.xml$Art))
expect_true(nrow(filteredLprop$landing.xml$Fangstdata) < nrow(Landings$landing.xml$Fangstdata))



# StoxLanding
StoxLanding <- RstoxData:::StoxLanding(Landings)
filteredSL <- RstoxData:::FilterStoxLanding(StoxLanding, filterExpressionSL)
expect_equal(nrow(filteredSL$Landing), sum(StoxLanding$Landing$Area %in% c("37","08")))
