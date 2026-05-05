# Read a biotic file and create a StoxBiotic object:
exampleFile <- system.file("testresources","biotic_2020821.zip", package="RstoxData")
suppressWarnings(exampleData <- RstoxData::StoxBiotic(RstoxData::ReadBiotic(exampleFile)))

# Translate SpeciesCategory from "sild'G05/161722.G05/126417/NA" to "HER":
TranslationTable_1 = data.table::data.table(
	SpeciesCategory = "sild'G05/161722.G05/126417/NA",
	NewValue = "HER"
)
Translation_1 <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_1, 
	VariableName = "SpeciesCategory"
)
exampleDataTranslated_1 <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput", Translation = Translation_1)

expect_equal(sum(exampleDataTranslated_1$SpeciesCategory$SpeciesCategory == "HER"), 2)

# Translate IndividualSex to Male if IndividualTotalLength > 20. All IndividualSex are NA, so we need to use a function both for IndividualSex and for IndividualTotalLength:
TranslationTable_2 = data.table::data.table(
	IndividualSex = "function(IndividualSex) TRUE",
	NewValue = "M", 
	IndividualTotalLength = "function(IndividualTotalLength) IndividualTotalLength > 20"
)
Translation_2 <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_2, 
	VariableName = "IndividualSex", 
	Conditional = TRUE, 
	ConditionalVariableNames = "IndividualTotalLength"
)
exampleDataTranslated_2 <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput",  Translation = Translation_2)

expect_equal(sum(exampleDataTranslated_2$Individual$IndividualSex == "M", na.rm = TRUE), 135)


# Translate conditional on variables in parent tables:
TranslationTable_3 = data.table::data.table(
	IndividualTotalLength = "function(IndividualTotalLength) TRUE",
	NewValue = 999, 
	SpeciesCategory = "function(SpeciesCategory) startsWith(SpeciesCategory, \"knurr\")"
)
Translation_3 <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_3, 
	VariableName = "IndividualTotalLength", 
	Conditional = TRUE, 
	ConditionalVariableNames = "SpeciesCategory"
)
exampleDataTranslated_3 <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput",  Translation = Translation_3)

expect_equal(exampleDataTranslated_3$Individual[, sum(IndividualTotalLength > 500, na.rm = TRUE)], 10)




TranslationTable_4 = data.table::data.table(
	IndividualTotalLength = "function(IndividualTotalLength) TRUE",
	NewValue = "function(IndividualTotalLength) IndividualTotalLength * 123", 
	Gear = "function(Gear) Gear == \"3270\""
)
Translation_4 <- RstoxData::DefineTranslation(
	DefinitionMethod = "Table", 
	TranslationTable = TranslationTable_4, 
	VariableName = "IndividualTotalLength", 
	Conditional = TRUE, 
	ConditionalVariableNames = "Gear"
)
exampleDataTranslated_4 <- RstoxData::TranslateStoxBiotic(StoxBioticData = exampleData, TranslationDefinition = "FunctionInput",  Translation = Translation_4)

expect_equal(exampleDataTranslated_4$Individual[, sum(IndividualTotalLength > 1000, na.rm = TRUE)], 540)



# Test translating in the Instrument table of ICESAcoustic (metadata table, not part of the hierarchy with keys):
exampleFile_ICESAcoustic <- system.file("testresources","ICESAcoustic.xml", package="RstoxData")
suppressWarnings(exampleData_ICESAcoustic <- RstoxData::ICESAcoustic(RstoxData::ReadBiotic(exampleFile_ICESAcoustic)))


translated <- TranslateICESAcoustic(exampleData_ICESAcoustic, TranslationTable = data.table::data.table(InstrumentTransducerDepth = NA, NewValue = 1.234, InstrumentFrequency = "38"))

# The original should be NA:
expect_true(is.na(exampleData_ICESAcoustic$Instrument$InstrumentTransducerDepth))

# The translated should be 1.234:
expect_equal(translated$Instrument$InstrumentTransducerDepth, 1.234)

