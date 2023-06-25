# RstoxData v1.10.0-9006  (2023-06-25)

* Added better warning when at least one of bottomdepthstart and bottomdepthstop are missing.


# RstoxData v1.10.0-9003  (2023-06-01)

* Improved warning when variables that cannot be converted to numeric as requested by the XSD are set to NA in ReadAcoustic()/ReadBiotic().


# RstoxData v1.10.0-9001  (2023-05-22)

* Fixed bug when translating with PreserveClass = FALSE, in which case the class change could corrupt the condition of the translation (e.g., translating a copy of the DateTime in the Log table of the StoxAcousticData would convert that to string after the first line of the TranslationTable, and then any time condition will fail, as the class is no longer POSIX).


# RstoxData v1.9.0  (2023-04-18)

## Summary
* This release contains improvements and bug-fixes in the ICESDatras() and the RegroupLengthICESDatras() functions.

## Genaral changes
* Updated documentation of StoxAcoustic and StoxBiotic with tables defining each variable.
* Removed variable names related to the NMCBiotic tables "prey", "preylengthfrequencytable" and "copepodedevstagefrequencytable".
* Added unit specification of StoxAcoustic and StoxBiotic variables.
* Changed RegroupLengthICESDatras() to regroup lengths both in the HL and the CA table, and also to support recalculating both HLNoAtLngt and CANoAtLngt. Also added the parameters ResolutionTableVariables and ResolutionTable to support specices specific (or other variables) regrouping.

## Bug fixes
* Fixed bug in RegroupLengthICESDatras(), where the columns were reordered with ResolutionTableVariables first.
* Fixed bug in ICESDatras() where distance was first rounded in nautical miles and then multiplied by 1852.
* Fixed bug in translateOneTranslationOneTable() used by Translate-functions, where type conversion was applied before applying the translation, which for a function such as IndividualAge > 9 resulted in 10, 11, ... to be comared as text and thus not translated.

## Warnings
* Fixed snprintf warnnings.
* Added a warning if there are more than one tag for at least one individual in AddToStoxBiotic() for NMDBiotic >= 3 files.
* Corrected warning " There are more than one 'serialnumber' ..." to end with "More than one serialnumber for the following cruise/station (of the fishstation table of the BioticData):" instead of "Duplicated serialnumber for the following cruise/station (of the fishstation table of the BioticData):".
* Corrected warning for more NASC in B than in P.
* Corrected warninig for non-supported NMDEhcosounder format from >= 1.4 to >= 1.1.


# RstoxData v1.9.0-9003  (2023-03-06)
* Fixed bug in RegroupLengthICESDatras(), where the columns were reordered with ResolutionTableVariables first.
* Added a warning if there are more than one tag for at least one individual in AddToStoxBiotic() for NMDBiotic >= 3 files.
* Removed variable names related to the NMCBiotic tables "prey", "preylengthfrequencytable" and "copepodedevstagefrequencytable".
* Fixed bug in ICESDatras() where distance was first rounded in nautical miles and then multiplied by 1852.


# RstoxData v1.9.0-9002  (2023-03-06)
* Updated documentation of StoxAcoustic and StoxBiotic with tables defining each variable.
* Added unit specification of StoxAcoustic and StoxBiotic variables.
* Removed error when filtering BioticData read from ICESBiotic and ICESAcoustic XML.
* Fixed snprintf warnnings.


# RstoxData v1.9.0-9001  (2023-01-19)
* Corrected warning " There are more than one 'serialnumber' ..." to end with "More than one serialnumber for the following cruise/station (of the fishstation table of the BioticData):" instead of "Duplicated serialnumber for the following cruise/station (of the fishstation table of the BioticData):".
* Corrected warning for more NASC in B than in P.
* Corrected warninig for non-supported NMDEhcosounder format from >= 1.4 to >= 1.1.
* Removed the StoX XML from https://acoustic.ices.dk/submissions.
* Fixed bug in translateOneTranslationOneTable() used by Translate-functions, where type conversion was applied before applying the translation, which for a function such as IndividualAge > 9 resulted in 10, 11, ... to be comared as text and thus not translated.
* Changed HaulNo to use the serialnumber and not the station variable of NMDBiotic >= 3 in ICESDatras().
* Changed RegroupLengthICESDatras() to regroup lengths both in the HL and the CA table, and also to support recalculating both HLNoAtLngt and CANoAtLngt. Also added the parameters ResolutionTableVariables and ResolutionTable to support specices specific (or other variables) regrouping.


# RstoxData v1.8.0  (2023-01-13)
* Changed error to warning when FilterUpwards from the top table.
* Added support for the string "NA" in WriteICESAcoustic() (used in the field DataProcessingTriwaveCorrection) (and also in WriteICESBiotic()). 


# RstoxData v1.8.0-9007  (2023-01-11)
* Disabled removing namespace prefix in readXmlFile(), which used xslt::xml_xslt().


# RstoxData v1.8.0-9006  (2023-01-09)
* Added support for NMDEchosounder files in ICESAcoustic().
* Added defaults for GroupingVariables and AggregationVariables in RegroupLengthICESDatras().
* Reorderd TranslationTable to after VariableName, Conditional and ConditionalVariableNames in Translate functions.
* Moved do.call_robust() from RstoxFramework to RstoxData.


# RstoxData v1.8.0-9005  (2022-12-23)
* Cleaned up data.R.


# RstoxData v1.8.0-9004  (2022-12-16)
* Fixed bug with filterpropagation on branched data formats. E.g. agereadings not being removed when corresponding individuals where removed in biotic, and clarified documentation (#241, #240).
* Fixed bug when RstoxData functions relying on the global variable xsdObjects was called without RstoxData being loaded ("263")
* Fixed bug with reading some logbook-variants (#237)


# RstoxData v1.8.0-9003  (2022-12-12)
* Added CopyBiotic, CopyStoxBiotic, CopyICESBiotic, CopyICESDatras, CopyAcoustic, CopyStoxAcoustic, CopyICESAcoustic, CopyLanding and CopyStoxLanding, used for copying one column to another (possibly existing) column.
* Fixed bug in Translate functions where PreserveClass = TRUE had no effect.
* Updated documentation of DefineTranslation and the Translate functions.
* Added drop-down lists for the parameters VariableName and ConditionalVariableNames, and for valueColumn, newValueColumn and conditionalValueColumns in the case that the table is read from a file.
* Fixed bug in StoxBiotic() from NMDBiotic <= 1.4 files, where platform from the mission table was used as CatchPlatform. Changed to using the platform from the fishstation.
* Added support for adding variables in AddToStoxBiotic() that originate from CopyBiotic().
* Fixed bug in Translate functions, where PreserveClass = FALSE did not work.


# RstoxData v1.8.0-9002  (2022-12-02)
* Changed check-full.yaml to run for 'release' and specific R versions (4.2, 4.1, etc).


# RstoxData v1.8.0-9001  (2022-11-30)
* Unified how lengtht code is calculated for ICESBiotic and ICESDatras.
* Added RegroupLengthICESDatras() (replacing PrepareWriteICESDatras()), which regroups the lengths of the HL table of ICESDatras.
* Added lengthResolutionTable and lengthCode_unit_table to Definitions.R.
* Fixed bug in pkgdown.yaml.
* Added support for pre-releases, which are not deployed to the StoX repo.


# RstoxData v1.7.5  (2022-11-17)
* Added the function PrepareWriteICESDatras().
* Fixed typo in ICESDatras() (BycSpecRecCode changed to BySpecRecCode).
* Added specialstage in Maturity in CA.
* Added the LiverWeight in CA, with values from the liverweight in NMDBiotic


# RstoxData v1.7.3-9002  (2022-11-17)
* Fixed typo in warning for more sa in B than in P.
* Added a unique for the Log table in StoxAcoustic for ICESAcoutsic.
* Added units for area number density, and one for megaton.
* Added the exported hasUnit().


# RstoxData v1.7.3-9001  (2022-11-15)
* Fixed bug in ReadAcoustic when an ICESAcoustic xml files contains more than one instrument.


# RstoxData v1.7.2  (2022-11-13)
* Fixed bug with R < 4.2, where a filter process with unspecified FilterExpression retuns error "zero-length inputs cannot be mixed with those of non-zero length". The error is returned both when opening the FilterExpression and when running the process.


# RstoxData v1.7.1-9002  (2022-10-31)
* Pre-release before 1.7.1. Errors are expected.
* Added warning when ch_type P is missing or represent less sa than B.


# RstoxData v1.7.1-9001  (2022-09-12)
* Pre-release before 1.7.1. Errors are expected.
* Added the exported functions WriteAcoustic() and WriteBiotic().
* Changed error to warning when type is not convertible in writeXmlFile().
* Improved warnings in StoxBiotic() when missing values are generated for different producttype etc.
* Cleaned up use of xsdObjects, which is now always used directly and not via RstoxData::xsdObjects.
* Added applyFunctionArgumentHierarchy() for use in RstoxFramework and in any other Rstox packages in need for evaluating the function argument hierarchy.
* Added doc for WriteAcoustic, WriteBiotic, FunctionArgumentHierarchy.
* Renamed match_arg() to match_arg_informative().
* Added warning for duplicated station in NMDBiotic, which leads to more than one Haul per Station. This is not supported when assigning Hauls in the map, where all Hauls of a Station are selected. Filtering out Hauls can be a solution.
* Exported printErrorIDs().


# RstoxData v1.7.0  (2022-08-11)
* Improved warnings AcousticDataToICESAcousticOne() when values are not found in ICES reference tables.
* Start of using semantic versioning (https://semver.org/). Before this release the two first version numbers represented the major and minor release number, in accordance with semantic versioning, whereas the third version number identified test versions. The major and minor releases (versions ending with 0.0 or 0) were considered as official versions. From this release and onwards, the third version number will represent patches (bug fixes), and are to be considered equally official as the major and minor releases. In fact, as patches are restricted to fixing bugs and not adding new functionality, the latest patch will be the recommended version.


# RstoxData v1.6.8  (2022-08-07)
* Fixed bug in getLogKey_ICESAcoustic().
* Disabled warning in Translate-functions when a table contained some but not all of the variables of the Translation. 
* Added warning when SampleCount is used instead of the new SampleNumber in a filter, asking the user to change the filter. 
* Updated the ICES XSDs.


# RstoxData v1.6.7  (2022-06-22)
* Restored the LogKey to the form where seconds are dropped in StoxAcousticData from ICESAcoustic files with minute resolution, to avoid issues with processData in existing StoX projects.
* Removed LogKey and EDSU from AcousticData read from ICESAcoustic files, which were added by reference in StoxAcoustic(). Added data.table::copy to fix this.
* Added test for allowed file extensions in readXmlFile() to avoid R crashing when e.g. RData file is accidentally used.
* Fixed temporary bug where StoxTimeZone was used from Definitions.R in processBioticData.R when it is not defined before onLoad.


# RstoxData v1.6.6  (2022-06-19)
* Fixed bug in the JSON schema of the Translation process data, where number, string and boolean were allowed for the NewValue field, in that order, whereas string and null is correct.
* Fixed bug in as.POSIXct_ICESAcoustic() where the minute resoslution was tested first, causing loss of available seconds.


# RstoxData v1.6.5  (2022-06-17)
* Fixed bug in StoxAcoustic() where LogKey in ICESAcoustic data was misspecified when using time format "YYYY-MM-DDThh:mm" or "YYYY-MM-DD hh:mm" (without seconds). The seconds (00) are now included.
* Fixed bug in StoxBiotic() where the time format "YYYY-MM-DDThh:mm" in an ICESBiotic xml files was truncated to only the date.


# RstoxData v1.6.4  (2022-06-15)
* Added exported functions MergeAcoustic(), unMergeAcoustic() and unMergeBiotic().
* Added exported function setColumnClasses(). 


# RstoxData v1.6.2  (2022-05-13)
* Replaced the logical AddToLowestTable by the string SplitTableAllocation in AddToStoxBiotic(), allowing for allocating variables to either the default, highest or lowest table when splitting tables StoxBiotic.
* Removed hard coded values for the following variables on ICESDatras() (variable name -> new value): 
	
	+ Table HH
	*Country -> nation
	*Ship -> platformname
	*SweepLngt -> NA
	*GearEx -> NA
	*DayNight -> NA
	*StatRec -> area + location (concatenation)
	*HaulVal -> NA
	*Distance -> distance (in meters)
	*GroundSpeed -> vesselspeed

	+ Table HL
	*SpecVal -> NA
	*LenMeasType -> lengthmeasurement
	
	+ Table CA
	*Maturity -> NA
	*MaturityScale -> NA
	*AgeSource -> agingstructure
	*OtGrading -> readability (only if agingstructure is 2)
	*PlusGr -> NA

* Removed hard coded values for the following variables on ICESBiotic(): 
	*Platform -> platformname
	*Validity -> NA
	*StatisticalRectangle -> area + location


# RstoxData v1.6.1  (2022-05-12)
* Fixed bug in DateTime in StoxBioticData, where milliseconds were pasted twice if present in the input data. 
* Fixed bug reported in https://jira.imr.no/browse/STOX-544, occurring when splitting catchsample into SpeciesCategory and Sample, by unique() in firstPhase(). 
* Added exported function match_arg(). Modified writeXmlFile.R and added WriteAcoustic().


# RstoxData v1.5.18  (2022-03-29)
* Improved warnings when catchproducttype, sampleproducttype, individualproducttype or lengthmeasurement does not have the appropriate value.


# RstoxData v1.5.17  (2022-03-22)
* Added the parameter AddToLowestTable in AddToStoxBiotic(), which can be used for adding variables from tables in NMDBiotic or ICESBiotic that are split into two tables in StoxBiotic (fishstation and catchsample in NMDBiotic and Haul and Catch in ICESBiotic). When these tables are split into two tables StoX decides which variable should be placed in each table. E.g., geographical position is placed in the Station table of StoxBiotic, which implies that only the first position of several hauls that comprise one Station is kept. If one needs all positions, AddToLowestTable can be set to TRUE so that the positions are placed in the Haul table instead of the Station table of StoxBiotic.


# RstoxData v1.5.16  (2022-03-14)
* Added warning when at least one of bottomdepthstart and bottomdepthstop are missing, so that BottomDepth is NA.


# RstoxData v1.5.15  (2022-03-10)
* Included more informative warning when e.g. product types are not the required value in StoxBiotic().


# RstoxData v1.5.13  (2022-03-02)
* Updated to the new ICESAcoustic and ICESBiotic format (released March 2022).


# RstoxData v1.5.11  (2022-02-28)
* Increased significant digits of small numbers foomo 6 to 12.


# RstoxData v1.5.10  (2022-02-25)
* Relaxed the warning for translation columns not present in the data to accept if all are not present (unaffected table).
* Moved application of setRstoxPrecisionLevel() from each StoX function to RstoxFramework::runProcess().
* Added SpatialPolygonsDataFrame and matrix to setRstoxPrecisionLevel().


# RstoxData v1.5.9 (2022-02-14)
* Added the parameters TranslationDefinition, TranslationTable, VariableName, Conditional and ConditionalVariableNames to all Translate functions, specifically TranslateAcoustic(), TranslateBiotic(), TranslateICESAcoustic(), TranslateICESBiotic(), TranslateICESDatras(), TranslateLanding(), TranslateStoxAcoustic(), TranslateStoxBiotic() and TranslateStoxLanding(). This allows for specifying the Translation as a table in the Translate function, without the need for DefineTranslation(). DefineTranslation() can still be used, and must be used if reading the Translation from a file.
* Added sanitizer for DefineTranlation(), as the Translation process data is evaluated in Translate functions.


# RstoxData v1.5.7 (2022-01-31)
* Postponed ICESDatras_New().
* Changed the process data Translation from a table with columns VariableName, Value, NewValue, ConditionalVariableName, ConditionalValueColumn, to a table of the variable to translate in the first column; the column NewValue giving the values to translate to in the second column; followed by zero or more conditional variables. This supports multiple conditional variables, but restricts to translating only one variable at the time (although the old table i still supported, but cannot be generated in the GUI). 
* Added support for NAs and other values in the same Translation process data.
* Added support for specifying a function as a string in Translation process data, usefull e.g. for setting fish larger than som value to mature.
* Renamed ConditionalVariableName to ConditionalVariableNames and ConditionalValueColumn to ConditionalValueColumns, as multiple values are now supported.
* Fixed bug in ICESDatras() occurring when there were rows with the same aphia and species, but with missing sex.
* Accepting NAs in convAgeSource().


# RstoxData v1.5.5 (2022-01-31)
* Added the functions FilterICESAcoustic(), FilterICESBiotic(), FilterICESDatras() and TranslateICESDatras().
* Changed the output of ICESBiotic(), ICESDatras() and ICESAcoustic() to combine tables from the different files in the same way that StoxBiotic() and StoxAcoustic() does. This also affects the name of the output of WriteICESAcoustic(), WriteICESBiotic() and WriteICESDatras(), which is no longer named by the input file to the Read* function.
* Added warning when a filter species a table that does not exist in the data.
* Fixed bug where log-distances of duplicated LogKey (sometimes occurring when times are on minutes instead of seconds resolution) were not properly removed in all tables.
* Removed unwanted and in some cases erroneous hard coded values in ICESDatras(), as listed in the following table:
	* Gear: Changed from "GOV" to the NMDBiotic/fishstation variable gear
	* DoorType: Changed from "P" to the NMDBiotic/fishstation variable trawldoortype
	* DoorSurface: Changed from 4 to the NMDBiotic/fishstation variable trawldoorarea = 4.5,
	* DoorWgt: Changed from 1075 to the NMDBiotic/fishstation variable trawldoorweight = 1075
	* KiteDim: Changed from 0.8 to NA_real_, as there is no relevant information in NMDBiotic
	* lngtCode: Changed from interpreting herring/sprat and crustatians to using RstoxData:::getLengthCodeICES()
	* Removed all filtering by species. This should rather be done in FilterBiotic() or rather FilterICESDatras()


# RstoxData v1.5.2 (2022-01-22)
* Added warnings for when (catch)producttype != 1, (sample)producttype != 1, (individual)producttype != 1, or lengthmeasurement != 'E'. 

* Added support for NMDBiotic files of mixed version (<= and > 1.4) in AddToStoxBiotic() (removing the prey table and other unused tables, as consistent link to the individual table is not provided by the XML schema.). 
* Fixed bug causing stream = TRUE to fail on MacOS Monterey in readXmlFile().


# RstoxData v1.3.5 (2022-01-09)
* Hiding the parameters VariableName and ConditionalVariableName when DefinitionMethod = "Table" in Translation().
* Renamed CatchFractionCount to CatchFractionNumber and SampleCount to SampleNumber in StoxBiotic. CatchFractionCount was considered misleading, as this variable is often not a result of counting but rather an estimate from total and sampled weight.


# RstoxData v1.3.3 (2022-01-09)
* Changed type of the columns of the Translation process data to accept strings, numeric and boolean (preivously restricted to string).


# RstoxData v1.3.2 (2022-01-07)
* Changed TranslationTable to Table in DefineTranslation(). Fixed bug where NA in Tanslation was not converted properly to the type of the existing data. Added change of class in ICESBiotic() as per the XSD. Fixed bug in AddToStoxBiotic(), where variables from agedetermination in NMDBiotic >= 3 were not added. Code changed to use the xsd to determine the variables that can be added.


# RstoxData v1.2.20 (2021-12-02)
* Final version for the release of StoX 3.2.0.


# RstoxData v1.2.20 (2021-12-02)
* Added warning when ValueColumn, NewValueColumn or ConditionalValueColumn did not exist in the file in DefineTranslation().
* Added return value from setorderv_numeric().


# RstoxData v1.2.18 (2021-11-22)
* Reverted to the original createOrderKey() of StoX 3.1.0, used in setorderv_numeric() and further in RstoxBase::formatOutput(), in order to produce the same seeds in RstoxBase::ImputeSuperIndividuals().
* Added parameters VariableName, ConditionalVariableName and ConditionalValueColumn to DefineTranslation(), to support full flexibility of column names in the resource file. Also added the parameter PreserveClass to Translate* functions, specifying whether to allow for the translation to change class of the data, e.g. form integer to string. Specified NAs in ICESBiotic() to the class defined by ICES.


# RstoxData v1.2.13 (2021-11-04)
* Fixed bug in readVariableTranslation() (where eval() was used instead of get()).


# RstoxData v1.2.12 (2021-10-26)
* Added warning when adding a variable that already exists in AddToStoxBiotic(), particularly aimed at SpeciesCategory in ICESBiotic, which has a different meaning that SpeciesCategory in StoxBioticData.


# RstoxData v1.2.11 (2021-10-23)
* Added the parameters ValueColumn and NewValueColumn to DefineTranslaion().


# RstoxData v1.2.10 (2021-10-20)
* Added CompensateEffectiveTowDistanceForFishingDepthCount() for NMDBiotic data with hauls made at several depths.


# RstoxData v1.2.7 (2021-10-08)
* Fixed error in links to documentation in RstoxData.
* Final version for the release of StoX 3.2.0.


# RstoxData v1.2.6 (2021-09-28)
* Final version for the release of StoX 3.2.0.
* Added class to stox-biotic-final-phase.csv, used when generating an empty table in StoxBiotic().
* Added support for empty tables in StoxBiotic() and in setPrecisionLevelOneDT(). 
* Refactored StoxBiotic() for ICESBiotic xml files with NumberAtLength given.


# RstoxData v1.2.3 (2021-08-18)
* Fixed formatting of output from WriteICESBiotic() so that precision is kept and values are not padded with blanks and zeros. Fixed bug in writeXmlFile().
* Fixed bug where TranslateStoxBiotic() and similar functions changed type of the data, so that translating numeric values did not work properly.
* Changed Distance in ICESBiotic() to distance * 1852.


# RstoxData v1.2.1 (2021-07-13)
* Fix bundled miniz to not using unaligned access. This should fix CRAN's gcc-UBSAN check.
* Bump miniz and pugixml to the latest release version.
* Fixed bug where TranslateStoxBiotic() and similar functions changed type of the data, so that translating numeric values did not work properly. 


# RstoxData v1.2.0 (2021-06-18)
* Final version for the release of StoX 3.1.0.


# RstoxData v1.1.16 (2021-06-16)
* Fixed bug in StoxBiotic(), where date and time were borrowed from Station to Haul, which could crash due to missing values in StationKey.
* Removed interpretation of agingstructure in ICESBiotic().


# RstoxData v1.1.13 (2021-06-07)
* Changed to sort in en_US_POSIX-locale in createOrderKey() using the stringi-package, which ensures platform independence while replicating sorting done by data.table.


# RstoxData v1.1.9 (2021-05-21)
* Fixed bugs and added auto-detect xsd for reading zipped xml files.
* Fixed bug in StoxAcoustic for ICESAcoustic data, where log-distances with no acoustic records were deleted.
* Changed to sort in C-locale in createOrderKey() using the stringi-package, to comply with data.table's philosophy of platform independence.


# RstoxData v1.1.6 (2021-05-04)
* Added sanitizeFilter() to avoid system calls in filter.
* Removed hard coded conversions in ICESBiotic(), moving the responsibility of such conversions to the translation functions.
* Optimized createOrderKey() for faster execution.
* Renamed ReportICESAcoustic(), ReportICESBiotic() and ReportICESDatras() to WriteICESAcoustic(), WriteICESBiotic() and WriteICESDatras(), respectievly.


# RstoxData v1.1.5 (2021-04-18)
* Added TranslateICESAcoustic() and TranslateICESBiotic().
* Added option of a conditional variable in DefineTranslation() and Translate*().
* Removed maturity conversion in ICESBiotic().
* Fixed format of columns of ReportICESBiotic().


# RstoxData v1.1.2 (2021-03-30)
* Fixed time format of StoxAcoustic().
* Moved translation of ICESBiotic before merging levels as merging changes name of some variables.
* Changed to keep original FishID and add sequetial integers for individuals regenerated from Catch continuing from the maximum FishID.
* Added documentation of the StoxBiotic format.
* Fixed bug in LengthResolution for ICESBiotic where only the first value was used.
* Fixed bug when converting length for ICESBiotic, where values were multiplied by 100 instead of 10 from mm to cm.
* Changed TowDistance to nautical miles.
* __NEW__: Reading XML files with namespace prefix is now supported.
* __NEW__: Writing XML files (alpha) is now supported.
* Fixed bug in DateTime for ICESAcoustic files, and bug in translateOneTable() causing incomplete translation in StoxAcoustic from ICESAcoustic files.
* Added ChannelDepthUpper-ChannelDepthLower as Channel in StoxAcousic. 
* Fixed bug in createOrderKey() where columns that are non-convertable to nunmeric were replaced by NA instead of being left unchanged
* Fixed bug in `filterData`, where `propagateUpwards` = TRUE did not remove rows of the higher tables if these rows were not present in the filtered table.
* Corrected type of variables of `StoxAcoustic` for NMDEchosounder input xml files.
* Added possible values for `redefinitionTable`.
* Renamed `readVariableConversion()` to `readVariableTranslation()`.
* Refactored `translateVariables()`.
* Throw error when file is missing in `readXmlFile()`.


# RstoxData v1.1.1 (2021-02-23)
* Refactor some of the reading functions to get rid of `readr` dependency.
* New feature: ECA integration. This is merged some time ago.


# RstoxData v1.1.0 (2021-02-10)
* Final version for the release of StoX 3.0.0.


# RstoxData v1.0.28 (2021-02-08)
* Renamed TowedDistance to TowDistance and EffectiveTowedDistance to EffectiveTowDistance. 
* Interpret keys as numeric (possibly separated by slash) if possiible when ordering StoxAcoustic and StoxBiotic. 
 
 
# RstoxData v1.0.26 (2021-02-02)
* Added support for NMDBiotoic1.4 and NMDBiotoic1.1 in StoxBiotic(). 


# RstoxData v1.0.25 (2021-01-28)
* Added DefineTranslation and TranslateAcoustic, TranslateBiotic, TranslateLanding and TranslateStoxLanding.
* Removed all Convert-functions. These may be added later.
* Added/fixed FilterLanding and FilterStoxLanding.
* Added all agedetermination variables in BioticData2GeneralSamplingHierarchy(), making these available for AddToStoxBiotic.


# RstoxData v1.0.24 (2021-01-21)
* `getStoxKeys()`: Fix for `stoxBioticObject` object not found
(https://github.com/StoXProject/RstoxData/issues/117).
* Remove sorting when merging in `AddToStoxBiotic()`.
* Rename `DefineStoxBioticTranslation` to `DefineTranslation`, and the coresponding
data type to `Translation`. Also added backward compatibility for this.


# RstoxData v1.0.23 (2021-01-13)
* `ICESDatras`: remove reference to SurveyName and addSurveyType parameters.
* Add reporting functions for ICES exports (`ReportICES*()`).
* Add Intercatch data parser (`parseInterCatch()`).
* Add `roundDrop0()` to replace `round()` as a more robust rounding function.


# RstoxData v1.0.20 (2021-01-06)
* `getICESShipCode`: improve ship conversion by removing the deprecated entries and sorting.
* `ICESDatras`: remove reference to SurveyName parameter.
* Move `readr` to suggests.
* Made `compareICES()` robust to missing internet connection.
* `ICESDatras` returning matrix to be written as csv by `RstoxFramework::runProcess`.
* Cleaned up translations using vocabulary for ICES data.
* Fixed bug with DateTime in `StoxAcoustic()`.
* Added `backwardCompatibility` and removed `NumberOfCores`.
* Refactored the functions for writing ICESBiotic and ICESAcoustic files to include NMDBioticToICESBiotic() and to use similar methods.


# RstoxData v1.0.17 (2020-11-23)
* In `writeICESDatras()`: Change country code 'NOR' to 'NO'.
* Change the default NumberOfCores to 1L in all parallel-able functions.
* Refactor `WriteICESAcoustic()`, `WriteICESBiotic()`, `WriteICESDatras()` into
`prepareICESAcoustic()`, `prepareICESBiotic()`, `prepareICESDatras()`.
* Tests: do not attempt to copy file outside `tempdir()` in `test-readXmlFile.R` file.
* Github actions: Update `check-full.yaml` file.
* `prepareICESAcoustic()` to use `data.table` and not use `format()`.
* Fix parallel `lapplyOnCores()` behavior in Windows platform.
* Remove `StoxAcousticStartMiddleStopDateTime()` function.


# RstoxData v1.0.16 (2020-11-11)
* Prepare for CRAN submission: Remove all attempts to modify the global environment.
* Prepare for CRAN submission: Shorten the package title.
* Update variables in `processDataSchema.json`.
* Delete `zzz.R` and `pkgname.R` and moved the contents to `RstoxData-package.R`.


# RstoxData v1.0.15 (2020-11-05)
* Added a `NEWS.md` file to track changes to the package.
* Added a `cran-comments.md` file for CRAN submission.
* `StoxExport` functions are now writing output files in `tempdir()`.
* Various small fixes for adhering to CRAN policies.


# RstoxData v1.0.14 (2020-10-29)
* New minor release.


# RstoxData v1.0.14 (2020-10-01)
* Changed SampleKey to catchsampleid.


# RstoxData v1.0.12 (2020-09-30)
* StoxExport: Adopt the latest unified DATRAS format.
* StoxExport: Fix function for getting ICES ship code.
* stoxBioticObject: Fix DateTime column from ICES Biotic has a different class compared to the one from NMD Biotic.
* Documentation fix.


# RstoxData v1.0.11 (2020-09-18)
* New minor release.


# RstoxData v1.0.10 (2020-09-08)
* New `getStoxKeys()`.


# RstoxData v1.0.9 (2020-09-07)
* Added `mergeByStoxKeys()`.


# RstoxData v1.0.8 (2020-09-01)
* Added support for undefined `signifDigits`.


# RstoxData v1.0.7 (2020-09-01)
* Modified `setPrecisionLevelOneDT()` to keep at least 6 significant digits.


# RstoxData v1.0.6 (2020-08-28)
* Removed unit in variable and parameter names.


# RstoxData v1.0.5 (2020-08-25)
* Added IDs separated by '-'.


# RstoxData v1.0.4 (2020-08-21)
* New minor release.


# RstoxData v1.0.3 (2020-08-15)
* Fix small bug in `DefineDataTranslation()`.


# RstoxData v1.0.2 (2020-08-14)
* Added `Cruise` in `StoxAcoustic`.


# RstoxData v1.0.1.9001 (2020-07-17)
* Added `Cruise` in the Cruise table of `StoxBiotic`.


# RstoxData v1.0.1.9000 (2020-07-17)
* Added Redefine, Translate and Convert.


# RstoxData v1.0.1 (2020-07-11)
* Changed `AcousticCategory` to character.


# RstoxData v0.8.15 (2020-06-21)
* Added warning for non-unique `LogKey`.


# RstoxData v0.8.14 (2020-06-16)
* Added `AddStoxBioticVariables()` and ConversionType in ConvertStoxBiotic.


# RstoxData v0.8.13 (2020-06-11)
* Add NMD Biotic format v3.1 support.


# RstoxData v0.8.12 (2020-06-10)
* Renamed Cores to NumberOfCores and LengthInterval to LengthIntervalCentimeters.


# RstoxData v0.8.11.9000 (2020-06-08)
* Filter: Filtering upwards now able to skip empty tables.


# RstoxData v0.8.11 (2020-06-06)
* Use Github actions.


# RstoxData v0.8.10.9002 (2020-06-02)
* Appveyor: fix logic.


# RstoxData v0.8.10.9001 (2020-06-02)
* Appveyor: test logic.


# RstoxData v0.8.10 (2020-06-02)
* New minor release.


# RstoxData v0.8.9 (2020-05-28)
* Minor update to stox acoustic.


# RstoxData v0.8.8 (2020-05-19)
* Minor update to stox acoustic.


# RstoxData v0.8.7 (2020-05-08)
* Minor update to stox acoustic.


# RstoxData v0.8.6.9001 (2020-05-05)
* Use strict lss format.


# RstoxData v0.8.6 (2020-05-04)
New features:
* Add multiple data export feature:
  1. ICES acoustic XML format to ICES acoustic CSV format
  2. NMD biotic v3 XML format to ICES biotic CSV format
  3. NMD biotic V3 XML format to NS-IBTS ICES Datras (CSV) format

Bug fixes:
* Fixes ICES Acoustic/Biotic vocabulary generation when parsing ICES's XML files


# RstoxData v0.8.5 (2020-05-01)
New features:
* Add filtering with upward propagation 

Bugfixes:
* Propagate down now able to skip empty tables (e.g., NMD v3 prey)


# RstoxData v0.8.4 (2020-04-28)
* New minor release.


# RstoxData v0.8.3 (2020-04-16)
* Add define and update variables.


# RstoxData v0.8.2 (2020-04-07)
* More fixes in stox acoustic.


# RstoxData v0.8.1 (2020-04-02)
* Fixes in stox acoustic.


# RstoxData v0.8.0.9002 (2020-04-01)
* Minor bug fix.


# RstoxData v0.8.0 (2020-04-01)
* New stox acoustic.


# RstoxData v0.7.0 (2020-03-27)
* Fixed bug with non-unique tables from ICESAcoustic.


# RstoxData v0.7 (2020-03-26)
* New stox acoustic.


# RstoxData v0.6.7 (2020-03-19)

New minor release. Notable new features are the support for NMD Biotic v1.4 and ICES Biotic data input.

Changelog:
* Updated README
* Increment version
* Add test for StoxBiotic
* Add ICES Biotic to StoXBiotic format conversion
* readXmlFile: Fix typo
* Add support for converting NMD Biotic v1.4 format to StoxBiotic


# RstoxData v0.6.6 (2020-02-11)
* New minor release.


# RstoxData v0.6.5 (2019-12-19)
* New minor release.

Changelog:
* Update README(.md)
* Increment version
* Refresh xsdObjects data
* Fix documentation
* Add verbose output switch for reading XMLs
* Add tests for StoxBiotic and StoxAcoustic functions
* Fix merging proses in StoxAcoustic() after correction of the column types
* Correct the data types in result tables
* Fix StoxBiotic function


# RstoxData v0.6.4 (2019-12-18)

Updated XML reader, RstoxBiotic and RstoxAcoustic functions.

Changes:
* Update DESCRIPTION
* Appveyor needs the whole R version digit
* Increment minor version
* Update CIs to use only R 3.5
* Merge branch 'biotic_acoustic' of github.com:StoXProject/RstoxData into biotic_acoustic
* Filter draft
* XML stream read speed improvements
* Result tables is now ordered
* Suppress warning on utils data
* metadata from readXmlFile is now a data.table
* Update ICES XSDs raw and compiled data objects
* Update stoxBioticObject data
* Fix processBioticData length resolution converter
* icesBiotic is now supported
* Fix NMDBiotic v.1x xsdObjects
* Added documentation for DataTypes
* Merge branch 'biotic_acoustic' of github.com:StoXProject/RstoxData into biotic_acoustic
* Added documentation for BioticData
* StoxBiotic: Remove duplicate rows from SpeciesCategory table
* Update data for conversion
* Latitude and Latitude2
* Latest StoxBiotic updates
* Merge branch 'biotic_acoustic' of github.com:StoXProject/RstoxData into biotic_acoustic
* Minor fix
* Fix (another) encoding problem
* Fix documents
* Update StoxBiotic conversion data and scripts
* Clean up data.table warnings and inefficiencies
* Removed PDFs
* Add StoxAcoustic and StoxBiotic


# RstoxData v0.6.3 (2019-12-06)
* New minor release, various bug fixes.


# RstoxData v0.6.2 (2019-11-26)
* New minor release, mainly for various bug fixes.


# RstoxData v0.6.1 (2019-06-21)
* New Feature: Reading ICES acoustic XML files.


# RstoxData v0.6.0 (2019-06-12)
Changes:
* Rebrand to RstoxData.
* Add support for all biotic formats.

# RNMDAPI v0.5.1 (2019-05-27)

Changes:
* BUG FIX: Correctly handle decimal type in the resulting tables.

# RNMDAPI v0.5.0 (2019-04-09)

Changes:
* Supports reading ZIP compressed XML files in the push XML parser. This function is built on top of MINIZ compression library.

# RNMDAPI v0.4.0 (2019-04-06)
Changes:
* Faster and more memory efficient in reading XML files.
* New XML engine, all XML processing are now done inside the C++ code.
* A new streaming pull parser is implemented to avoid reading the whole big XML files into memory.
* Now supports landings v2 XML format.
