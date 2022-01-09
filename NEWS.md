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

# stoxData v0.8.10 (2020-06-02)
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
