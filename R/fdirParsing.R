
#' Parses landings (sales notes)
#' @description
#'  Parses sales notes data from the Norwegian Directorate of Fisheries (FDIR) on the LSS format
#' @details
#'  The LSS format is a pipe-separated format encoding landings (sales-notes).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#'
#'  Historically, columns in the landings provided from FDIR has been adapted for each data delivery
#'  Lately data deliveries has become standardized, but in order to support variants
#'  adherence to the standardization is not enforced by this function, unless option 'strict' is selected.
#'  If column names does not match specification, but data is otherwise parse-able, a warning will be issued.
#'  
#'  If the parameter 'strict' is not TRUE, data types may be inferred from data.
#' @param file path to file with LSS landings
#' @param encoding encoding for 'file', must be accepted by \code{\link[data.table]{fread}}
#' @param guessMax deprecated parameter, has no effect.
#' @param strict enforce strict adherence to data format.
#' @return data.table with LSS landings
#' @importFrom data.table as.data.table
#' @export
readLssFile <- function(file, encoding="Latin-1", guessMax = 100000, strict=T){
  
  spec_land <- list(
    Dokumentnummer = "character",
    `Dokumenttype (kode)` = "character",
    Dokumenttype = "character",
    `Dokument versjonsnummer` = "character",
    `Dokument salgsdato` = "character",
    `Dokument versjonstidspunkt` = "character",
    `Salgslag ID` = "character",
    `Salgslag (kode)` = "character",
    Salgslag = "character",
    `Mottakernasjonalitet (kode)` = "character",
    Mottakernasjonalitet = "character",
    Mottaksstasjon = "character",
    `Landingskommune (kode)` = "character",
    Landingskommune = "character",
    `Landingsfylke (kode)` = "character",
    Landingsfylke = "character",
    `Landingsnasjon (kode)` = "character",
    Landingsnasjon = "character",
    Produksjonsanlegg = "character",
    `Produksjonskommune (kode)` = "character",
    Produksjonskommune = "character",
    `Fiskerkommune (kode)` = "character",
    Fiskerkommune = "character",
    `Fiskernasjonalitet (kode)` = "character",
    Fiskernasjonalitet = "character",
    Fartoynavn = "character",
    `Fartoy ID` = "character",
    `Registreringsmerke (seddel)` = "character",
    `Radiokallesignal (seddel)` = "character",
    `Storste lengde` = "numeric",
    `Lengdegruppe (kode)` = "character",
    Lengdegruppe = "character",
    `Bruttotonnasje 1969` = "numeric",
    `Bruttotonnasje annen` = "numeric",
    Byggear = "integer",
    Ombyggingsar = "integer",
    Motorkraft = "numeric",
    Motorbyggear = "integer",
    `Fartoy gjelder fra dato` = "character",
    `Fartoy gjelder til dato` = "character",
    `Fartoytype (kode)` = "character",
    Fartoytype = "character",
    `Kvotefartoy reg.merke` = "character",
    `Fartoykommune (kode)` = "character",
    Fartoykommune = "character",
    `Fartoyfylke (kode)` = "character",
    Fartoyfylke = "character",
    `Fartoynasjonalitet (kode)` = "character",
    Fartoynasjonalitet = "character",
    `Mottakende fartoy reg.merke` = "character",
    `Mottakende fartoy rkal` = "character",
    `Mottakende fartoytype (kode)` = "character",
    `Mottakende fart.type` = "character",
    `Mottakende fartoynasj. (kode)` = "character",
    `Mottakende fart.nasj` = "character",
    Fangstar = "integer",
    `Siste fangstdato` = "character",
    `Kvotetype (kode)` = "character",
    Kvotetype = "character",
    `Redskap (kode)` = "character",
    Redskap = "character",
    `Redskap - hovedgruppe (kode)` = "character",
    `Redskap - hovedgruppe` = "character",
    `Fangstfelt (kode)` = "character",
    `Kyst/hav (kode)` = "character",
    `Hovedomrade (kode)` = "character",
    Hovedomrade = "character",
    `Lokasjon (kode)` = "character",
    `Sone (kode)` = "character",
    Sone = "character",
    Omradegruppering = "character",
    `Hovedomrade FAO (kode)` = "character",
    `Hovedomrade FAO` = "character",
    `Nord/sor for 62 grader nord` = "character",
    `Fangstdagbok (nummer)` = "character",
    `Fangstdagbok (turnummer)` = "character",
    Landingsdato = "character",
    Landingsklokkeslett = "character",
    `Dellanding (signal)` = "character",
    `Neste mottaksstasjon` = "character",
    `Forrige mottakstasjon` = "character",
    Linjenummer = "integer",
    `Art - FDIR (kode)` = "character",
    `Art - FDIR` = "character",
    `Art - gruppe (kode)` = "character",
    `Art - gruppe` = "character",
    `Art - hovedgruppe (kode)` = "character",
    `Art - hovedgruppe` = "character",
    `Art FAO (kode)` = "character",
    `Art FAO` = "character",
    `Produkttilstand (kode)` = "character",
    Produkttilstand = "character",
    `Konserveringsmate (kode)` = "character",
    Konserveringsmate = "character",
    `Landingsmate (kode)` = "character",
    Landingsmate = "character",
    `Kvalitet (kode)` = "character",
    Kvalitet = "character",
    `Storrelsesgruppering (kode)` = "character",
    `Anvendelse (kode)` = "character",
    Anvendelse = "character",
    `Anvendelse hovedgruppe (kode)` = "character",
    `Anvendelse hovedgruppe` = "character",
    `Antall stykk` = "integer",
    Bruttovekt = "numeric",
    Produktvekt = "numeric",
    Rundvekt = "numeric"
  )
  names(spec_land)[26] <- "Fart\u00F8ynavn"
  names(spec_land)[27] <- "Fart\u00F8y ID"
  names(spec_land)[30] <- "St\u00F8rste lengde"
  names(spec_land)[35] <- "Bygge\u00E5r"
  names(spec_land)[36] <- "Ombyggings\u00E5r"
  names(spec_land)[38] <- "Motorbygge\u00E5r"
  names(spec_land)[39] <- "Fart\u00F8y gjelder fra dato"
  names(spec_land)[40] <- "Fart\u00F8y gjelder til dato"
  names(spec_land)[41] <- "Fart\u00F8ytype (kode)"
  names(spec_land)[42] <- "Fart\u00F8ytype"
  names(spec_land)[43] <- "Kvotefart\u00F8y reg.merke"
  names(spec_land)[44] <- "Fart\u00F8ykommune (kode)"
  names(spec_land)[45] <- "Fart\u00F8ykommune"
  names(spec_land)[46] <- "Fart\u00F8yfylke (kode)"
  names(spec_land)[47] <- "Fart\u00F8yfylke"
  names(spec_land)[48] <- "Fart\u00F8ynasjonalitet (kode)"
  names(spec_land)[49] <- "Fart\u00F8ynasjonalitet"
  names(spec_land)[50] <- "Mottakende fart\u00F8y reg.merke"
  names(spec_land)[51] <- "Mottakende fart\u00F8y rkal"
  names(spec_land)[52] <- "Mottakende fart\u00F8ytype (kode)"
  names(spec_land)[54] <- "Mottakende fart\u00F8ynasj. (kode)"
  names(spec_land)[56] <- "Fangst\u00E5r"
  names(spec_land)[66] <- "Hovedomr\u00E5de (kode)"
  names(spec_land)[67] <- "Hovedomr\u00E5de"
  names(spec_land)[71] <- "Omr\u00E5degruppering"
  names(spec_land)[72] <- "Hovedomr\u00E5de FAO (kode)"
  names(spec_land)[73] <- "Hovedomr\u00E5de FAO"
  names(spec_land)[74] <- "Nord/s\u00F8r for 62 grader nord"
  names(spec_land)[93] <- "Konserveringsm\u00E5te (kode)"
  names(spec_land)[94] <- "Konserveringsm\u00E5te"
  names(spec_land)[95] <- "Landingsm\u00E5te (kode)"
  names(spec_land)[96] <- "Landingsm\u00E5te"
  names(spec_land)[99] <- "St\u00F8rrelsesgruppering (kode)"
  
  sel <- names(spec_land)
  typ <- unlist(spec_land, use.names = F)
  
  if (strict){
    headers <- names(data.table::fread(file, sep="|", colClasses = c("character"), header = T, dec=",", strip.white=TRUE, na.strings=c("", "na", "NA"), nrows = 1, encoding = encoding, showProgress=F))
    
    if (length(headers) != length(spec_land)){
      stop("Number of columns in file does not match specification.")
    }
    if (!all(headers == sel)){
      differences <- sum(headers != sel)
      warning(paste("StoX: Header names does not match specification,", differences, "column names differ."))
    }
      
    db <- data.table::fread(file, sep="|", header = T, colClasses = typ, dec=",", strip.white=TRUE, na.strings=c("", "na", "NA"), encoding = encoding)
    names(db) <- sel
    db$`Siste fangstdato` <- as.POSIXct(db$`Siste fangstdato`, format='%d.%m.%Y', tz="CET")
  }
  else{
    db <- data.table::fread(file, sep="|", header = T, dec=",", strip.white=TRUE, na.strings=c("", "na", "NA"), encoding = encoding, keepLeadingZeros=T)
    if ("Siste fangstdato" %in% names(db)){
      db$`Siste fangstdato` <- as.POSIXct(db$`Siste fangstdato`, format='%d.%m.%Y', tz="CET")
    }
  }
  
  return(db)
}


#' Read pipe separated file with specified columns
#' @noRd
read_psv <- function(file, encoding, col_types){
  db <- data.table::fread(file, sep="|", header =T, strip.white=TRUE, na.strings=c("", "na", "NA"), dec=",", colClasses = col_types, encoding=encoding)
  return(db)
}

#' Parses logbooks (ERS) 
#' @description 
#'  Parses electronic logbooks (ERS) from tabular format delivered by Directorate of Fisheries (FDIR)
#' @details 
#'  The format is a pipe-separated format encoding aggregated ERS records (logbooks).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#' @param file path to file
#' @param encoding encoding for 'file', must be accepted by \code{\link[data.table]{fread}}
#' @return data.table() with logbooks
#' @export
readErsFile <- function(file, encoding="Latin-1"){
  
  spec_log <- list(
    RC = "character",
    REGM = "character",
    STORSTE_LENGDE = "numeric",
    BRUTTOTONNASJE = "integer",
    MOTORKRAFT = "integer",
    TM1 = "character",
    AKTIVITET_KODE = "character",
    AKTIVITET = "character",
    PUMPET_FRA = "character",
    FANGSTAR = "integer",
    STARTTIDSPUNKT = "character",
    START_LT = "numeric",
    START_LG = "numeric",
    SONE = "character",
    KVOTETYPE_KODE = "character",
    KVOTETYPE = "character",
    REDSKAP_FAO = "character",
    REDSKAP_NS = "character",
    REDSKAP = "character",
    REDSKAPSSPESIFIKASJON_KODE = "character",
    REDSKAPSSPESIFIKASJON = "character",
    MASKEVIDDE = "integer",
    REDSKAP_PROBLEMER_KODE = "character",
    REDSKAP_PROBLEMER = "character",
    STOPPTIDSPUNKT = "character",
    STOPP_LT = "numeric",
    STOPP_LG = "numeric",
    VARIGHET = "integer",
    INNSATS = "numeric",
    SILD_BESTAND_KODE = "character",
    SILD_BESTAND_NS = "character",
    SILD_BESTAND = "character",
    HOVEDART_FAO = "character",
    HOVEDART_NS = "character",
    HOVEDART = "character",
    INT_OMR_GML_START = "character",
    INT_OMR_NY_START = "character",
    INT_OMR_GML_STOPP = "character",
    INT_OMR_NY_STOPP = "character",
    HAV_DYBDE_START = "numeric",
    HAV_DYBDE_STOPP = "numeric",
    LOKASJON_START = "character",
    LOKASJON_STOPP = "character",
    TREKK_AVSTAND_METER = "integer",
    FANGSTART_FAO = "character",
    FANGSTART_NS = "character",
    FANGSTART = "character",
    RUNDVEKT = "numeric"
  )
  
  names(spec_log) <- c(names(spec_log)[1:2], "ST\u00D8RSTE_LENGDE", names(spec_log)[4:9], "FANGST\u00C5R", names(spec_log)[11:length(spec_log)])
  
  #sel <- names(spec_log)
  typ <- unlist(spec_log)
  
  logb <- read_psv(file, encoding, col_types=typ)
  logb$STARTTIDSPUNKT <- as.POSIXct(logb$STARTTIDSPUNKT, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  logb$STOPPTIDSPUNKT <- as.POSIXct(logb$STOPPTIDSPUNKT, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  
  return(logb)
}

#' Convert landings
#' @description 
#'  Convert landings to \code{\link[RstoxData]{LandingData}}.
#' @details 
#'  When converting from lss:
#'  All columns of 'lssLandings' are converted, except 'salgsdato' and 'versjonstidspunkt'.
#'  All columns of \code{\link[RstoxData]{LandingData}} are filled, except 'DokumentFormulardato' and 'DokumentElektroniskDato'.
#' @param lssLandings landings in the format provided by \code{\link[RstoxData]{readLssFile}}
#' @return \code{\link[RstoxData]{LandingData}} the converted landings
#' @export
convertToLandingData <- function(lssLandings){
  xsdObject <- RstoxData::xsdObjects$landingerv2.xsd
  
  #name mapping between lss and landingerv2.xsd
  nameMap <- list("Dokumentnummer"="Dokumentnummer",
                    "Dokumenttype (kode)"="Dokumenttype_Kode",
                    "Dokumenttype"="Dokumenttype_Bokm\u00E5l",
                    "Dokument versjonsnummer"="DokumentVersjonsnummer",
                    "Dokument salgsdato"=NA,
                    "Dokument versjonstidspunkt"=NA,
                    "Salgslag ID"="SalgslagID",
                    "Salgslag (kode)"="Salgslag_kode",
                    "Salgslag"="Salgslag",
                    "Mottakernasjonalitet (kode)"="Mottakernasjonalitet_kode",
                    "Mottakernasjonalitet"="Mottakernasjonalitet_bokm\u00E5l",
                    "Mottaksstasjon"="Mottaksstasjon",
                    "Landingskommune (kode)"="Landingskommune_kode",
                    "Landingskommune"="Landingskommune",
                    "Landingsfylke (kode)"="Landingsfylke_kode",
                    "Landingsfylke"="Landingsfylke",
                    "Landingsnasjon (kode)"="Landingsnasjon_kode",
                    "Landingsnasjon"="Landingsnasjon_bokm\u00E5l",
                    "Produksjonsanlegg"="Produksjonsanlegg",
                    "Produksjonskommune (kode)"="Produksjonskommune_kode",
                    "Produksjonskommune"="Produksjonskommune",
                    "Fiskerkommune (kode)"="Fiskerkommune_kode",
                    "Fiskerkommune"="Fiskerkommune",
                    "Fiskernasjonalitet (kode)"="Fiskernasjonalitet_kode",
                    "Fiskernasjonalitet"="Fiskernasjonalitet_bokm\u00E5l",      
                    "Fartoynavn"="Fart\u00F8ynavn",
                    "Fartoy ID"="Fart\u00F8yID",           
                    "Registreringsmerke (seddel)"="Registreringsmerke_seddel",
                    "Radiokallesignal (seddel)"="Radiokallesignal_seddel",
                    "Storste lengde"="St\u00F8rsteLengde",
                    "Lengdegruppe (kode)"="Lengdegruppe_kode",
                    "Lengdegruppe"="Lengdegruppe_bokm\u00E5l",
                    "Bruttotonnasje 1969"="Bruttotonnasje1969",
                    "Bruttotonnasje annen"="BruttotonnasjeAnnen",
                    "Byggear"="Bygge\u00E5r",
                    "Ombyggingsar"="Ombyggings\u00E5r",
                    "Motorkraft"="Motorkraft",
                    "Motorbyggear"="Motorbygge\u00E5r",   
                    "Fartoy gjelder fra dato"="Fart\u00F8yGjelderFraDato",
                    "Farto gjelder til dato"="Fart\u00F8yGjelderTilDato",
                    "Fartotype (kode)"="Fart\u00F8ytype_kode",
                    "Fartotype"="Fart\u00F8ytype_bokm\u00E5l",
                    "Kvotefartoy reg.merke"="Kvotefart\u00F8yRegMerke",
                    "Fartoykommune (kode)"="Fart\u00F8ykommune_kode",
                    "Fartoykommune"="Fart\u00F8ykommune",
                    "Fartoyfylke (kode)"="Fart\u00F8yfylke_kode",
                    "Fartoyfylke"="Fart\u00F8yfylke",
                    "Fartoynasjonalitet (kode)"="Fart\u00F8ynasjonalitet_kode",
                    "Fartoynasjonalitet"="Fart\u00F8ynasjonalitet_bokm\u00E5l",
                    "Mottakende farto reg.merke"="MottakendeFart\u00F8yRegMerke",
                    "Mottakende farto rkal"="MottakendeFart\u00F8yRKAL",
                    "Mottakende fartotype (kode)"="MottakendeFart\u00F8ytype_kode",
                    "Mottakende fart.type"="MottakendeFart\u00F8ytype_bokm\u00E5l",
                    "Mottakende fartoynasj. (kode)"="MottakendeFart\u00F8ynasj_kode",
                    "Mottakende fart.nasj"="MottakendeFart\u00F8ynasj_bokm\u00E5l",
                    "Fangstar"="Fangst\u00E5r",
                    "Siste fangstdato"="SisteFangstdato" ,
                    "Kvotetype (kode)"="Kvotetype_kode",
                    "Kvotetype"="Kvotetype_bokm\u00E5l",
                    "Redskap (kode)"="Redskap_kode",
                    "Redskap"="Redskap_bokm\u00E5l",
                    "Redskap - hovedgruppe (kode)"="HovedgruppeRedskap_kode",
                    "Redskap - hovedgruppe"="HovedgruppeRedskap_bokm\u00E5l",
                    "Fangstfelt (kode)"="Fangstfelt_kode",
                    "Kyst/hav (kode)"="KystHav_kode",
                    "Hovedomrade (kode)"="Hovedomr\u00E5de_kode",
                    "Hovedomrade"="Hovedomr\u00E5de_bokm\u00E5l",
                    "Lokasjon (kode)"="Lokasjon_kode",
                    "Sone (kode)"="Sone_kode",
                    "Sone"="Sone_bokm\u00E5l",
                    "Omradegruppering"="Omr\u00E5degruppering_bokm\u00E5l",
                    "Hovedomrade FAO (kode)"="Hovedomr\u00E5deFAO_kode",
                    "Hovedomrade FAO"="Hovedomr\u00E5deFAO_bokm\u00E5l",
                    "Nord/sor for 62 grader nord"="NordS\u00F8rFor62GraderNord",
                    "Fangstdagbok (nummer)"="Fangstdagbok_nummer",
                    "Fangstdagbok (turnummer)"="Fangstdagbok_turnummer",
                    "Landingsdato"="Landingsdato",
                    "Landingsklokkeslett"="Landingsklokkeslett",
                    "Dellanding (signal)"="Dellanding_signal",
                    "Neste mottaksstasjon"="NesteMottaksstasjon",
                    "Forrige mottakstasjon"="ForrigeMottakstasjon",
                    "Linjenummer"="Linjenummer",
                    "Art - FDIR (kode)"="Art_kode",
                    "Art - FDIR"="Art_bokm\u00E5l",
                    "Art - gruppe (kode)"="ArtsgruppeHistorisk_kode",
                    "Art - gruppe"="ArtsgruppeHistorisk_bokm\u00E5l",
                    "Art - hovedgruppe (kode)"="HovedgruppeArt_kode",
                    "Art - hovedgruppe"="HovedgruppeArt_bokm\u00E5l",
                    "Art FAO (kode)"="ArtFAO_kode",
                    "Art FAO"="ArtFAO_bokm\u00E5l",
                    "Produkttilstand (kode)"="Produkttilstand_kode",
                    "Produkttilstand"="Produkttilstand_bokm\u00E5l",
                    "Konserveringsmate (kode)"="Konserveringsm\u00E5te_kode",
                    "Konserveringsmate"="Konserveringsm\u00E5te_bokm\u00E5l",
                    "Landingsmate (kode)"="Landingsm\u00E5te_kode",
                    "Landingsmate"="Landingsm\u00E5te_bokm\u00E5l",
                    "Kvalitet (kode)"="Kvalitet_kode",
                    "Kvalitet"="Kvalitet_bokm\u00E5l",
                    "Storrelsesgruppering (kode)"="St\u00F8rrelsesgruppering_kode",
                    "Anvendelse (kode)"="Anvendelse_kode",
                    "Anvendelse"="Anvendelse_bokm\u00E5l",
                    "Anvendelse hovedgruppe (kode)"="HovedgruppeAnvendelse_kode",
                    "Anvendelse hovedgruppe"="HovedgruppeAnvendelse_bokm\u00E5l",
                    "Antall stykk"="AntallStykk",
                    "Bruttovekt"="Bruttovekt",
                    "Produktvekt"="Produktvekt",
                    "Rundvekt"="Rundvekt")
  
  
  names(nameMap)[26] <- "Fart\u00F8ynavn"
  names(nameMap)[27] <- "Fart\u00F8y ID"
  names(nameMap)[30] <- "St\u00F8rste lengde"
  names(nameMap)[35] <- "Bygge\u00E5r"
  names(nameMap)[36] <- "Ombyggings\u00E5r"
  names(nameMap)[38] <- "Motorbygge\u00E5r"
  names(nameMap)[39] <- "Fart\u00F8y gjelder fra dato"
  names(nameMap)[40] <- "Fart\u00F8y gjelder til dato"
  names(nameMap)[41] <- "Fart\u00F8ytype (kode)"
  names(nameMap)[42] <- "Fart\u00F8ytype"
  names(nameMap)[43] <- "Kvotefart\u00F8y reg.merke"
  names(nameMap)[44] <- "Fart\u00F8ykommune (kode)"
  names(nameMap)[45] <- "Fart\u00F8ykommune"
  names(nameMap)[46] <- "Fart\u00F8yfylke (kode)"
  names(nameMap)[47] <- "Fart\u00F8yfylke"
  names(nameMap)[48] <- "Fart\u00F8ynasjonalitet (kode)"
  names(nameMap)[49] <- "Fart\u00F8ynasjonalitet"
  names(nameMap)[50] <- "Mottakende fart\u00F8y reg.merke"
  names(nameMap)[51] <- "Mottakende fart\u00F8y rkal"
  names(nameMap)[52] <- "Mottakende fart\u00F8ytype (kode)"
  names(nameMap)[54] <- "Mottakende fart\u00F8ynasj. (kode)"
  names(nameMap)[56] <- "Fangst\u00E5r"
  names(nameMap)[66] <- "Hovedomr\u00E5de (kode)"
  names(nameMap)[67] <- "Hovedomr\u00E5de"
  names(nameMap)[71] <- "Omr\u00E5degruppering"
  names(nameMap)[72] <- "Hovedomr\u00E5de FAO (kode)"
  names(nameMap)[73] <- "Hovedomr\u00E5de FAO"
  names(nameMap)[74] <- "Nord/s\u00F8r for 62 grader nord"
  names(nameMap)[93] <- "Konserveringsm\u00E5te (kode)"
  names(nameMap)[94] <- "Konserveringsm\u00E5te"
  names(nameMap)[95] <- "Landingsm\u00E5te (kode)"
  names(nameMap)[96] <- "Landingsm\u00E5te"
  names(nameMap)[99] <- "St\u00F8rrelsesgruppering (kode)"
  
  stopifnot(all(names(nameMap) %in% names(lssLandings)))
  
  #change column names to those in xsdObj
  remove <- names(lssLandings)[is.na(nameMap[names(lssLandings)])]
  lssLandings <- lssLandings[,.SD, .SDcols=!remove]
  names(lssLandings) <- unlist(nameMap[names(lssLandings)])
  
  #add id column, but don't set it to anything
  suppressWarnings(lssLandings$id <- as.character(NA))
  
  #add unfilled columns to make sure setting of types are correctly indexed,
  # but remove them afterwards
  suppressWarnings(lssLandings$DokumentFormulardato <- as.character(NA))
  suppressWarnings(lssLandings$DokumentElektroniskDato <- as.character(NA))
  
  #set date to character
  lssLandings$SisteFangstdato <- format(lssLandings$SisteFangstdato, format = "%d.%m.%Y")
  ConvertedData <- list()
  # divide into different tables
  for (n in names(xsdObject$tableHeaders)){
    if (length(xsdObject$tableHeaders[[n]])>0){
      ConvertedData[[n]] <- lssLandings[,.SD, .SDcols=xsdObject$tableHeaders[[n]]]
    }
    else{
      ConvertedData[[n]] <- data.table::data.table()
    }
  }
  
  # Deal with duplicated column the same way as readXmlFile
  if (any(duplicated(names(ConvertedData[["Fart\u00F8y"]])))){
    tabnames <- names(ConvertedData[["Fart\u00F8y"]])
    tabnames[duplicated(tabnames)] <- paste(tabnames[duplicated(tabnames)], "Fart\u00F8y", sep=".")
    names(ConvertedData[["Fart\u00F8y"]]) <- tabnames
  }
  
  # set data types as in xsdObj
  for (n in names(xsdObject$tableTypes)){
    if (length(xsdObject$tableTypes[[n]])>0){
      stopifnot(ncol(ConvertedData[[n]]) == length(xsdObject$tableTypes[[n]]))
      for (i in 1:length(xsdObject$tableTypes[[n]])){
        t <- xsdObject$tableTypes[[n]][i]
        if (t == "xs:string"){
          if (class(ConvertedData[[n]][[i]])!="character"){
            stop("Handle type for ", names(ConvertedData[[n]])[i], " (", class(ConvertedData[[n]][[i]]), ")")
          }
          stopifnot(class(ConvertedData[[n]][[i]])=="character")
        }
        else if (t == "xs:decimal"){
          ConvertedData[[n]][[i]] <- as.numeric(ConvertedData[[n]][[i]])
        }
        else if (t == "xs:integer"){
          ConvertedData[[n]][[i]] <- as.integer(ConvertedData[[n]][[i]])
        }
        else if (t == "xs:long"){
          ConvertedData[[n]][[i]] <- as.numeric(ConvertedData[[n]][[i]])
        }
        else{
          stop("Handle:", t)
        }
      } 
    }
  }
  
  lssLandings$DokumentFormulardato <- NULL
  lssLandings$DokumentElektroniskDato <- NULL
  
  output <- list()
  output$ConvertedData <- ConvertedData
  
  return(output)
}

#' Convert landings
#' @description 
#'  Convert landings to format format provided by \code{\link[RstoxData]{readLssFile}}.
#' @details 
#'  When converting from lss:
#'  All columns of 'lssLandings' are populated, except 'salgsdato' and 'versjonstidspunkt'.
#'  All columns of \code{\link[RstoxData]{LandingData}} are converted, except 'DokumentFormulardato' and 'DokumentElektroniskDato'.
#' @param LandingData \code{\link[RstoxData]{LandingData}} landings to convert
#' @return converted landings. See \code{\link[RstoxData]{readLssFile}} for format
#' @export
convertToLssData <- function(LandingData){
  
  #name mapping between lss and landingerv2.xsd. Keep exactly like convertToLandingData
  nameMap <- list("Dokumentnummer"="Dokumentnummer",
                  "Dokumenttype (kode)"="Dokumenttype_Kode",
                  "Dokumenttype"="Dokumenttype_Bokm\u00E5l",
                  "Dokument versjonsnummer"="DokumentVersjonsnummer",
                  "Dokument salgsdato"=NA,
                  "Dokument versjonstidspunkt"=NA,
                  "Salgslag ID"="SalgslagID",
                  "Salgslag (kode)"="Salgslag_kode",
                  "Salgslag"="Salgslag",
                  "Mottakernasjonalitet (kode)"="Mottakernasjonalitet_kode",
                  "Mottakernasjonalitet"="Mottakernasjonalitet_bokm\u00E5l",
                  "Mottaksstasjon"="Mottaksstasjon",
                  "Landingskommune (kode)"="Landingskommune_kode",
                  "Landingskommune"="Landingskommune",
                  "Landingsfylke (kode)"="Landingsfylke_kode",
                  "Landingsfylke"="Landingsfylke",
                  "Landingsnasjon (kode)"="Landingsnasjon_kode",
                  "Landingsnasjon"="Landingsnasjon_bokm\u00E5l",
                  "Produksjonsanlegg"="Produksjonsanlegg",
                  "Produksjonskommune (kode)"="Produksjonskommune_kode",
                  "Produksjonskommune"="Produksjonskommune",
                  "Fiskerkommune (kode)"="Fiskerkommune_kode",
                  "Fiskerkommune"="Fiskerkommune",
                  "Fiskernasjonalitet (kode)"="Fiskernasjonalitet_kode",
                  "Fiskernasjonalitet"="Fiskernasjonalitet_bokm\u00E5l",      
                  "Fartoynavn"="Fart\u00F8ynavn",
                  "Fartoy ID"="Fart\u00F8yID",           
                  "Registreringsmerke (seddel)"="Registreringsmerke_seddel",
                  "Radiokallesignal (seddel)"="Radiokallesignal_seddel",
                  "Storste lengde"="St\u00F8rsteLengde",
                  "Lengdegruppe (kode)"="Lengdegruppe_kode",
                  "Lengdegruppe"="Lengdegruppe_bokm\u00E5l",
                  "Bruttotonnasje 1969"="Bruttotonnasje1969",
                  "Bruttotonnasje annen"="BruttotonnasjeAnnen",
                  "Byggear"="Bygge\u00E5r",
                  "Ombyggingsar"="Ombyggings\u00E5r",
                  "Motorkraft"="Motorkraft",
                  "Motorbyggear"="Motorbygge\u00E5r",   
                  "Fartoy gjelder fra dato"="Fart\u00F8yGjelderFraDato",
                  "Farto gjelder til dato"="Fart\u00F8yGjelderTilDato",
                  "Fartotype (kode)"="Fart\u00F8ytype_kode",
                  "Fartotype"="Fart\u00F8ytype_bokm\u00E5l",
                  "Kvotefartoy reg.merke"="Kvotefart\u00F8yRegMerke",
                  "Fartoykommune (kode)"="Fart\u00F8ykommune_kode",
                  "Fartoykommune"="Fart\u00F8ykommune",
                  "Fartoyfylke (kode)"="Fart\u00F8yfylke_kode",
                  "Fartoyfylke"="Fart\u00F8yfylke",
                  "Fartoynasjonalitet (kode)"="Fart\u00F8ynasjonalitet_kode",
                  "Fartoynasjonalitet"="Fart\u00F8ynasjonalitet_bokm\u00E5l",
                  "Mottakende farto reg.merke"="MottakendeFart\u00F8yRegMerke",
                  "Mottakende farto rkal"="MottakendeFart\u00F8yRKAL",
                  "Mottakende fartotype (kode)"="MottakendeFart\u00F8ytype_kode",
                  "Mottakende fart.type"="MottakendeFart\u00F8ytype_bokm\u00E5l",
                  "Mottakende fartoynasj. (kode)"="MottakendeFart\u00F8ynasj_kode",
                  "Mottakende fart.nasj"="MottakendeFart\u00F8ynasj_bokm\u00E5l",
                  "Fangstar"="Fangst\u00E5r",
                  "Siste fangstdato"="SisteFangstdato" ,
                  "Kvotetype (kode)"="Kvotetype_kode",
                  "Kvotetype"="Kvotetype_bokm\u00E5l",
                  "Redskap (kode)"="Redskap_kode",
                  "Redskap"="Redskap_bokm\u00E5l",
                  "Redskap - hovedgruppe (kode)"="HovedgruppeRedskap_kode",
                  "Redskap - hovedgruppe"="HovedgruppeRedskap_bokm\u00E5l",
                  "Fangstfelt (kode)"="Fangstfelt_kode",
                  "Kyst/hav (kode)"="KystHav_kode",
                  "Hovedomrade (kode)"="Hovedomr\u00E5de_kode",
                  "Hovedomrade"="Hovedomr\u00E5de_bokm\u00E5l",
                  "Lokasjon (kode)"="Lokasjon_kode",
                  "Sone (kode)"="Sone_kode",
                  "Sone"="Sone_bokm\u00E5l",
                  "Omradegruppering"="Omr\u00E5degruppering_bokm\u00E5l",
                  "Hovedomrade FAO (kode)"="Hovedomr\u00E5deFAO_kode",
                  "Hovedomrade FAO"="Hovedomr\u00E5deFAO_bokm\u00E5l",
                  "Nord/sor for 62 grader nord"="NordS\u00F8rFor62GraderNord",
                  "Fangstdagbok (nummer)"="Fangstdagbok_nummer",
                  "Fangstdagbok (turnummer)"="Fangstdagbok_turnummer",
                  "Landingsdato"="Landingsdato",
                  "Landingsklokkeslett"="Landingsklokkeslett",
                  "Dellanding (signal)"="Dellanding_signal",
                  "Neste mottaksstasjon"="NesteMottaksstasjon",
                  "Forrige mottakstasjon"="ForrigeMottakstasjon",
                  "Linjenummer"="Linjenummer",
                  "Art - FDIR (kode)"="Art_kode",
                  "Art - FDIR"="Art_bokm\u00E5l",
                  "Art - gruppe (kode)"="ArtsgruppeHistorisk_kode",
                  "Art - gruppe"="ArtsgruppeHistorisk_bokm\u00E5l",
                  "Art - hovedgruppe (kode)"="HovedgruppeArt_kode",
                  "Art - hovedgruppe"="HovedgruppeArt_bokm\u00E5l",
                  "Art FAO (kode)"="ArtFAO_kode",
                  "Art FAO"="ArtFAO_bokm\u00E5l",
                  "Produkttilstand (kode)"="Produkttilstand_kode",
                  "Produkttilstand"="Produkttilstand_bokm\u00E5l",
                  "Konserveringsmate (kode)"="Konserveringsm\u00E5te_kode",
                  "Konserveringsmate"="Konserveringsm\u00E5te_bokm\u00E5l",
                  "Landingsmate (kode)"="Landingsm\u00E5te_kode",
                  "Landingsmate"="Landingsm\u00E5te_bokm\u00E5l",
                  "Kvalitet (kode)"="Kvalitet_kode",
                  "Kvalitet"="Kvalitet_bokm\u00E5l",
                  "Storrelsesgruppering (kode)"="St\u00F8rrelsesgruppering_kode",
                  "Anvendelse (kode)"="Anvendelse_kode",
                  "Anvendelse"="Anvendelse_bokm\u00E5l",
                  "Anvendelse hovedgruppe (kode)"="HovedgruppeAnvendelse_kode",
                  "Anvendelse hovedgruppe"="HovedgruppeAnvendelse_bokm\u00E5l",
                  "Antall stykk"="AntallStykk",
                  "Bruttovekt"="Bruttovekt",
                  "Produktvekt"="Produktvekt",
                  "Rundvekt"="Rundvekt")
  
  
  names(nameMap)[26] <- "Fart\u00F8ynavn"
  names(nameMap)[27] <- "Fart\u00F8y ID"
  names(nameMap)[30] <- "St\u00F8rste lengde"
  names(nameMap)[35] <- "Bygge\u00E5r"
  names(nameMap)[36] <- "Ombyggings\u00E5r"
  names(nameMap)[38] <- "Motorbygge\u00E5r"
  names(nameMap)[39] <- "Fart\u00F8y gjelder fra dato"
  names(nameMap)[40] <- "Fart\u00F8y gjelder til dato"
  names(nameMap)[41] <- "Fart\u00F8ytype (kode)"
  names(nameMap)[42] <- "Fart\u00F8ytype"
  names(nameMap)[43] <- "Kvotefart\u00F8y reg.merke"
  names(nameMap)[44] <- "Fart\u00F8ykommune (kode)"
  names(nameMap)[45] <- "Fart\u00F8ykommune"
  names(nameMap)[46] <- "Fart\u00F8yfylke (kode)"
  names(nameMap)[47] <- "Fart\u00F8yfylke"
  names(nameMap)[48] <- "Fart\u00F8ynasjonalitet (kode)"
  names(nameMap)[49] <- "Fart\u00F8ynasjonalitet"
  names(nameMap)[50] <- "Mottakende fart\u00F8y reg.merke"
  names(nameMap)[51] <- "Mottakende fart\u00F8y rkal"
  names(nameMap)[52] <- "Mottakende fart\u00F8ytype (kode)"
  names(nameMap)[54] <- "Mottakende fart\u00F8ynasj. (kode)"
  names(nameMap)[56] <- "Fangst\u00E5r"
  names(nameMap)[66] <- "Hovedomr\u00E5de (kode)"
  names(nameMap)[67] <- "Hovedomr\u00E5de"
  names(nameMap)[71] <- "Omr\u00E5degruppering"
  names(nameMap)[72] <- "Hovedomr\u00E5de FAO (kode)"
  names(nameMap)[73] <- "Hovedomr\u00E5de FAO"
  names(nameMap)[74] <- "Nord/s\u00F8r for 62 grader nord"
  names(nameMap)[93] <- "Konserveringsm\u00E5te (kode)"
  names(nameMap)[94] <- "Konserveringsm\u00E5te"
  names(nameMap)[95] <- "Landingsm\u00E5te (kode)"
  names(nameMap)[96] <- "Landingsm\u00E5te"
  names(nameMap)[99] <- "St\u00F8rrelsesgruppering (kode)"
  
  stopifnot(is.LandingData(LandingData))
  
  flatlandings <- LandingData[[1]]$Seddellinje
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Salgslagdata)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Mottaker)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Produksjon)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Fangstdata)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Fisker)

  #correcting for mistake in format spec (xsd). Nationality of vessel is repeated. Renameing by same convention as readXmlFile.
  if (any(duplicated(names(LandingData[[1]][["Fart\u00F8y"]])))){
    dups <- duplicated(names(LandingData[[1]][["Fart\u00F8y"]]))
    names(LandingData[[1]][["Fart\u00F8y"]])[dups] <- paste(names(LandingData[[1]][["Fart\u00F8y"]])[dups], "Fart\u00F8y", sep=".")
  }

  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]][["Fart\u00F8y"]])
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]][["Mottakendefart\u00F8y"]])
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Kvote)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Redskap)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Dellanding)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Produkt)
  flatlandings <- mergeByIntersect(flatlandings, LandingData[[1]]$Art)
  
  flatlandings$DokumentFormulardato <- NULL
  flatlandings$DokumentElektroniskDato <- NULL
  flatlandings[["Dokument salgsdato"]] <- NA
  nameMap["Dokument salgsdato"] <- "Dokument salgsdato"
  flatlandings[["Dokument versjonstidspunkt"]] <- NA
  nameMap["Dokument versjonstidspunkt"] <- "Dokument versjonstidspunkt"
  
  flatlandings <- flatlandings[, .SD, .SDcols=unlist(nameMap)]
  names(flatlandings) <- names(nameMap)
  
  #convert types
  flatlandings[["Dokumenttype (kode)"]] <- as.character(flatlandings[["Dokumenttype (kode)"]])
  flatlandings[["Dokument versjonsnummer"]] <- as.character(flatlandings[["Dokument versjonsnummer"]])
  flatlandings[["Dokument salgsdato"]] <- as.character(flatlandings[["Dokument salgsdato"]])
  flatlandings[["Dokument versjonstidspunkt"]] <- as.character(flatlandings[["Dokument versjonstidspunkt"]])
  flatlandings[["Salgslag ID"]] <- as.character(flatlandings[["Salgslag ID"]])
  flatlandings[["Landingskommune (kode)"]] <- as.character(flatlandings[["Landingskommune (kode)"]])
  flatlandings[["Landingsfylke (kode)"]] <- as.character(flatlandings[["Landingsfylke (kode)"]])
  flatlandings[["Fiskerkommune (kode)"]] <- as.character(flatlandings[["Fiskerkommune (kode)"]])
  flatlandings[["Fart\u00F8ykommune (kode)"]] <- as.character(flatlandings[["Fart\u00F8ykommune (kode)"]])
  flatlandings[["Fart\u00F8yfylke (kode)"]] <- as.character(flatlandings[["Fart\u00F8yfylke (kode)"]])
  flatlandings[["Kyst/hav (kode)"]] <- as.character(flatlandings[["Kyst/hav (kode)"]])
  flatlandings[["Siste fangstdato"]] <- as.POSIXct(flatlandings[["Siste fangstdato"]], format="%d.%m.%Y", tz="CET")
  flatlandings[["Fangstdagbok (nummer)"]] <- as.character(flatlandings[["Fangstdagbok (nummer)"]])
  flatlandings[["Fangstdagbok (turnummer)"]] <- as.character(flatlandings[["Fangstdagbok (turnummer)"]])
  flatlandings[["Dellanding (signal)"]] <- as.character(flatlandings[["Dellanding (signal)"]])
  data.table::setkey(flatlandings, NULL)
  
  return(flatlandings)
}
