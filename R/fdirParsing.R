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
  typ <- unlist(spec_land)
  
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
    db$`Siste fangstdato` <- as.POSIXct(db$`Siste fangstdato`, format='%d.%m.%Y')
  }
  else{
    db <- data.table::fread(file, sep="|", header = T, dec=",", strip.white=TRUE, na.strings=c("", "na", "NA"), encoding = encoding, keepLeadingZeros=T)
    if ("Siste fangstdato" %in% names(db)){
      db$`Siste fangstdato` <- as.POSIXct(db$`Siste fangstdato`, format='%d.%m.%Y')
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
  logb$STARTTIDSPUNKT <- as.POSIXct(logb$STARTTIDSPUNKT, format="%Y-%m-%d %H:%M:%S")
  logb$STOPPTIDSPUNKT <- as.POSIXct(logb$STOPPTIDSPUNKT, format="%Y-%m-%d %H:%M:%S")
  
  return(logb)
}
