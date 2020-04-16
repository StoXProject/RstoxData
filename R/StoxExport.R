
write2ICESacoustic_CSV <- function(Acoustic,save=T){
  
  for(aco in Acoustic){
    if(aco$metadata$useXsd=='icesAcoustic'){
      
      hl<-c()
      hl$Instrument <- 'Instrument'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$Instrument)
      names(tmp)<-paste0('Instrument',names(tmp))
      hl <- cbind(hl,tmp)
      HInst <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$Calibration <- 'Calibration'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$Calibration)
      names(tmp)<-paste0('Calibration',names(tmp))
      hl <- cbind(hl,tmp)
      HCal <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$DataAcquisition <- 'DataAcquisition'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$DataAcquisition)
      names(tmp)<-paste0('DataAcquisition',names(tmp))
      hl <- cbind(hl,tmp)
      HDatA <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$DataProcessing <- 'DataProcessing'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$DataProcessing)
      names(tmp)<-paste0('DataProcessing',names(tmp))
      hl <- cbind(hl,tmp)
      HDatP <- format(hl, trim=TRUE, width=0)
      
      hl<-c()
      hl$Cruise <- 'Cruise'
      hl$Header <- 'Record'
      hl$CruiseSurvey <- aco$Survey[2]
      tmp <- data.frame(aco$Cruise)
      names(tmp)<-paste0('Cruise',names(tmp))
      hl <- cbind(hl,tmp)
      HCru <- format(hl, trim=TRUE, width=0)
      
      
      hl<-c()
      hl$Data <- 'Data'
      hl$Header <- 'Record'
      tmp_log <- unique(data.frame(aco$Log))
      names(tmp_log)[names(tmp_log)=='LocalID']<-'CruiseLocalID'
      names(tmp_log)[names(tmp_log)!='CruiseLocalID']<-paste0('Log',names(tmp_log)[names(tmp_log)!='CruiseLocalID'])
      tmp_sample <- data.frame(aco$Sample)
      names(tmp_sample)[names(tmp_sample)=='LocalID']<-'CruiseLocalID'
      names(tmp_sample)[names(tmp_sample)=='Distance']<-'LogDistance'
      names(tmp_sample)[names(tmp_sample)=='Instrument']<-'InstrumentID'
      names(tmp_sample)[names(tmp_sample)=='Calibration']<-'CalibrationID'
      names(tmp_sample)[names(tmp_sample)=='DataAcquisition']<-'DataAcquisitionID'
      names(tmp_sample)[names(tmp_sample)=='DataProcessing']<-'DataProcessingID'
      names(tmp_sample)[!names(tmp_sample)%in%c('CruiseLocalID','LogDistance','InstrumentID','CalibrationID','DataAcquisitionID','DataProcessingID')] <-paste0('Sample',names(tmp_sample)[!names(tmp_sample)%in%c('CruiseLocalID','LogDistance','InstrumentID','CalibrationID','DataAcquisitionID','DataProcessingID')])
      
      
      tmp_data <- data.frame(aco$Data)
      names(tmp_data)[names(tmp_data)=='LocalID']<-'CruiseLocalID'
      names(tmp_data)[names(tmp_data)=='Distance']<-'LogDistance'
      names(tmp_data)[names(tmp_data)=='ChannelDepthUpper']<-'SampleChannelDepthUpper'
      names(tmp_data)[!names(tmp_data)%in%c('CruiseLocalID','LogDistance','SampleChannelDepthUpper')]<-paste0('Data',names(tmp_data)[!names(tmp_data)%in%c('CruiseLocalID','LogDistance','SampleChannelDepthUpper')])
      
      
      tmp_sub <-(merge(tmp_data,tmp_sample, by=intersect(names(tmp_data),names(tmp_sample))))
      tmp_sub <-merge(tmp_log,tmp_sub,by=intersect(names(tmp_log),names(tmp_sub)))
      hl<-cbind(hl,tmp_sub)
      HDat <- format(hl, trim=TRUE, width=0)
      
      tmp <- list(Instrument=HInst,
                  Calibration=HCal,
                  DataAcquisition=HDatA,
                  DataProcessing=HDatP,
                  Cruise=HCru,
                  Data=HDat
      )
      
      if(save==T){
        filename = gsub('.xml','.csv',aco$metadata$file)
        if(file.exists(filename))file.remove(filename)
        suppressWarnings(lapply(tmp, write.table, file=filename, append=TRUE, row.names=FALSE, quote=FALSE, sep=","))
        }
      
    }else{warning('only ices acoustic format is allowed')}

  }
  
  return(tmp)
}

# Datras format generator from Biotic v3
generateDATRAS <- function(raw, additionalStation = NA) {

    # Check input is a NMD Biotic v3 data
    if(raw$metadata$useXsd != "nmdbioticv3") {
        print("DATRAS export can only accept NMD Biotic version 3 data as input. Exiting.")
        return(NA)
    }

    # Construct user-defined additional stations (default to NA stationtypes)
    if(length(additionalStation) > 0 && !all(is.na(additionalStation))) {
        targetStationType <- additionalStation
    } else {
        targetStationType <- c()
    }

    ## 1. HH ##
    '%ni%' <- Negate('%in%')

    getTSCountryByIOC <- function(nation) {
        cnvTbl <- c("58" = "NOR")

        x <- cnvTbl[as.character(nation)]
        x[is.null(x)] <- "-9"
        return(x)
    }

    getGOVSweepByEquipment <- function(gear) {
        cnvTbl <- c("3120" = -9,
                    "3190" = 60,
                    "3191" = 60,
                    "3192" = 60,
                    "3193" = 110,
                    "3194" = 110,
                    "3195" = 110,
                    "3196" = 60,
                    "3197" = 110,
                    "3198" = 60,
                    "3199" = 60)

        x <- cnvTbl[as.character(gear)]
        x[is.null(x)] <- "-9"
        return(x)
    }

    getGearExp <- function(sweep, year, serialnumber, depth) {

        temp <-  as.data.table(cbind(sweep, year, serialnumber, depth))
        temp[, res:= "S"]

        temp[year == 2011 & serialnumber > 24362 & depth >= 70 | year == 2012
                    | year == 2011 & serialnumber >= 24135 & depth >= 70, res:="ST"]

        return (temp$res)
    }

    getYear <- function(stationstartdate) {
        format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%Y")
    }

    getMonth <- function(stationstartdate) {
        format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
    }

    getDay <- function(stationstartdate) {
        format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%d")
    }

    getTimeShot <- function(stationstarttime) {

        timeshot <- function(y) {
            if(length(y) == 3) {
                return(paste0(y[1], y[2]))
            } else {
                return("-9")
            }
        }

        x <- strsplit(stationstarttime, ":")

        return(unlist(lapply(x, timeshot)))
    }

    getTimeDiff <- function(stationstartdate, stationstarttime, stationstopdate, stationstoptime) {

        start <- as.POSIXct(gsub("Z", " ", paste0(stationstartdate, stationstarttime)))
        end <- as.POSIXct(gsub("Z", " ", paste0(stationstopdate, stationstoptime)))

        return(round(difftime(end, start)/60))
    }

    getQuarter <- function(stationstartdate) {
        x <- format(as.Date(stationstartdate, format="%Y-%m-%dZ"), "%m")
        return(floor((as.numeric(x) - 1) / 3 + 1))
    }

    getDistanceMeter <- function(lat1, lon1, lat2, lon2) {
        x <-  acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000
        return(x)
    }

    getHaulVal <- function(gearcondition, samplequality) {
        temp <-  as.data.table(cbind(g=gearcondition, s=samplequality))
        temp[, res:="I"]
        temp[(is.na(g) | g %in% c("1","2")) &
            (is.na(s) | s %in% c("0", "1")), res:="V"]

        return(temp$res)
    }

    # Stolen from: https://github.com/cran/mapplots/blob/master/R/ices.rect.R
    getICESrect <- function(lat, lng){
        x <- floor(lng+60)+1000
        y <- floor(lat*2)-71+100
        num1<- substr(y,2,3)
        lett <- LETTERS[as.numeric(substr(x,2,3))]
        num2 <- substr(x,4,4)
        paste(num1,lett,num2,sep='')
    }

    # Adopted from: https://www.mathworks.com/matlabcentral/fileexchange/62180-sunriseset-lat-lng-utcoff-date-plot
    getDayNight <- function(stationstartdate, stationstarttime, latitudestart, longitudestart, UTCoff = 0) {

        deg2rad <- function(val) {
            return(val * (pi / 180))
        }

        rad2deg <- function(val) {
            return(val * (180 / pi))
        }

        datetime0 <- as.POSIXct("1990-12-30", tz = "GMT")

        uniqueDates <- unique(stationstartdate)

        nDaysA = as.numeric(difftime(uniqueDates, datetime0, units = "days")) # Number of days since 01/01

        nTimes = 24*3600;                       # Number of seconds in the day
        tArray = seq(0, 1, length = nTimes);

        ssTab <- list()

        for(idx in seq_len(length(nDaysA))) {

            nDays <- nDaysA[idx]
            lat <- latitudestart[idx]
            lng <- longitudestart[idx]
            localdate <- as.POSIXct(uniqueDates[idx], tz = "GMT")

            # Compute
            # Letters correspond to colums in the NOAA Excel
            E = tArray;
            F = nDays+2415018.5+E-UTCoff/24;
            G = (F-2451545)/36525;
            I = (280.46646+G * (36000.76983+G*0.0003032)) %% 360;
            J = 357.52911+G * (35999.05029-0.0001537*G);
            K = 0.016708634-G * (0.000042037+0.0000001267*G);
            L = sin(deg2rad(J)) * (1.914602-G * (0.004817+0.000014*G))+sin(deg2rad(2*J)) * (0.019993-0.000101*G)+sin(deg2rad(3*J))*0.000289;
            M = I+L;
            P = M-0.00569-0.00478*sin(deg2rad(125.04-1934.136*G));
            Q = 23+(26+((21.448-G * (46.815+G * (0.00059-G*0.001813))))/60)/60;
            R = Q+0.00256*cos(deg2rad(125.04-1934.136*G));
            T = rad2deg(asin(sin(deg2rad(R)) * sin(deg2rad(P))));
            U = tan(deg2rad(R/2)) * tan(deg2rad(R/2));
            V = 4*rad2deg(U * sin(2*deg2rad(I))-2*K * sin(deg2rad(J))+4*K * U * sin(deg2rad(J)) *  cos(2*deg2rad(I))-0.5 * U * U * sin(4*deg2rad(I))-1.25 * K * K * sin(2 * deg2rad(J)));
            AB = (E*1440+V+4*lng-60*UTCoff) %% 1440

            AC = ifelse (AB/4 < 0, AB/4+180, AB/4-180)

            AD = rad2deg(acos(sin(deg2rad(lat))*sin(deg2rad(T))+cos(deg2rad(lat))*cos(deg2rad(T)) * cos(deg2rad(AC))));
            W = rad2deg(acos(cos(deg2rad(90.833)) / (cos(deg2rad(lat))*cos(deg2rad(T)))-tan(deg2rad(lat))*tan(deg2rad(T))));
            X = (720-4*lng-V+UTCoff*60)*60;

            sunrise = which.min(abs(X-round(W*4*60) - nTimes*tArray));
            sunrisetime = localdate + sunrise
            sunset = which.min(abs(X+round(W*4*60) - nTimes*tArray));
            sunsettime = localdate + sunset

            ssTab[[uniqueDates[idx]]] <- list(sunrise = sunrisetime, sunset = sunsettime)
        }

        getDN <- function(x, ssTab) {

            y <- ssTab[[format(x, "%Y-%m-%dZ")]]

            if(x < y$sunrise || x >= y$sunset) {
                return("N")
            } else {
                return("D")
            }
        }

        datetime <- as.POSIXct(gsub("Z", " ", paste0(stationstartdate, stationstarttime)), tz = "GMT")

        return(unlist(lapply(datetime, getDN, ssTab)))
    }

    # Get ICES ship data
    getICESShipCode <- function(platformname) {

        construct <- function(shipName) {
            # We have to remove "."," " and use uppercase
            shipName <- toupper(gsub("[[:space:][:punct:]]", "", shipName))

            # Replace the nordic character with AA
            shipName <- gsub("\u00C5", "AA", shipName)

            data <- tryCatch(
                {
                    read_xml("https://vocab.ices.dk/services/rdf/collection/TS_Ship")
                },
                    error = function(e){return(NA)}
            )

            if(is.na(data))
                return("-9")

            nodes <- xml_find_all(data, paste0("//skos:concept[contains(translate(skos:prefLabel[normalize-space()],'abcdefghijklmnopqrstuvwxyz. ','ABCDEFGHIJKLMNOPQRSTUVWXYZ'), \"", shipName, "\")]"))
            shipDetailUrl <- xml_attr(nodes[[1]], "about")
            shipCode <- tail(unlist(strsplit(shipDetailUrl, split="/")), 1)

            # Simple hack for Cefas Endeavour
            if(shipCode == "END") shipCode <- "ENDW"

            return(shipCode)
        }

        nm <- unique(platformname)
        y <- unlist(lapply(nm, construct))
        names(y) <- nm

        x <- y[as.character(platformname)]
        x[is.null(x)] <- "-9"

        return(x)
    }

    finalHH <- merge(raw$mission, raw$fishstation)

    # Make HH records and filter only valid stationtype
    finalHH[, `:=`(
        "RecordType" = "HH",
        "Quarter" = getQuarter(stationstartdate),
        "Country" = getTSCountryByIOC(nation),
        "Ship" = getICESShipCode(platformname),
        "Gear" = "GOV",
        "SweepLngt" = getGOVSweepByEquipment(gear),
        "GearExp" = getGearExp(getGOVSweepByEquipment(gear), startyear, serialnumber, bottomdepthstart),
        "DoorType" = "P",
        "StNo" = serialnumber,
        "HaulNo" = station,
        "Year" = getYear(stationstartdate),
        "Month" = getMonth(stationstartdate),
        "Day" = getDay(stationstartdate),
        "TimeShot" = getTimeShot(stationstarttime),
        "Stratum" = "-9",
        "HaulDur" = as.numeric(getTimeDiff(stationstartdate, stationstarttime, stationstopdate, stationstoptime)),
        "DayNight" = getDayNight(stationstartdate, stationstarttime, latitudestart, longitudestart),
        "ShootLat" = latitudestart,
        "ShootLong" = longitudestart,
        "HaulLat" = latitudeend,
        "HaulLong" = longitudeend,
        "StatRec" = getICESrect(latitudestart, longitudestart),
        "Depth" = round(bottomdepthstart),
        "HaulVal" = getHaulVal(gearcondition, samplequality),
        "HydroStNo" = -9,
        "StdSpecRecCode" = 1,
        "BycSpecRecCode" = 1,
        "DataType" = "R",
        "Netopening"= verticaltrawlopening,
        "Rigging" = -9,
        "Tickler" = -9,
        "Distance" = getDistanceMeter(latitudestart, longitudestart, latitudeend, longitudeend),
        "Warpingt" = wirelength,
        "Warpdia" = -9,
        "WarpDen" = -9,
        "DoorSurface" = 4.46,
        "DoorWgt" = 1075,
        "DoorSpread" = ifelse(!is.na(trawldoorspread), trawldoorspread, -9),
        "WingSpread" = -9,
        "Buoyancy" = -9,
        "KiteDim" = 0.85,
        "WgtGroundRope" = -9,
        "TowDir" = ifelse(!is.na(direction), round(direction), -9),
        "GroundSpeed" = round(gearflow, 1),
        "SpeedWater" = -9,
        "SurCurDir" = -9,
        "SurCurSpeed" = -9,
        "BotCurDir" = -9,
        "BotCurSpeed" = -9,
        "WindDir" = -9,
        "WindSpeed" = -9,
        "SwellDir" = -9,
        "SwellHeight" = -9,
        "SurTemp" = -9,
        "BotTemp" = -9,
        "SurSal" = -9,
        "BotSal" = -9,
        "ThermoCline" = -9,
        "ThClineDepth" = -9
    )]

    HHraw <- copy(finalHH[is.na(stationtype) | stationtype %in% targetStationType, c("RecordType", "Quarter", "Country", "Ship", "Gear",
                            "SweepLngt", "GearExp", "DoorType", "StNo", "HaulNo", "Year", "Month", "Day",
                            "TimeShot", "Stratum", "HaulDur", "DayNight", "ShootLat", "ShootLong", "HaulLat", "HaulLong",
                            "StatRec", "Depth", "HaulVal", "HydroStNo", "StdSpecRecCode", "BycSpecRecCode", "DataType", "Netopening",
                            "Rigging", "Tickler", "Distance", "Warpingt", "Warpdia", "WarpDen", "DoorSurface", "DoorWgt",
                            "DoorSpread", "WingSpread", "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", "GroundSpeed",
                            "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir", "BotCurSpeed", "WindDir", "WindSpeed",
                            "SwellDir", "SwellHeight", "SurTemp", "BotTemp", "SurSal", "BotSal", "ThermoCline", "ThClineDepth")]
                            )

    ## 2. HL ##

    mergedHL <- merge(raw$catchsample, finalHH, by=intersect(names(raw$catchsample), names(finalHH)))

    groupHL <- c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "aphia", "sex")

    # Remove rows with empty aphia
    mergedHL <- mergedHL[!is.na(aphia)]

    getSpecVal <- function(HaulVal, catchcount, lengthsamplecount, catchweight){
        temp <-  as.data.table(cbind(hv=HaulVal, cc=catchcount, lsc=lengthsamplecount, cw=catchweight))

        temp[, res:="-9"]
        temp[!is.na(cc) & !is.na(lsc) & !is.na(cw), res:="1"]
        temp[!is.na(cc) &  is.na(lsc) &  is.na(cw), res:="4"]
        temp[ is.na(cc) &  is.na(lsc) & !is.na(cw), res:="6"]
        temp[!is.na(cc) &  is.na(lsc) & !is.na(cw), res:="7"]
        temp[ is.na(cc) &  is.na(lsc) &  is.na(cw), res:="5"]
        temp[!is.na(cc) & !is.na(lsc) &  is.na(cw), res:="0"]

        temp[hv == "I", res:="0"]

        return(temp$res)
    }

    mergedHL[, SpecVal := getSpecVal(HaulVal, catchcount, lengthsamplecount, catchweight)]

    # Get herring or sprat
    mergedHL[,`:=`( isHerringOrSprat = ifelse(aphia %in% c("126417", "126425"), TRUE, FALSE),
                    isCrustacean = ifelse(aphia %in% c("107275", "107276", "107369", "107253", "107703", "107704", "107350", "107254", "107205", "140712", "140687", "140658"), TRUE, FALSE))]

    # Calculate lngtCode
    mergedHL[,lngtCode := "1"]
    mergedHL[is.na(sampletype), lngtCode := "-9"]
    mergedHL[isCrustacean == TRUE, lngtCode := "."]
    mergedHL[isHerringOrSprat == TRUE, lngtCode := "0"]

    # lenInterval, and reportInMM
    mergedHL[,`:=`(lenInterval = ifelse(lngtCode=="0", "5", "1"), reportInMM = ifelse(lngtCode %ni% c("1", "-9"), TRUE, FALSE))]

    # catCatchWgt & subWeight
    mergedHL[is.na(catchweight), catchweight:= 0]
    mergedHL[is.na(lengthsampleweight), lengthsampleweight:= 0]
    mergedHL[, catCatchWgt := ceiling(catchweight * 1000)]
    mergedHL[, subWeight := ceiling(lengthsampleweight * 1000)]

    mergedHL[is.na(catchcount), catchcount := 0]
    mergedHL[is.na(lengthsamplecount), lengthsamplecount := 0]

    # get sampleFac
    mergedHL[, sampleFac := ifelse(lengthsampleweight > 0, catchweight / lengthsampleweight, 1.0)]

    # Merge with individual
    mergedHL <- merge(mergedHL, raw$individual, by = intersect(names(mergedHL), names(raw$individual)), all.x = TRUE)

    # Get count
    mergedHL[, N := sum(!is.na(specimenid)), by = groupHL]

    # For the record with empty individual data
    mergedHL[N == 0 | lengthsampleweight == 0, `:=`(lngtClass = "-9", sex = NA)]

    # Get Individual length
    mergedHL[, length := length * 100]

    # Some species have very small length in cm, use mm instead
    mergedHL[length < 1, `:=`(lngtCode = ".", lenInterval = "1", reportInMM = TRUE)]

    # Process MM length
    mergedHL[reportInMM == TRUE, length := length * 10]

    # Get sex
    mergedHL[, sex := ifelse(is.na(sex), "-9", ifelse(sex == "1", "F", "M"))]

    # Get lngtClass
    mergedHL[lengthsampleweight == 0, lngtClass := "-9"]
    for(interval in unique(mergedHL$lenInterval)) {
        intVec <- seq(0, max(mergedHL$length, na.rm = T), by = as.numeric(interval))
        mergedHL[lenInterval == interval, lngtClass := intVec[findInterval(length, intVec)]]
    }
    mergedHL[is.na(lngtClass), lngtClass := "-9"]

    # Aggregate hlNoAtLngth and lsCountTot
    mergedHL[, `:=`(hlNoAtLngth = ifelse(N==0 | lengthsampleweight == 0, 1.0 * catchcount, 1.0 * sampleFac), lsCountTot = ifelse(N==0 | lengthsampleweight == 0, 1.0 * lengthsamplecount, 1.0))]

    finalHL <- mergedHL[, .(hlNoAtLngth = sum(hlNoAtLngth), lsCountTot = sum(lsCountTot)), by = c(groupHL,  "lngtClass",
                            "Quarter",
                            "Country",
                            "Ship",
                            "Gear",
                            "SweepLngt", "GearExp", "DoorType", "HaulNo", "SpecVal", "catchpartnumber", "catCatchWgt", "subWeight", "lngtCode", "stationtype")]

    finalHL[,`:=`(totalNo = sum(hlNoAtLngth), noMeas = sum(lsCountTot)), by=c(groupHL)]
    finalHL[, subFactor := ifelse(totalNo == 0 | noMeas == 0, -9, totalNo / noMeas)]
    finalHL[, subWeight := ifelse(subFactor == 1, catCatchWgt, subWeight)]


    HLraw <- copy(finalHL[is.na(stationtype) | stationtype %in% targetStationType, .("RecordType" = "HL",
        "Quarter" = Quarter,
        "Country" = Country,
        "Ship" = Ship,
        "Gear" = Gear,
        "SweepLngt" = SweepLngt,
        "GearExp" = GearExp,
        "DoorType" = DoorType,
        "StNo" = serialnumber,
        "HaulNo" = HaulNo,
        "Year" = startyear,
        "SpecCodeType" = "W",
        "SpecCode" = aphia,
        "SpecVal" = SpecVal,
        "Sex" = sex,
        "TotalNo" = ifelse(totalNo == 0, -9, totalNo),
        "CatIdentifier" = catchpartnumber,
        "NoMeas" = ifelse(noMeas == 0, -9, noMeas),
        "SubFactor" = subFactor,
        "SubWgt" = ifelse(subWeight == 0, -9, subWeight),
        "CatCatchWgt" = ifelse(catCatchWgt == 0, -9, catCatchWgt),
        "LngtCode" = lngtCode,
        "LngtClass" = lngtClass,
        "HLNoAtLngt" = ifelse(lsCountTot > 0, lsCountTot, -9))
        ]
    )


    ## 3. CA ##

    getDATRASMaturity <- function(quarter, aphia, specialstage, maturationstage) {

        temp <-  as.data.table(cbind(q=quarter, ap=aphia, sp=specialstage, ms=maturationstage, res=-9))

        temp[, `:=`(sp = as.numeric(sp), ms = as.numeric(ms), res = as.numeric(res) )]

        temp[, isHerringOrSpratOrMackerel := ifelse(ap %in% c("126417", "126425", "127023"), TRUE, FALSE)]

        temp[!is.na(sp) & isHerringOrSpratOrMackerel == TRUE,  res := ifelse(sp <= 2, 61, ifelse(sp <= 5, 62, 60 + sp - 3))]
        temp[!is.na(sp) & isHerringOrSpratOrMackerel == FALSE, res := 60 + sp]

        temp[is.na(sp) & !is.na(ms), res := ifelse(ms == 5 & q == "3", -9, 60 + ms)]

        return(temp$res)
    }

    mergedHL[is.na(preferredagereading), preferredagereading := 1]
    baseAge <- intersect(names(mergedHL), names(raw$agedetermination))
    mergedCA <- merge(mergedHL, raw$agedetermination, by.x=c(baseAge, "preferredagereading"), by.y= c(baseAge, "agedeterminationid"), all.x = TRUE)

    # Remove empty individual
    mergedCA <- mergedCA[!is.na(specimenid)]

    # Get maturity
    mergedCA[, maturity:=getDATRASMaturity(Quarter, aphia, specialstage, maturationstage)]

    # Aggregate count
    mergedCA[!is.na(individualweight), `:=`(nWithWeight =.N, totWeight = sum(individualweight)), by = c(groupHL,  "lngtClass", "maturity", "age")]

    finalCA <- mergedCA[, .(nInd =.N), by = c(groupHL,  "lngtClass", "maturity", "age",
                            "Quarter",
                            "Country",
                            "Ship",
                            "Gear",
                            "SweepLngt", "GearExp", "DoorType", "HaulNo", "SpecVal", "StatRec", "lngtCode", "stationtype", "nWithWeight", "totWeight")]
    finalCA[!is.na(nWithWeight),  meanW := totWeight / nWithWeight]

    CAraw <- copy(finalCA[is.na(stationtype) | stationtype %in% targetStationType, .("RecordType" = "CA",
        "Quarter" = Quarter,
        "Country" = Country,
        "Ship" = Ship,
        "Gear" = Gear,
        "SweepLngt" = SweepLngt,
        "GearExp" = GearExp,
        "DoorType" = DoorType,
        "StNo" = serialnumber,
        "HaulNo" = HaulNo,
        "Year" = startyear,
        "SpecCodeType" = "W",
        "SpecCode" = aphia,
        "AreaType" = "0",
        "AreaCode" = StatRec,
        "LngtCode" = lngtCode,
        "LngtClass" = lngtClass,
        "Sex" = sex,
        "Maturity" = maturity,
        "PlusGr" = as.character(-9),
        "AgeRings" = ifelse(!is.na(age), age, -9),
        "CANoAtLngt" = nInd,
        "IndWgt" = ifelse(!is.na(meanW), meanW * 1000, -9))
        ]
    )


    ## Prepare for cleaning (mostly from the old Rstox. Will change as we improve) ##
    hh <- HHraw
    hl <- HLraw
    ca <- CAraw

    ## WARN #0:
	# It's possible to have two same aphia (but different species, e.g. SILD05) catch sampes in a haul.
	# We need to combine them if we have two different TotalNo and catcatchwgt.

	# Find duplicate species in a haul
	dupl <- aggregate(catchcategory ~ aphia + serialnumber, raw$catchsample, FUN = function(x) length(unique(x)))
	dupl <- dupl[dupl$catchcategory > 1, ]

	# Find the above in our DATRAS HL
	if(nrow(dupl)) {
		found <- aggregate(CatCatchWgt ~ StNo + SpecCode + Sex + CatIdentifier, hl[(hl$SpecCode %in% dupl$aphia & hl$StNo %in% dupl$serialnumber),], FUN = function(x) length(unique(x)))
		found <- found[found$CatCatchWgt > 1, ]
		if(nrow(found)) {
			for(iz in 1:nrow(found)) {
				tmpHL <- hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], ]
				combinedCatCatchWgt <- tmpHL
				# Fix CatCatchWgt
				hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "CatCatchWgt"] <- round(mean(tmpHL$CatCatchWgt))
				# Fix CatCatchWgt
				hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "SubWgt"] <- round(mean(tmpHL$SubWgt))
				# Fix totalNo
				hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "TotalNo"] <- sum(unique(tmpHL$TotalNo))
				# Fix noMeas
				hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "NoMeas"] <- sum(tmpHL$HLNoAtLngt)
				# Finally, fix SubFactor
				hl[hl$StNo==found[iz, "StNo"] & hl$SpecCode==found[iz, "SpecCode"] & hl$Sex==found[iz, "Sex"] & hl$CatIdentifier==found[iz, "CatIdentifier"], "SubFactor"] <- sum(unique(tmpHL$TotalNo))/sum(tmpHL$HLNoAtLngt)
			}
		}
	}

    ## WARN #1:
	# Find species with different SpecVal, if any of them have SpecVal == 1, delete any other records with different SpecVal
	# otherwise, use the lowest SpecVal value for all

	tmp <- aggregate(SpecVal ~ SpecCode + StNo, hl, FUN = function(x) length(unique(x)))
	tmp <- tmp[tmp$SpecVal>1, ]

	if( nrow(tmp) > 0 ) {
		for( rownum in 1: nrow(tmp) ) {
			tmpSpecs <- hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]),]$SpecVal
			if(any(tmpSpecs == 1))
				hl <- hl[!(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum] & hl$SpecVal!=1),]
			else
				hl[(hl$StNo==tmp$StNo[rownum] & hl$SpecCode==tmp$SpecCode[rownum]), c("SpecVal")] <- min(tmpSpecs)
		}
	}

	## SpecVal Conditionals
	hl[hl$SpecVal==0, c("Sex", "TotalNo", "CatIdentifier", "NoMeas", "SubFactor", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==4, c("NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9
	hl[hl$SpecVal==4, c("SubFactor")] <- 1

	hl[hl$SpecVal==5, c("TotalNo", "NoMeas", "SubWgt", "CatCatchWgt", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9
	hl[hl$SpecVal==5, c("SubFactor")] <- 1

	hl[hl$SpecVal==6, c("TotalNo", "NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==7, c("NoMeas", "LngtCode", "LngtClass", "HLNoAtLngt")] <- -9

	hl[hl$SpecVal==10, c("CatCatchWgt")] <- -9

	## WARN #2:
	## will now get errors in DATRAS upload for duplicate records
	hl <- hl[!duplicated(hl),]

	## hl and ca contain 0-tow info - must throw these out
	hl <- hl[hl$StNo %in% hh$StNo,]
	ca <- ca[ca$StNo %in% hh$StNo,]
	# throw out ca records for Invalid hauls
	ca <- ca[!ca$StNo %in% hh$StNo[hh$HaulVal=='I'],]

	# Number formats
	hh$Distance <- round(hh$Distance)
	hh$Warpingt <- round(hh$Warpingt)
	hh$KiteDim <- round(hh$KiteDim, 1)
	hh$Netopening <- round(hh$Netopening, 1)
	hh$WingSpread <- round(hh$WingSpread, 1)
	hh$DoorSpread <- sprintf("%.1f", round(hh$DoorSpread, 1))
	hh$DataType <- as.character(as.factor(hh$DataType))
	hh$HaulVal <- as.character(as.factor(hh$HaulVal))
	hl$TotalNo <- sprintf("%.2f",round(as.numeric(as.character(hl$TotalNo)), 2))
	hl$HLNoAtLngt <- sprintf("%.2f", round(as.numeric(as.character(hl$HLNoAtLngt)), 2))
	hl$SubFactor <- sprintf("%.4f", round(as.numeric(as.character(hl$SubFactor)), 4))
	hl$SubWgt <- sprintf("%d", round(as.numeric(as.character(hl$SubWgt))))

	##########################################
	## Removing some benthos - this won't be needed in the future
	## keep 11725 138139 138482 138483 140600 140621 140624 140625 141443 141444 141449 153083 153131-- these are cephaolopods
	## required benthos: 107205
	hl <- hl[!hl$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
		            106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
		            107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
		            123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
		            132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
		            144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
		            107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
		            124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
		            143755,145541,145546,145548,532031,589677,1762,123082,149),]

	ca <- ca[!ca$SpecCode %in% c(230,558,830,883,1302,1839,100635,100706,100930,103929,106048,106087,106204,106733,106791,
		            106854,106928,107044,107218,107230,107240,107273,107292,107318,107330,107346,107397,107398,107551,
		            107616,107643,111374,111597,111604,116986,117302,117809,117815,117890,123117,123867,123920,123970,
		            123987,124319,124418,124913,124929,124934,125128,125131,125134,129196,129229,130464,130867,132072,
		            132480,135144,135302,137704,137732,138223,138239,138760,138899,139004,139488,140299,140627,141753,
		            144129,150642,178639,181228,23986719494,21263,100817,100982,106738,107160,107232,107277,107322,
		            107323,107327,107387,107531,107552,107564,107649,107651,111367,123080,123083,123084,123776,123813,
		            124043,124154,124160,124287,124535,125166,125333,128517,129840,138802,138878,138920,140467,140717,
		            143755,145541,145546,145548,532031,589677,1762,123082,149),]

	#more benthods 10216 = skate egg case
	hl <- hl[!hl$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
		            106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
		            117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
		            128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
		            1135,1267,100793),]
	hl <- hl[!hl$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
		            117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
		            167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
		            117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
		            146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]

	ca <- ca[!ca$SpecCode %in% c(443,938,1131,1292,1337,1360,19494,22988,100751,100757,100790,101054,103484,104062,
		            106122,106669,107011,107052,107148,107239,107388,107563,110690,110911,110956,111411,117136,
		            117258,123260,123276,123321,123335,123574,123593 ,123851,123922,123985,124085,125158,125269,
		            128506,130467,130987,131779,134591,137683,141872,146142 ,149864,445590,510534,105,175,927,1107,
		            1135,1267,100793),]
	ca <- ca[!ca$SpecCode %in% c(105,175,927,1107,1135,1267,100793,103443,103692,106057,106835,106903,107558,110908,111361,
		            117940,122348,123160,123426,124257,125027,125284,131495,135294,135301,135306,138992,140528,140687,
		            167882,178527,239867,291396,106763,137656,117225,100653,125125,100698,131774,134366,123386,117228,
		            117994,138923,123127,137701,123320,131629 ,152391,1363,214,103543,106994,103450,129400,140143,
		            146420,141905,22496,988,103717,107163,982,985,123622,102145,1082,10216,103483),]

	hl <- hl[!hl$SpecCode %in% c(-9, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]
	ca <- ca[!ca$SpecCode %in% c(-9, 101,106769,106782,107010,107726,122478,123506,12437,124951,128539,129402,196221,205077,124373, 123187, 124710),]

	## IU: Filter out additional benthos:
	benthosSpecCodes <- c(104,956,966,1128,1296,1367,1608,11707,100782,100839,100854,103439,103732,104040,105865,106041,106673,106702,106789,106834,107152,
				107205,107264,110749,110916,110993,111152,111355,111365,117093,117195,118445,122626,123204,123255,123613,124147,124151,124324,124670,
				128490,128503,129563,130057,134691,136025,137710,138018,138068,138477,138631,138749,138938,140166,140173,140480,140625,141904,141929,
				149854,152997,532035,816800)

	hl <- hl[!hl$SpecCode %in% benthosSpecCodes,]
	ca <- ca[!ca$SpecCode %in% benthosSpecCodes,]


    ## WARN #3:
	## ca records with no HL records
	## these records are because there is no catch weight
	## DATRAS does not accept length info without catch weight
	## so create a line in the HL for each, but give SpecValue=4 and delete ca record

	#IU: Improved cleaning#
	# Use join to find missing value in HL
	testca <- unique(data.frame(StNo=ca$StNo, SpecCode=ca$SpecCode, ca=TRUE))
	testhl <- unique(data.frame(StNo=hl$StNo, SpecCode=hl$SpecCode, hl=TRUE))
	tt <- merge(testca, testhl, by = c("StNo","SpecCode"), all=TRUE)
	missingHL <- tt[is.na(tt$hl),]

	if(nrow(missingHL) > 0) {
		# Populate missing value in HL
		for(idxHL in 1:nrow(missingHL)){
			r <- missingHL[idxHL,]
			tmp <- hl[hl$StNo==r$StNo,][1,]
			tmp$SpecCode <- r$SpecCode
			tmp$SpecVal <- 4
			tmp$TotalNo <- c(hh$HaulDur[hh$StNo==r$StNo])
			tmp$CatCatchWgt <- -9
			hl <- rbind(hl,tmp)
		}
	}

  ## WARN #4:
  # Use plus group for herring and mackerel individuals with age ring above 15
  ca[ which((ca$SpecCode==127023 | ca$SpecCode==126417) & ca$AgeRings >= 15), c("PlusGr", "AgeRings")] <- list("+", 15)

  # Order HL
  hl <- hl[order(hl$StNo),]

  return(list(HH=hh, HL=hl, CA=ca))
}
