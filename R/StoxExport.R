
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
      tmp <- data.frame(aco$DataProcessing)
      names(tmp)<-paste0('DataProcessing',names(tmp))
      hl <- cbind(hl,tmp)
      HCru <- format(hl, trim=TRUE, width=0)
      
      
      hl<-c()
      hl$Cruise <- 'Data'
      hl$Header <- 'Record'
      tmp <- data.frame(aco$Data)
      names(tmp)<-paste0('DataProcessing',names(tmp))
      hl <- data.frame(aco$Data)
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
