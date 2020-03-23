
write2ICESacoustic_CSV <- function(Acoustic){
  
  for(aco in Acoustic){
    if(aco$metadata$useXsd=='icesAcoustic'){
      
      hl <- data.frame(aco$Instrument)
      HInst <- format(hl, trim=TRUE, width=0)
      
      hl <- data.frame(aco$Calibration)
      HCal <- format(hl, trim=TRUE, width=0)
      
      hl <- data.frame(aco$DataAcquisition)
      HDatA <- format(hl, trim=TRUE, width=0)
      
      hl <- data.frame(aco$DataProcessing)
      HDatP <- format(hl, trim=TRUE, width=0)
      
      hl <- data.frame(aco$Cruise)
      HCru <- format(hl, trim=TRUE, width=0)
      
      hl <- data.frame(aco$Data)
      HDat <- format(hl, trim=TRUE, width=0)
      
      tmp <- list(Instrument=HInst,
                  Calibration=HCal,
                  DataAcquisition=HDatA,
                  DataProcessing=HDatP,
                  Cruise=HCru,
                  Data=HDat
      )
      
      suppressWarnings(lapply(tmp, write.table, file=gsub('.xml','.csv',aco$metadata$file), append=TRUE, row.names=FALSE, quote=FALSE, sep=","))
      
    }else{'Warning: only ices acoustic format is allowed'}

  }
}
