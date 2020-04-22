# Satisfy R CMD check
options("mc.cores" = 2)


context("test-StoxExport: DATRAS export")
example <- system.file("testresources", "biotic_v3_example.xml", package="RstoxData")	
data <- ReadBiotic(example)

data[[1]]$fishstation[, stationstartdate := stationstopdate]
datras1 <- RstoxData:::generateDATRAS(data, save = FALSE)
datras2 <- RstoxData:::generateDATRAS(data, save = TRUE)
expect_equal(nrow(datras2[[1]]$HH), 2)



context("test-StoxExport: ICES acoustic export #1")
example <- system.file("testresources", "ICES_Acoustic_1.xml", package="RstoxData")
data <- ReadAcoustic(example)
icesacoustic1 <- RstoxData:::write2ICESacoustic_CSV(data, save = FALSE)
icesacoustic2 <- RstoxData:::write2ICESacoustic_CSV(data, save = TRUE)

expect_equal(nrow(icesacoustic1$Data), 11)

context("test-StoxExport: ICES acoustic export #2")
example <- system.file("testresources", "ICES_Acoustic_2.xml", package="RstoxData")
data <- ReadAcoustic(example)
icesacoustic1 <- RstoxData:::write2ICESacoustic_CSV(data, save = FALSE)
icesacoustic2 <- RstoxData:::write2ICESacoustic_CSV(data, save = TRUE)

expect_equal(nrow(icesacoustic2$Data), 12)
