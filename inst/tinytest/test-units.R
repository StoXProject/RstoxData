#context("Test unit conversion")

symbol <- findUnit("mass", "kg")
shortname <- findUnit("mass", shortname="kg")
name <- findUnit("mass", name="kilogram")

expect_equal(symbol, "mass-kg")
expect_equal(shortname, "mass-kg")
expect_equal(name, "mass-kg")

expect_error(findUnit("mass", "m"), "m is not a valid shortname for quantity: mass")
expect_error(findUnit("mass", shortname="m"), "m is not a valid shortname for quantity: mass")
expect_error(findUnit("mass", name="m"), "m is not a valid name for quantity: mass")

expect_error(findUnit("nonsense", name="m"), "nonsense is not a valid quantity.")

m <- RstoxData:::convertUnits(1, "length-km", "length-m")
expect_equal(m,1000)

m <- RstoxData:::convertUnits(2.5, "mass-g", "mass-kt")
expect_equal(m,2.5e-9)

expect_error(RstoxData:::convertUnits(2.5, "length-nmi", "mass-kt"), "length-nmi and mass-kt are of different quantities.")
expect_error(RstoxData:::convertUnits(2.5, "kts", "mass-kt"), "kts not found in 'conversionTable'")

m <- RstoxData:::convertUnits(c(1,2), "length-km", "length-nmi")

expect_equal(m,c(1000/1852, 2000/1852))

m1 <- RstoxData:::convertUnits(20, "fraction-percent", "fraction-decimal")
expect_equal(m1,.2)
m2 <- RstoxData:::convertUnits(.20, "fraction-decimal", "fraction-percent")
expect_equal(m2,20)

dt <- data.table::data.table(weight=c(1000,1200), var=c("v1","v2"))
dt$weight <- setUnit(dt$weight, "mass-g")
expect_equal(attr(dt$weight, "stoxUnit"), "mass-g")
expect_equal(dt$weight[1:2], c(1000,1200))

dt$weight <- setUnit(dt$weight, "mass-kg")
expect_equal(attr(dt$weight, "stoxUnit"), "mass-kg")
expect_equal(dt$weight[1:2], c(1.000,1.200))

expect_error(setUnit(dt$weight, "fraction-percent"), "mass-kg and fraction-percent are of different quantities")

expect_equal(getUnit(dt$weight), "mass-kg")
expect_equal(getUnit(dt$weight, "name"), "kilogram")
expect_true(is.na(getUnit(dt$var)))
expect_error(getUnit(dt$weight, "names"), "'arg' should be one of")

expect_true(!is.null(attr(dt$weight, "stoxUnit")))
dt$weight <- setUnit(dt$weight, NA)
expect_null(attr(dt$weight, "stoxUnit"))

massoptions <- getUnitOptions("mass", property="symbol")
expect_true(all(c("g", "kg", "t", "kt") %in% massoptions))
massoptions <- getUnitOptions("mass", property="symbol", conversionRange=c(1e-3,1e3))
expect_true(all(c("g", "kg", "t") %in% massoptions))
expect_false(all(c("kt") %in% massoptions))
lengthoptions <- getUnitOptions("length", property="symbol")
expect_true(all(c("mm", "cm", "m", "km", "M") %in% lengthoptions))
expect_error(getUnitOptions("nonsense"), "nonsense is not a valid quantity.")

