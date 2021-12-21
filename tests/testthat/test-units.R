context("Test unit conversion")

m <- convertUnits(1, "km", "m")
expect_equal(m,1000)

m <- convertUnits(2.5, "g", "kt")
expect_equal(m,2.5e-9)

expect_error(convertUnits(2.5, "nmi", "kt"), "nmi and kt are of different quantities.")
expect_error(convertUnits(2.5, "kts", "kt"), "Symbol kts not found in 'conversionTable'")

m <- convertUnits(c(1,2), "km", "nmi")

expect_equal(m,c(1000/1852, 2000/1852))

m1 <- convertUnits(20, "%", "0.")
expect_equal(m1,.2)
m2 <- convertUnits(.20, "0.", "%")
expect_equal(m2,20)

dt <- data.table::data.table(weight=c(1000,1200), var=c("v1","v2"))
dt$weight <- setUnit(dt$weight, "g")
expect_equal(attr(dt$weight, "stoxUnit"), "g")
expect_equal(dt$weight[1:2], c(1000,1200))

dt$weight <- setUnit(dt$weight, "kg")
expect_equal(attr(dt$weight, "stoxUnit"), "kg")
expect_equal(dt$weight[1:2], c(1.000,1.200))

expect_error(setUnit(dt$weight, "%"), "kg and % are of different quantities")


expect_equal(getUnit(dt$weight), "kg")
expect_equal(getUnit(dt$weight, "name"), "kilogram")
expect_true(is.na(getUnit(dt$var)))
expect_error(getUnit(dt$weight, "names"), "'arg' should be one of")


massoptions <- getUnitOptions("mass")
expect_true(all(c("g", "kg", "t", "kt") %in% massoptions))
lengthoptions <- getUnitOptions("length")
expect_true(all(c("mm", "cm", "m", "km", "nmi") %in% lengthoptions))
expect_error(getUnitOptions("nonsense"), "nonsense is not a valid quantity.")