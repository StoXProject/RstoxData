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
