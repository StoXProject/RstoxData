if ( requireNamespace("tinytest", quietly = TRUE) ){
	tinytest::test_package("RstoxData", set_env = list(LC_COLLATE = "C", LANG = "en_US.UTF-8"))
}