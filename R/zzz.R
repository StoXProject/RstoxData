# Set local package extdata location
utils::globalVariables(c("xsdObjects", "localEnv", ":=") )

.onLoad <- function(libname, pkgname){
	assign("localEnv", new.env(), parent.env(environment()))
	fpath <- system.file("extdata", package = pkgname)
	assign("fpath", fpath, envir = localEnv)
}

# Try to unload dynamic library
.onUnload <- function (libpath) {
	library.dynam.unload("RstoxData", libpath)
}
