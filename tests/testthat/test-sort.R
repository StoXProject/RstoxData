context("test-sort")
expectedSort <- c("havsil", "kutlingfamilien", "øyepål", "rødspette", "Tunge")
locales <- stringi::stri_locale_list()


s <- lapply(locales, function(loc) stringr::str_sort(expectedSort, locale = loc))
ss <- sapply(s, paste, collapse = ",")
#write(paste(names(table(ss)), table(ss), sep = ": ", collapse = "; "), "~/testtest.txt")
warning(paste(names(table(ss)), table(ss), sep = ": ", collapse = "; "))

warning(locales[startsWith(ss, "Tunge")])

warning(Sys.getlocale("LC_COLLATE"))

cloc <- stringr::str_sort(expectedSort, locale = "C")
warning(paste(cloc, collapse = "; "))


Sys.setlocale(category = "LC_COLLATE", locale = "C")
warning(Sys.getlocale("LC_COLLATE"))
cloc <- stringr::str_sort(expectedSort, locale = "C")
warning(paste(cloc, collapse = "; "))


set.seed(1)
dt <- data.table::data.table(
	expectedSort = sample(expectedSort)
)
warning(paste(dt$expectedSort, collapse = ", "))
data.table::setorderv(dt)
warning(paste(dt$expectedSort, collapse = ", "))





expect_equal(expectedSort, cloc)
