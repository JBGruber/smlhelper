library("devtools")
library("roxygen2")

setwd(here::here())

## Update date and version
update_description <- function() {
  desc <- readLines("DESCRIPTION")
  date <- desc[grepl("^Date:", desc)]
  date2 <- gsub("[^[:digit:]-]", "", date)
  desc[grepl("^Date:", desc)] <- gsub(date2, Sys.Date(), desc[grepl("^Date:", desc)])
  vers <- desc[grepl("^Version:", desc)]
  vers2 <- gsub("[^[:digit:].]", "", vers)
  vers3 <- readline(prompt = paste("New Version? Old:", vers2))
  if (vers3 == "") {
    vers3 <- vers2
  }
  desc[grepl("^Version:", desc)] <- gsub(vers2, vers3, desc[grepl("^Version:", desc)])
  writeLines(desc, "DESCRIPTION")
}
update_description()

# update documentation
roxygen2::roxygenise(clean = TRUE)

# Checks
devtools::check()
spelling::spell_check_package()
spelling::update_wordlist()
lintr::lint_package()

devtools::install()
