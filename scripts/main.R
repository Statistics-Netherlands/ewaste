#' @title Run the entire e-waste analysis sequence.
#'
#' @description
#' 'Master' script that allows one to run the entire e-waste analysis.
#'
#' @details
#' This script should be sourced with Rscript.
#'
# Usage:
# rscript.exe main.R [-[-countries|c] [<character>]] [-[-help|h]]
#
# For example:
# --- run the analyses for all countries
# rscript main.R
#
# --- run the analyses for Austria and Belgium alone
# rscript.exe --countries=AUT;BEL
 

#------------------------------------
# define path for source code including check if variable has been set
stopifnot(file.exists(Sys.getenv("EWASTE_SCRIPT_PATH")))
SCRIPT_PATH <- (Sys.getenv("EWASTE_SCRIPT_PATH"))
#------------------------------------


require(tcltk)
require(getopt)


#------------------------------------
# parse commandline options.

spec = matrix(c('countries', 'c', 2, "character",
                'help', 'h', 0, "logical")
              , byrow=TRUE, ncol=4)
myoptions = getopt(spec)

# if help was asked for print a friendly message
# and exit with a non-zero error code
if ( !is.null(myoptions$help) ) {
  cat(getopt(spec, usage=TRUE));
  q(status=1);
}

# set some reasonable defaults for the options that are needed, but were not specified.
tbl_Countries <- normalizePath(file.path(SCRIPT_PATH, "..\\data\\tbl_Countries.csv"))
all_countries <- read.csv(tbl_Countries, quote = "\"",
                          colClasses = c("character", "NULL", "NULL", "NULL"))$Country
all_countries <- sort(toupper(unique(all_countries)))

# use all countries unless one or more countries are specified
if ( is.null(myoptions$countries) ) {
  myoptions$countries <- all_countries 
} else {
  myoptions$countries <- toupper(strsplit(myoptions$countries, "\\W+")[[1]])
  diff <- setdiff(myoptions$countries, all_countries)
  if ( length(diff) ) {
    stop(sprintf("Unknown country: %s\nValid countries are: %s\n", 
                 paste(diff, collapse=", "),
                 paste(all_countries, collapse=", ")))
  }
}
PROCESS_ALL_COUNTRIES <- length(myoptions$countries) == length(all_countries)



# define path for data.
DATA_PATH <- normalizePath(file.path(SCRIPT_PATH, "..", "data"))
print(sprintf("Looking for R scripts in '%s'", SCRIPT_PATH))
print(sprintf("Looking for data in '%s'", DATA_PATH))
stopifnot(all(file.exists(c(SCRIPT_PATH, DATA_PATH))))



#------------------------------------
# run all scripts
scripts <- list(#"00a_Prepare_Prodcom_data.R",
                 #"00b_Prepare_nternational_Trade_data.R",
                "01_Prodcom_confidentials.R",
                "02_Apparent_Consumption_Method.R",
                "03_POM_calculations.R", 
                "04_WEEE_calculations.R",
                "05_Make_tblAnalysis.R"
                )
                

execute <- function(script) {
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  print(sprintf("... %s: Sourcing script '%s'", stamp, script))
  source(file.path(SCRIPT_PATH, script))
}
crap <- lapply(scripts, execute)

