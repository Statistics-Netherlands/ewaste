# -----------------------------------------------------------------------------------------------------------
#
#   Name:           00a_Prepare_Prodcom_data.R
#
#   Description:    Prodcom data is published at the Eurostat website:
#                   http://ec.europa.eu/eurostat/web/prodcom/data/database
#
#                   Data including the Prodcom codes referring to Electronic Equipment are only available in
#                   Excel sheets.
#
#                   These data files are autmatically downloaded at the start of this script
#                   and placed at the right location.
#
#                   In case the download does not work, download the files manually:
#                   - For the years 1995-2007 choose:
#                       Detailed data by Prodcom LIST (NACE Rev. 1.1) (prom1) (Excel tables N1)
#                   - For the years 2008 and later choose:
#                       Detailed data by Prodcom LIST (NACE Rev. 2) (prom2) (Excel tables N2)
#
#                   Every year has a separate file, for instance Website_snapshot_2015_N2.
#                   Download every available year. Make sure that all files have a name starting with:
#                   "Website_snapshot_" followed by the year that the data refers to.
#
#                   Place the saved files in the "prodcom_import" folder of the R working directory.
#                   Each Excel file contains a few worksheets. Only "Value" and "Sold Volume" are used.
#   
#                   Run the following R script to create a single datafile that can be used in this project.
#
#   Author:         V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------


setwd(DATA_PATH)

options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)
require(readxl)


# ----------------------------------------------------------
# Download the Prodcom files
# ----------------------------------------------------------

# 2015
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2015_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2015_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2014
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2014_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2014_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2013
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2013_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2013_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2012
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2012_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2012_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2011
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2011_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2011_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2010
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2010_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2010_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2009
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2009_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2009_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2008
url <- "http://ec.europa.eu/eurostat/documents/120432/120476/Website_snapshot_2008_N2.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2008_N2.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2007
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website_snapshot_2007_N1.xlsx"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2007_N1.xlsx", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2006
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website_snapshot_2006_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2006_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2005
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website_snapshot_2005_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2005_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2004
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website_snapshot_2004_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2004_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2003
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website_snapshot_2003_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2003_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2002
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-2002-created-2009-11-09-N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2002-created-2009-11-09-N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2001
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-2001-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2001-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 2000
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-2000-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_2000-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 1999
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-1999-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_1999-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 1998
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-1998-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_1998-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 1997
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-1997-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_1997-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 1996
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-1996-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_1996-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

# 1995
url <- "http://ec.europa.eu/eurostat/documents/120432/6191935/Website-snapshot-1995-created-2009-11-09_N1.xls"
destfile <- paste(DATA_PATH, "prodcom_import/Website_snapshot_1995-created-2009-11-09_N1.xls", sep = "/")
download.file(url, destfile, quiet = FALSE, mode = "wb",
              cacheOK = TRUE, extra = getOption("download.file.extra"))

rm(url)
rm(destfile)



# ----------------------------------------------------------
# htbl_PCC_Match_Key: Read conversion table PCC to UNU_Keys, which is used later to select only relevant Prodcom codes
# ----------------------------------------------------------
htbl_PCC_Match_Key <- read.csv("htbl_PCC_Match_Key.csv", quote = "\"",
                                 colClasses = c("character", "character", "character"))

# Remove empty values
htbl_PCC_Match_Key <- htbl_PCC_Match_Key[(htbl_PCC_Match_Key$PCC != "" & htbl_PCC_Match_Key$UNU_Key != ""), ]

# ----------------------------------------------------------
# Read the Excel files with Prodcom data
# ----------------------------------------------------------

# All values of the prodcom data in the Excel files from Eurostat website are expressed in thousands of euro's.
# All volumes are expressed in thousands of the given unit

filenames <- list.files("./prodcom_import", pattern="^Website_snapshot_.*\\.xls", full.names=TRUE)
filenames <- normalizePath(filenames)
stopifnot(all(file.exists(filenames)))

for (i in 1:length(filenames)) {
  print(sprintf("************** %d %s **************", i, filenames[i]))

  # Read volume data. We just use it for the column names, so warnings don't matter.
  PCC_SoldVolume <- read_excel(filenames[i], sheet = "Sold Volume", 
                               na="-",
                               skip = 2)
  colnames <- names(PCC_SoldVolume)
  
  # Read again but now with column names as a data row. This way ensures all are read as characters.
  PCC_SoldVolume <- read_excel(filenames[i], sheet = "Sold Volume",
                               col_names = FALSE,
                               na="-",
                               skip = 2)
  # Resore column names
  names(PCC_SoldVolume) <- colnames
  
  # Remove first 4 rows. 
  PCC_SoldVolume <- PCC_SoldVolume[5:nrow(PCC_SoldVolume), ]
  
  # Use only first 8 characters for prodcom code
  PCC_SoldVolume$"PRODCOM Code" <- substr(PCC_SoldVolume$"PRODCOM Code", 1, 8)
  
  # Read value. We just use it for the column names, so warnings don't matter.
  PCC_Value <- read_excel(filenames[i], sheet = "Value", 
                               na="-",
                               skip = 2)
  colnames <- names(PCC_Value)
  
  # Read again but now with column names as a data row. This way ensures all are read as characters.
  PCC_Value <- read_excel(filenames[i], sheet = "Value",
                               col_names = FALSE,
                               na="-",
                               skip = 2)
  
  # Resore column names
  names(PCC_Value) <- colnames
  
  # Remove first 4 rows
  PCC_Value <- PCC_Value[5:nrow(PCC_Value), ]
  
  # Use only first 8 characters for prodcom code
  PCC_Value$"PRODCOM Code" <- substr(PCC_Value$"PRODCOM Code", 1, 8)
  
  
  
  # ### Clean-up PCC_SoldVolume ###
  
  # Remove flag and Base variables
  flagindexes <- grep("^flag", names(PCC_SoldVolume))
  baseindexes <- grep("^Base", names(PCC_SoldVolume))
  PCC_SoldVolume <- PCC_SoldVolume[, -c(flagindexes, baseindexes)]
  
  # Melt all years into long form
  PCC_SoldVolume <- melt(PCC_SoldVolume, id = c("PRODCOM Code", "Unit"))
  
  # Rename
  PCC_SoldVolume <- plyr::rename(PCC_SoldVolume,c("variable"="Country_Name", "value"="SoldVolume"))
  
  # Clean up Country_Name
  # Change or make sure that Country_Name is not a factor but a character string
  PCC_SoldVolume$Country_Name <- as.character(PCC_SoldVolume$Country_Name)
  
  selection <- grep("^Volume", PCC_SoldVolume$Country_Name)
  PCC_SoldVolume[selection, "Country_Name"] <- substr(PCC_SoldVolume[selection, "Country_Name"],
                                                      (nchar(PCC_SoldVolume[selection, "Country_Name"])+1) -4,
                                                      nchar(PCC_SoldVolume[selection, "Country_Name"]))
  
  # Remove empty countries. These come from the comments on the right of the source Excel file.
  PCC_SoldVolume <- PCC_SoldVolume[PCC_SoldVolume$Country_Name!="",]
  
  # Rename
  PCC_SoldVolume <- plyr::rename(PCC_SoldVolume,c("PRODCOM Code"="PCC"))
  
  # Create column with year
  pos <- regexpr("Website_snapshot_",filenames[i])[1] # regexpr gives 3 elements. We only need the first.
  datayear <- substr(filenames[i], pos+nchar("Website_snapshot_"), pos+nchar("Website_snapshot_")+3)
  PCC_SoldVolume$Year <- datayear
  
  # We only need prodcom codes that link to EEE. Therefore add UNU_Key and select only those with a value
  PCC_SoldVolume <- merge( PCC_SoldVolume, htbl_PCC_Match_Key,
                           by=c("PCC", "Year"))
  
  PCC_SoldVolume$UNU_Key <- NULL
  
  
  
  # ### Clean-up PCC_Value ###
  # Remove the decimals that appear after import
  PCC_Value$`PRODCOM Code` <- substr(PCC_Value$`PRODCOM Code`,1,8)
  
  # Remove unit, flag and Base variables
  unitindexes <- grep("^Unit", names(PCC_Value))
  flagindexes <- grep("^flag", names(PCC_Value))
  baseindexes <- grep("^Base", names(PCC_Value))
  PCC_Value <- PCC_Value[, -c(unitindexes, flagindexes, baseindexes)]
  
  # Melt all years into long form
  PCC_Value <- melt(PCC_Value, id = "PRODCOM Code")
  
  # Rename
  PCC_Value <- plyr::rename(PCC_Value,c("variable"="Country_Name", "value"="Value"))
  
  # Remove empty countries. These come from the comments on the right of the source Excel file.
  PCC_Value <- PCC_Value[PCC_Value$Country_Name!="",]
  
  # Clean up Country_Name
  # Change or make sure that Country_Name is not a factor but a character string
  PCC_Value$Country_Name <- as.character(PCC_Value$Country_Name)
  
  selection <- grep("^Value", PCC_Value$Country_Name)
  PCC_Value[selection, "Country_Name"] <- substr(PCC_Value[selection, "Country_Name"],
                                                 (nchar(PCC_Value[selection, "Country_Name"])+1) -4,
                                                 nchar(PCC_Value[selection, "Country_Name"]))
  
  # Rename
  PCC_Value <- plyr::rename(PCC_Value,c("PRODCOM Code"="PCC"))
  
  # Create column with year.
  PCC_Value$Year <- datayear
  
  # We only need prodcom codes that link to EEE. Therefore add UNU_Key and select only those with a value
  PCC_Value <- merge( PCC_Value, htbl_PCC_Match_Key,
                      by=c("PCC", "Year"))
  
  PCC_Value$UNU_Key <- NULL
  
  # Combine sold volume and value
  prodcom_single_year <- merge( PCC_SoldVolume, PCC_Value,
                                by=c("PCC", "Country_Name", "Year"),  all.x = TRUE)
  
  # Now the file is correct, we append it to dataframe name containing all years
  # start with empty vector
  if (i == 1)
  {prodcom <- data.frame() }
  # Append the data read to this file
  prodcom <- rbind(prodcom, prodcom_single_year)
  
}



# ----------------------------------------------------------
# Stratum: Read table with country codes and names
# ----------------------------------------------------------
tbl_Countries <- read.csv("tbl_Countries.csv", quote = "\"",
                            colClasses = c("character", "character", "NULL"))


# ----------------------------------------------------------
# prodcom: Attach country codes and clean-up file
# ----------------------------------------------------------
prodcom <- merge(prodcom, tbl_Countries,  by="Country_Name",  all.x = TRUE)
rm(tbl_Countries)

# CountryCode2 is not needed
prodcom$CountryCode2 <- NULL

# Convert country codes to uppercase.
prodcom$Country <- toupper(prodcom$Country)

# Only use EU28 aggregates. Select all other aggregates for removal.
selection <- which( substr(prodcom$Country_Name,1,2) == "EU" &
                      !substr(prodcom$Country_Name,1,4) == "EU28" )

prodcom <- prodcom[-selection, ]

# EU aggregate values have no country code. Copy EU values here.
selection <- grep("^EU", prodcom$Country_Name)
prodcom[selection, "Country"] <- prodcom[selection, "Country_Name"]

# Empty codes are results from empty columns that have been read during import of the data from Excel
prodcom <- prodcom[!is.na(prodcom$Country), ]

# Names can go
prodcom$Country_Name <- NULL

# empty records ("-" mean NA)
selection <- which( prodcom$SoldVolume=="-" |  prodcom$SoldVolume=="")
prodcom[selection,"SoldVolume"] <- NA

# Create conf variable that tells if a Prodcom value is confident or not.
prodcom$conf <- ifelse(substr(prodcom$SoldVolume, 1, 1) %in% c(":", "C"), 1, 0) 

# Convert prodcom units to numeric if not confident.
# Prodcom volume is expressed in thousands. Convert them also to single units.
selection <- which( prodcom$conf == 0 & !is.na(prodcom$SoldVolume) )
prodcom[selection,"prodcom_units"] <- as.numeric(prodcom[selection, "SoldVolume"]) * 1000

# Original variable not needed anymore
prodcom$SoldVolume <- NULL

# Remove non numeric codes from Value column. Only values are needed here.
selection <- which( substr(prodcom$Value, 1, 1) %in% c(":", "C") )
prodcom[selection, "Value"] <- NA

# Some values can contain comma's. We need to replace them to dots.
prodcom$Value <- gsub(",", ".", prodcom$Value)

# Convert Value to numeric, convert from 1000 euro's to single euro's and round.
prodcom$Value <- round(as.numeric(prodcom$Value) * 1000, digits = 0)


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Clean up and save result
# ----------------------------------------------------------

# Sort order for columns
sortorder_c <- c(5, 2, 1, 4, 7, 3, 6)

# Sort dataframe rows by Country, Year and PCC.
sortorder <- order(prodcom$Country, prodcom$Year, prodcom$PCC)
prodcom <- prodcom[sortorder, sortorder_c]

write.csv(prodcom, file = "tbl_data_pcc_conf.csv", quote = FALSE, row.names = FALSE)
