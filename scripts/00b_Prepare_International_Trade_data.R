# -----------------------------------------------------------------------------------------------------------
#
#   Name:           00b_Prepare_International_Trade_data.R
#
#   Description:    
#                   This script shows the procedures taken to make
#                   the International Trade datafile in the shape used for further processing.
#                   In case you start from zero, then download a one or few years together.
#
#                   International trade data is published at the Eurostat website:
#                   http://epp.eurostat.ec.europa.eu/newxtweb/
#
#                   Make sure you register yourself so you can download the big amount of data
#                   Go to "Available datasets" --> "INTERNATIONAL TRADE" -->
#                     "EU Trade Since 1988 By CN8 (DS-016890)". Click on the "New Query" icon.
#                   
#                   Fill the following fields in Step 1 "Dimension Selection":
#                   
#                     Reporter:
#                       Select all countries except the aggregations starting with EA, EU or EUROZONE.
#                       Click the "select" button when you are done.
#                     Partner:
#                       Select only:
#                         - EU25_INTRA-EU25-EXTRA
#                         - EU25_INTRA-EU25-INTRA
#                     Product:
#                       Select only the following CN codes (the advanced selection box is very helpful):
#                         - 63011000
#                         - 84*
#                         - 85*
#                         - 87119010 + 87119090
#                         - 90*
#                         - 91*
#                         - 92*
#                         - 94*
#                         - 95*
#                     Flow:
#                       Select all
#                     Period:
#                       Select the years for which you would like to download data. You can choose up to 6 years,
#                       at once which will result in a download of about 20MB.
#                       Choose only the complete years.
#                       For instance for year 2015 I choose: 201552-Jan.Dec.2015.
#                     Indicators:
#                       Select all
#
#                   After this press the Compress button to remove codes that do not generate data.
#
#                   In Step 2 "Layout selection" make the following selection:
#                       Rows:
#                         - Dimension#1: PERIOD
#                         - Dimension#2: FLOW
#                         - Dimension#3: REPORTER
#                         - Dimension#4: PRODUCT
#                       Columns:
#                         - Dimension#1: INDICATORS
#                         - Dimension#2: PARTNER
#
#                       All formats are codes
#
#                   In Step 3 "Output selection" do the following:
#                       - Select: Batch extraction
#                       - Select: Also generate output
#                       - Select Show in output: Codes
#                       - Select output format: CSV
#
#                   Give extraction  name and it is useful to select "Notify me whenever this dataset is uploaded"
#                   Click the "Finish" button.
#                   Once completed you can download the data under the tab "Completed Works".
#                   Place it in the folder '.\ewaste-master\data\international_trade'.
#
#                   Extract and rename the datafile into something meaningful like "IT2010-2015.csv"
#                   Enter the filenames in the list "ITdata" in the script below at line 90.
#   
#                   Run the following R script to put it in the shape used for further processing.
#
#   Author:         V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------

setwd(DATA_PATH)

options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)


# Names of files to read.
ITdata <-list("IT_1995-1998.csv",
  "IT_1999-2003.csv",
  "IT_2004-2009.csv",
  "IT_2010-2015.csv"
)



# First read data needed in the process of processing the International Trade data.

# ----------------------------------------------------------
# tbl_Countries: Read table with country codes
# ----------------------------------------------------------
tbl_Countries <- read.csv("tbl_Countries.csv", quote = "\"",
                          colClasses = c("character", "NULL", "NULL", "character"))

# Convert country codes to uppercase.
tbl_Countries$Country <- toupper(tbl_Countries$Country)

# rename
tbl_Countries <- rename(tbl_Countries,c("CountryCode2"="REPORTER"))


# ----------------------------------------------------------
# tbl_sup_units: Read the CSV file with the supplementary units information
# ----------------------------------------------------------

# The supplentary quanity units are needed to know what the supplementary quantity means.
# When the units are Number of items (p/st) they will be used in the calculations.

tbl_sup_units <- read.csv("htbl_supplementary_units.csv", header = TRUE,
                          colClasses = c("character", "character"))


# ----------------------------------------------------------
# htbl_CN_Match_Key: Read conversion table CN to UNU_Keys to select only relevant CN codes
# ----------------------------------------------------------
htbl_CN_Match_Key <- read.csv("htbl_CN_Match_Key.csv", quote = "\"",
                              colClasses = c("character", "character", "character"))






# Pass filenames to code that processes the International Trade data.
# This will read the varous files specified in the list at row 90 and generate
# a combined dataframe in the form that is needed for the rest of the code.
for (i in 1:length(ITdata) ){
  print(sprintf("************** %d %s **************", i, ITdata[i]))
  
  # ----------------------------------------------------------
  # Read the CSV file with the International trade data
  # ----------------------------------------------------------
  tbl_CN <- read.csv( paste( "./international_trade/", ITdata[i], sep = ""), header = TRUE,
                      colClasses = c("character", "character", "character", "numeric", "numeric", "character",
                                     "numeric"))
  
  
  # Aggregate EU28-INTRA and EU28-EXTRA to World data
  tbl_CN <- ddply( tbl_CN, c("REPORTER", "PRODUCT", "FLOW", "PERIOD", "INDICATORS"), summarise,
                   INDICATOR_VALUE = sum(INDICATOR_VALUE, na.rm=TRUE) )
  
  
  
  
  # ----------------------------------------------------------
  # tbl_CN: Convert country codes and reshape data
  # ----------------------------------------------------------
  
  # Merge International Trade data with Country codes list
  tbl_CN <- merge(tbl_CN, tbl_Countries, by="REPORTER", all.x = TRUE)
  
  # REPORTER is not needed anymore
  tbl_CN$REPORTER <- NULL
  
  # Create Year
  tbl_CN$Year <- substr(tbl_CN$PERIOD, 1, 4)
  tbl_CN$PERIOD <- NULL
  
  # Cast data into wide form while calculating the sum of every group
  tbl_CN <- dcast(tbl_CN, PRODUCT + Country + Year ~ FLOW + INDICATORS, value.var = "INDICATOR_VALUE",
                  sum, na.rm=TRUE)
  
  # Rename new variables
  tbl_CN <- rename(tbl_CN,c("1_QUANTITY_IN_100KG"="Import_Quantity_kg"))
  tbl_CN <- rename(tbl_CN,c("1_SUPPLEMENTARY_QUANTITY"="Import_Quantity_Sup"))
  tbl_CN <- rename(tbl_CN,c("1_VALUE_IN_EUROS"="Import_Value"))
  tbl_CN <- rename(tbl_CN,c("2_QUANTITY_IN_100KG"="Export_Quantity_kg"))
  tbl_CN <- rename(tbl_CN,c("2_SUPPLEMENTARY_QUANTITY"="Export_Quantity_Sup"))
  tbl_CN <- rename(tbl_CN,c("2_VALUE_IN_EUROS"="Export_Value"))
  
  # Data in 100kg still has to be converted to kg.
  tbl_CN$Import_Quantity_kg <- tbl_CN$Import_Quantity_kg * 100
  tbl_CN$Export_Quantity_kg <- tbl_CN$Export_Quantity_kg * 100
  
  # Rename PRODUCT to CN
  tbl_CN <- rename(tbl_CN,c("PRODUCT"="CN"))
  
  
  # Add variable Unit to CN data
  tbl_CN <- merge(tbl_CN, tbl_sup_units, by="CN", all.x = TRUE)
  
  
  
  
  # ----------------------------------------------------------
  #  tbl_CN: Only select CN records that are connected with a UNU_Key
  # ----------------------------------------------------------
  
  # Only take records that are available in both files
  tbl_CN <- merge(tbl_CN, htbl_CN_Match_Key, by=c("CN", "Year"))
  tbl_CN$UNU_Key <- NULL
  
  
  # ----------------------------------------------------------
  #  tbl_CN: Add last read data to previously read data files
  # ----------------------------------------------------------
  
  # Create empty dataframe if it does not exist yet.
  if (exists("tbl_CN_all") == FALSE)
  {tbl_CN_all <- data.frame()}
  
  # Append the last read data to this file to makea datafile for all years
  tbl_CN_all <- rbind(tbl_CN_all, tbl_CN)
  
}




# ----------------------------------------------------------
#  tbl_CN_all: Clean up and save result
# ----------------------------------------------------------

# Sort order for columns
sortorder_c <- c(3, 1, 2, 5, 6, 8, 9, 10, 4, 7)

# Sort dataframe rows by Country, Year and PCC.
sortorder <- order(tbl_CN_all$Country, tbl_CN_all$Year, tbl_CN_all$CN)
tbl_CN_all <- tbl_CN_all[sortorder, sortorder_c]

write.csv(tbl_CN_all, file = "tbl_CN.csv", quote = TRUE, row.names = FALSE)


# Clean up
rm(tbl_Countries)
rm(tbl_sup_units)
rm(htbl_CN_Match_Key)
rm(tbl_CN_all)
rm(tbl_CN)
