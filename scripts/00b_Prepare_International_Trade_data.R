# -----------------------------------------------------------------------------------------------------------
#
#   Name:           00b_Prepare_International_Trade_data.R
#
#   Description:    
#                   This script shows the procedures taken to make
#                   the International Trade datafile in the shape used for further processing.
#                   In case you start from zero, then download all available years.
#
#                   International trade data available by the Eurostat Bulk Download facility.
#                   The following script will use this Bulk download facility data.
#
#                   The data is also published at the Eurostat website:
#                   http://epp.eurostat.ec.europa.eu/newxtweb/
#
#                   You can also download data from there as well if you prefer, but his is not
#                   what is used in the remainder of the script.
#                   This is how the download from http://epp.eurostat.ec.europa.eu/newxtweb/ works:
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
#
#   Author:         V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------

setwd(DATA_PATH)

options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)
require(plyr)
require(reshape2)
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



# ----------------------------------------------------------
# Collect the data
# ----------------------------------------------------------
# Annual data can be found at
# http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2F2016S2%2Fdata
# Unpack all this in the .\data\international_trade folder

# Metadata can be found at:
# http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&dir=comext%2F201706%2Ftext%2Fenglish
# Change 201706 for the directory of the current year/month.


filenames <- list.files("./international_trade", pattern="^nc.*\\.dat", full.names=TRUE)
filenames <- normalizePath(filenames)
stopifnot(all(file.exists(filenames)))

# Create emtpy dataframe to store the data in.
tbl_CN_all <- data.frame()

for (i in 1:length(filenames)) {
  print(sprintf("************** %d %s **************", i, filenames[i]))

  CN_1Year_data <- read.csv(filenames[i], header = TRUE, colClasses = rep("character", 7) )

  CN_1Year_data$VALUE_1000ECU <- as.numeric(CN_1Year_data$VALUE_1000ECU)
  CN_1Year_data$QUANTITY_TON <- as.numeric(CN_1Year_data$QUANTITY_TON)
  CN_1Year_data$SUP_QUANTITY <- as.numeric(CN_1Year_data$SUP_QUANTITY)
  
  # Partners 1010 (EU-intra total) and 1011 (EU_extra total) are aggregates.
  # Selecting those, or selecting all the others give the same results.
  # Also use only STAT_REGIME code 4 which are totals of all statistical procedures.
  
  selection <- which ( CN_1Year_data$PARTNER %in% c("1010", "1011") & CN_1Year_data$STAT_REGIME == "4" )
  CN_1Year_data <- CN_1Year_data[selection, ]
  CN_1Year_data$STAT_REGIME <- NULL
  
  # Rename PRODUCT_NC to CN
  CN_1Year_data <- rename(CN_1Year_data,c("PRODUCT_NC"="CN"))
  
  # Create Year
  CN_1Year_data$Year <- substr(CN_1Year_data$PERIOD, 1, 4)
  CN_1Year_data$PERIOD <- NULL
  
  # ----------------------------------------------------------
  #  tbl_CN: Only select CN records that are connected with a UNU_Key.
  # ----------------------------------------------------------
  # Don't run this part if you want all products (so also other than electrical)
  # Here the not electrical products are removed for speed and in case of computer memory shortage.
  # Running only the electrical products takes about 50 minutes. All records takes a couple of hours.
  # When running all records also run line 163 to save that data.
  # Only take records that are available in both files
  CN_1Year_data <- merge(CN_1Year_data, htbl_CN_Match_Key, by=c("CN", "Year"))
  CN_1Year_data$UNU_Key <- NULL
  # ----------------------------------------------------------
  
  # Aggregate the partners. 
  CN_1Year_data <- ddply( CN_1Year_data, c("DECLARANT", "CN", "FLOW", "Year"), summarise,
                    VALUE_1000ECU = sum(VALUE_1000ECU, na.rm=TRUE),
                    QUANTITY_TON = sum(QUANTITY_TON, na.rm=TRUE),
                    SUP_QUANTITY = sum(SUP_QUANTITY, na.rm=TRUE) )
  
  # Read metadata
  #REPORTERS <- read.table("./international_trade/REPORTERS.txt", sep = "\t",
  #                        colClasses = c("character", "NULL", "NULL", "character", "NULL", "NULL") )
  
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="001"] <- "FRA"
  # Declarant code '002' is actually "Belg.-Luxbg", but on the http://epp.eurostat.ec.europa.eu/newxtweb/
  # website, al that data is given to country code BEL and none to LUX. So do the same thing
  # over here.
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="002"] <- "BEL"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="003"] <- "NLD"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="004"] <- "DEU"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="005"] <- "ITA"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="006"] <- "GBR"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="007"] <- "IRL"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="008"] <- "DNK"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="009"] <- "GRC"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="010"] <- "PRT"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="011"] <- "ESP"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="017"] <- "BEL"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="018"] <- "LUX"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="030"] <- "SWE"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="032"] <- "FIN"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="038"] <- "AUT"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="046"] <- "MLT"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="053"] <- "EST"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="054"] <- "LVA"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="055"] <- "LTU"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="060"] <- "POL"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="061"] <- "CZE"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="063"] <- "SVK"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="064"] <- "HUN"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="066"] <- "ROU"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="068"] <- "BRG"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="091"] <- "SVN"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="092"] <- "HRV"
  CN_1Year_data$DECLARANT[CN_1Year_data$DECLARANT=="600"] <- "CYP"
  
  # Rename
  CN_1Year_data <- rename(CN_1Year_data,c("DECLARANT"="Country"))

  # Reshape data into wide form while calculating the sum of every group
  # First melt, then cast.
  CN_1Year_data <- melt(CN_1Year_data, id = c("Country", "CN", "FLOW", "Year"))
  CN_1Year_data <- dcast(CN_1Year_data, Country + CN + Year ~ FLOW + variable, value.var = "value",
                sum, na.rm=TRUE)
  
  # Rename new variables
  CN_1Year_data <- rename(CN_1Year_data,c("1_QUANTITY_TON"="Import_Quantity_kg"))
  CN_1Year_data <- rename(CN_1Year_data,c("1_SUP_QUANTITY"="Import_Quantity_Sup"))
  CN_1Year_data <- rename(CN_1Year_data,c("1_VALUE_1000ECU"="Import_Value"))
  CN_1Year_data <- rename(CN_1Year_data,c("2_QUANTITY_TON"="Export_Quantity_kg"))
  CN_1Year_data <- rename(CN_1Year_data,c("2_SUP_QUANTITY"="Export_Quantity_Sup"))
  CN_1Year_data <- rename(CN_1Year_data,c("2_VALUE_1000ECU"="Export_Value"))
  
  # Data in TON still has to be converted to kg.
  CN_1Year_data$Import_Quantity_kg <- CN_1Year_data$Import_Quantity_kg * 1000
  CN_1Year_data$Export_Quantity_kg <- CN_1Year_data$Export_Quantity_kg * 1000
  
  # And 1000 Euros' have to be converted to Euro's.
  CN_1Year_data$Import_Value <- CN_1Year_data$Import_Value * 1000
  CN_1Year_data$Export_Value <- CN_1Year_data$Export_Value * 1000
  
  
  # Add variable Unit to CN data
  CN_1Year_data <- merge(CN_1Year_data, tbl_sup_units, by="CN", all.x = TRUE)
  
  
  # ----------------------------------------------------------
  #  tbl_CN: Add last read data to previously read data files
  # ----------------------------------------------------------
  
  # Create empty dataframe if it does not exist yet.
  if (exists("tbl_CN_all") == FALSE)
  {tbl_CN_all <- data.frame()}
  
  # Append the last read data to this file to makea datafile for all years
  tbl_CN_all <- rbind(tbl_CN_all, CN_1Year_data)
  
}

#write.csv(tbl_CN_all, file = "tbl_CN_all_products.csv", quote = TRUE, row.names = FALSE)

# Select only electronic products.
tbl_CN_all <- merge(tbl_CN_all, htbl_CN_Match_Key, by=c("CN", "Year"))
tbl_CN_all$UNU_Key <- NULL



# ----------------------------------------------------------
#  tbl_CN_all: Clean up and save result
# ----------------------------------------------------------

# Sort order for columns
sortorder_c <- c("Country", "CN", "Year", "Import_Quantity_Sup", "Import_Value",
                 "Export_Quantity_Sup", "Export_Value", "Unit",
                 "Import_Quantity_kg",  "Export_Quantity_kg")

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
