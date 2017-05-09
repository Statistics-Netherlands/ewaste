# -----------------------------------------------------------------------------------------------------------
#
#   Name:             05_WEEE_calculations.R
#
#   Description:      This script reads the CSV file with the altered POM data.
#                     Then aggregates and external data are added for further analysis.
#
#
#   Author:           dr C.P. Balde - Statistics Netherlands,
#                     V.M. van Straalen - Statistics Netherlands
#
#   Revised version:  V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------


setwd(DATA_PATH)
options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)


# ----------------------------------------------------------
# htbl_Key_Aggregates: Read table with publication categories
# ----------------------------------------------------------

htbl_Key_Aggregates <- read.csv("htbl_Key_Aggregates.csv", quote = "\"",
                                 colClasses = c("character", "character","character"))


# ----------------------------------------------------------
# htbl_Key_Description: Read table descriptions of all UNU_Keys and its aggregates
# ----------------------------------------------------------


htbl_Key_Description <- read.csv("htbl_Key_Description.csv", quote = "\"",
                                  colClasses = c("character", "character"))



# ----------------------------------------------------------
# Population: Read population data
# ----------------------------------------------------------
Population <- read.csv("tbl_Data.csv", quote = "\"",
                       colClasses = c("character", "numeric", "numeric", "character"))

# Only keep data about number of inhabitants
Population <- Population[Population$Destination == "pop 17", ]

# Convert country codes to uppercase.
Population$Country <- toupper(Population$Country)

Population$Destination <- NULL
Population <- plyr::rename(Population,c("Value"="Inhabitants"))


# ----------------------------------------------------------
# htbl_Key_Weight: Read average weights per UNU_Key
# ----------------------------------------------------------
htbl_Key_Weight <- read.csv("htbl_Key_Weight.csv", quote = "\"",
                            colClasses = c("character", "numeric", "character"))




# ----------------------------------------------------------------------------------
#                                        POM
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------
# tbl_POM: Read POM data
# ----------------------------------------------------------
tbl_POM <- read.csv("tbl_POM.csv", quote = "\"",
                    colClasses = c("numeric", "character", "character", "character", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric"))



# ----------------------------------------------------------
# tbl_POM: Aggregate the data and append it to dataset
# ----------------------------------------------------------

# Set inputfile for calculations
mydf <- tbl_POM
mydf <- plyr::rename(mydf,c("POM_t"="var"))

source(file.path(SCRIPT_PATH, "05a_calculate_aggregates.R"))

mydf_all <- plyr::rename(mydf_all,c("var"="POM_t"))

# Remove variable Inhabitants...
mydf_all$Inhabitants <- NULL

# ... and now attach it again from population so it is also available for the aggregates.
mydf_all <- merge(mydf_all, Population,  by=c("Country", "Year"),  all.x = TRUE)

# Attach description of UNU_Keys
mydf_all <- merge(mydf_all, htbl_Key_Description,  by="UNU_Key",  all.x = TRUE)
mydf_all <- plyr::rename(mydf_all,c("Description"="UNU_Key_Description"))

# Copy result to tbl_POM_all

# Order of rows:
sortorder <- order( mydf_all$UNU_Key, mydf_all$Country, -rank(mydf_all$Year) )

# Order of columns: 
sortorder_c <- c("UNU_Key", "UNU_Key_Description", "Stratum", "Country", "Year", "Inhabitants", "kpi", "ppi",
                 "POM_t", "POM_pieces", "flag")

tbl_POM_all <- mydf_all[sortorder, sortorder_c]

# Calculate POM_t again so it is available also for the aggregates
tbl_POM_all$POM_t = tbl_POM_all$kpi * tbl_POM_all$Inhabitants / 1000





# ----------------------------------------------------------------------------------
#                                        WEEE
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------
# tbl_WEEE: Add calculated WEEE data
# ----------------------------------------------------------
tbl_WEEE <- read.csv("tbl_WEEE.csv", quote = "\"",
                     colClasses = c("character", "character", "character", "numeric"))




# ----------------------------------------------------------
# Stratum: Read table with country stratums
# ----------------------------------------------------------
Stratum <- read.csv("tbl_Countries.csv",quote = "\"",
                    colClasses = c("character", "NULL", "numeric", "NULL"))

# Convert country codes to uppercase.
Stratum$Country <- toupper(Stratum$Country)



# ----------------------------------------------------------
# tbl_WEEE: add stratum and population data and aggregate to collection categories
# ----------------------------------------------------------

# Add stratum data
tbl_WEEE <- merge(tbl_WEEE, Stratum,  by="Country",  all.x = TRUE)
rm(Stratum)

# Add population data
tbl_WEEE <- merge(tbl_WEEE, Population,  by=c("Country", "Year"),  all.x = TRUE)

# Calculate kpi for WEEE
tbl_WEEE$kpi <- tbl_WEEE$WEEE_t / tbl_WEEE$Inhabitants * 1000



# ----------------------------------------------------------
# tbl_WEEE: Aggregate the data and append it to dataset
# ----------------------------------------------------------

# Set inputfile for calculations
mydf <- tbl_WEEE
mydf <- plyr::rename(mydf,c("WEEE_t"="var"))

source(file.path(SCRIPT_PATH, "05a_calculate_aggregates.R"))

mydf_all <- plyr::rename(mydf_all,c("var"="WEEE_t"))


# merge Average weight with mydf_all
mydf_all <- merge(mydf_all, htbl_Key_Weight,
                  by=c("UNU_Key", "Year"), all.x = TRUE)

# Estimation of stock in pieces per inhabitant. Based on average weight of year for which stock is calculated.
# For products that get lighter every year this will lead to an over estimation of the number of pieces.
mydf_all$ppi <- mydf_all$WEEE_t * 1000 /
  mydf_all$AverageWeight / mydf_all$Inhabitants

mydf_all$WEEE_pieces <- mydf_all$ppi * mydf_all$Inhabitants

mydf_all$flag <- NA

# Remove variable Inhabitants...
mydf_all$Inhabitants <- NULL

# ... and now attach it again from population so it is also available for the aggregates.
mydf_all <- merge(mydf_all, Population,  by=c("Country", "Year"),  all.x = TRUE)

# Attach description of UNU_Keys
mydf_all <- merge(mydf_all, htbl_Key_Description,  by="UNU_Key",  all.x = TRUE)
mydf_all <- plyr::rename(mydf_all,c("Description"="UNU_Key_Description"))

# Copy result to tbl_WEEE_all

# Order of rows:
sortorder <- order( mydf_all$UNU_Key, mydf_all$Country, -rank(mydf_all$Year) )

# Order of columns: 
sortorder_c <- c("UNU_Key", "UNU_Key_Description", "Stratum", "Country", "Year", "Inhabitants", "kpi", "ppi",
                 "WEEE_t", "WEEE_pieces", "flag")

tbl_WEEE_all <- mydf_all[sortorder, sortorder_c]

# Calculate WEEE_t again so it is available also for the aggregates
tbl_WEEE_all$WEEE_t = tbl_WEEE_all$kpi * tbl_WEEE_all$Inhabitants / 1000





# ----------------------------------------------------------------------------------
#                                       STOCK OF EEE
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------
# tbl_stock: Calculate stock data (historic POM - historic WEEE)
# ----------------------------------------------------------

# Merge tbl_POM and tbl_WEEE so the same number of years are in the calculations.
# We use those versions because the contain the total weight and not per inhabitants.

tbl_stock <- merge(tbl_POM[, c(1:5, 7)], tbl_WEEE[, 1:5],
                   by=c("UNU_Key", "Stratum" , "Country", "Year"), all.x = TRUE)


# Calculate cumulative sums per group
require(data.table)
tbl_stock <- data.table(tbl_stock)
tbl_stock[, kpi_POM_cumsum := cumsum(POM_t), by=list(UNU_Key, Stratum, Country)]
tbl_stock[, kpi_WEEE_cumsum := cumsum(WEEE_t), by=list(UNU_Key, Stratum, Country)]

tbl_stock$stock_t <- tbl_stock$kpi_POM_cumsum - tbl_stock$kpi_WEEE_cumsum
tbl_stock <- as.data.frame(tbl_stock)

# Calculate kpi for the stock (this is used for calculating stock_t for the aggregates)
tbl_stock$kpi <- tbl_stock$stock_t / tbl_stock$Inhabitants * 1000


# ----------------------------------------------------------
# tbl_stock: Aggregate the data and append it to dataset
# ----------------------------------------------------------

# The following procedure calculates the kpi (kilo's per inhabitant) of the stock.
# From there we can recalculate the totals.
# First remove columns we don't need anymore.

tbl_stock$POM_t <- NULL
tbl_stock$WEEE_t <- NULL
tbl_stock$kpi_POM_cumsum <- NULL
tbl_stock$kpi_WEEE_cumsum <- NULL


# Set inputfile for calculations
mydf <- tbl_stock
mydf <- plyr::rename(mydf,c("stock_t"="var"))

source(file.path(SCRIPT_PATH, "05a_calculate_aggregates.R"))

# Copy result to tbl_Stock_all
tbl_Stock_all <- mydf_all[, -(8:10)]
tbl_Stock_all <- plyr::rename(tbl_Stock_all,c("var"="stock_t"))


# Remove variable Inhabitants...
tbl_Stock_all$Inhabitants <- NULL

# ... and now attach it again from population so it is also available for the aggregates.
tbl_Stock_all <- merge(tbl_Stock_all, Population,  by=c("Country", "Year"),  all.x = TRUE)

tbl_Stock_all <- merge(tbl_Stock_all, htbl_Key_Description,  by="UNU_Key",  all.x = TRUE)
tbl_Stock_all <- plyr::rename(tbl_Stock_all,c("Description"="UNU_Key_Description"))

# Calculate stock_t were missing.
tbl_Stock_all[is.na(tbl_Stock_all$stock_t), "stock_t"] <- tbl_Stock_all[is.na(tbl_Stock_all$stock_t), "kpi"] *
  tbl_Stock_all[is.na(tbl_Stock_all$stock_t), "Inhabitants"]


# merge Average weight with tbl_Stock
tbl_Stock_all <- merge(tbl_Stock_all, htbl_Key_Weight,
                       by=c("UNU_Key", "Year"), all.x = TRUE)


# Estimation of stock in pieces per inhabitant. Based on average weight of year for which stock is calculated.
# For products that get lighter every year this will lead to an over estimation of the number of pieces.
tbl_Stock_all$ppi <- tbl_Stock_all$stock_t * 1000 /
  tbl_Stock_all$AverageWeight / tbl_Stock_all$Inhabitants

tbl_Stock_all$stock_pieces <- tbl_Stock_all$ppi * tbl_Stock_all$Inhabitants

# Order of rows:
sortorder <- order( mydf_all$UNU_Key, mydf_all$Country, -rank(mydf_all$Year) )

tbl_Stock_all$flag <- NA

# Order of columns: 
sortorder_c <- c("UNU_Key", "UNU_Key_Description", "Stratum", "Country", "Year", "Inhabitants", "kpi", "ppi",
                 "stock_t", "stock_pieces", "flag")

tbl_Stock_all <- tbl_Stock_all[sortorder, sortorder_c]


# Clean-up
rm(htbl_Key_Weight)
rm(Population)
rm(htbl_Key_Aggregates)




# ----------------------------------------------------------------------------------
#                                       REFERENTIAL DATA 
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------
# tbl_Eurostat_Data.csv: Add Eurostat reference data for POM
# ----------------------------------------------------------
eurostatdata <- read.csv("tbl_Eurostat_Data.csv", quote = "\"",
                         colClasses = c("character", "character", "character",
                                        "numeric", "character", "character"))

eurostatdata$Description <- NULL
eurostatdata$Parameter <- NULL

eurostatdata <- plyr::rename(eurostatdata,c("Value"="kpi"))






# ----------------------------------------------------------------------------------
#  tbl_ANALYSIS: Save
# ----------------------------------------------------------------------------------

# Put all the data in single list
tbl_ANALYSIS <- list(tbl_POM_all, tbl_WEEE_all, tbl_Stock_all, eurostatdata)

# Save data
save(tbl_ANALYSIS, file="tbl_ANALYSIS.RData")

# Also save POM, WEEE and Stock data including the aggregates as CSV (European style).
write.csv2(tbl_POM_all, file = "tbl_POM_all.csv", quote = TRUE, row.names = FALSE)
write.csv2(tbl_WEEE_all, file = "tbl_WEEE_all.csv", quote = TRUE, row.names = FALSE)
write.csv2(tbl_Stock_all, file = "tbl_Stock_all.csv", quote = TRUE, row.names = FALSE)



