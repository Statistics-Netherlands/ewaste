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
mydf <- plyr::rename(mydf,c("POM_t"="var_t"))
mydf <- plyr::rename(mydf,c("POM_pieces"="var_p"))

source(file.path(SCRIPT_PATH, "05a_calculate_aggregates.R"))

mydf_all <- plyr::rename(mydf_all,c("var_t"="POM_t"))
mydf_all <- plyr::rename(mydf_all,c("var_p"="POM_pieces"))

# Calculate kpi and ppi
selection <- which( is.na(mydf_all$kpi) )
mydf_all[selection, "kpi"] <- mydf_all[selection, "POM_t"] / mydf_all[selection, "Inhabitants"] * 1000
mydf_all[selection, "ppi"] <- mydf_all[selection, "POM_pieces"] / mydf_all[selection, "Inhabitants"]

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






# ----------------------------------------------------------------------------------
#                                        WEEE and Stock
# ----------------------------------------------------------------------------------

# ----------------------------------------------------------
# tbl_WEEE: Add calculated WEEE data
# ----------------------------------------------------------
tbl_WEEE <- read.csv("tbl_WEEE.csv", quote = "\"",
                     colClasses = c("numeric", "character", "character", "character", "numeric",
                                    "numeric", "numeric", "numeric"))

# select only data for the same years as are available in tbl_POM.
selection <- which( as.numeric(tbl_WEEE$Year) <= as.numeric(max(tbl_POM$Year)) )
tbl_WEEE <- tbl_WEEE[selection, ]


# ----------------------------------------------------------
# tbl_WEEE: Aggregate the data and append it to dataset
# ----------------------------------------------------------

# Set inputfile for calculations
mydf <- tbl_WEEE
mydf <- plyr::rename(mydf,c("WEEE_t"="var_t"))
mydf <- plyr::rename(mydf,c("WEEE_pieces"="var_p"))

source(file.path(SCRIPT_PATH, "05a_calculate_aggregates.R"))

mydf_all <- plyr::rename(mydf_all,c("var_t"="WEEE_t"))
mydf_all <- plyr::rename(mydf_all,c("var_p"="WEEE_pieces"))

tbl_WEEE_all <- mydf_all

# Calculate the stock (historic POM - historic WEEE) in weight and units.
tbl_stock <- merge(tbl_POM_all[, c(1:6, 9, 10)], tbl_WEEE_all[, 1:6],
                   by=c("UNU_Key", "Stratum" , "Country", "Year"), all.x = TRUE)

# Calculate cumulative sums per group
require(data.table)
tbl_stock <- data.table(tbl_stock)
tbl_stock[, POM_t_cumsum := cumsum(POM_t), by=list(UNU_Key, Stratum, Country)]
tbl_stock[, POM_pieces_cumsum := cumsum(POM_pieces), by=list(UNU_Key, Stratum, Country)]
tbl_stock[, WEEE_t_cumsum := cumsum(WEEE_t), by=list(UNU_Key, Stratum, Country)]
tbl_stock[, WEEE_pieces_cumsum := cumsum(WEEE_pieces), by=list(UNU_Key, Stratum, Country)]

tbl_stock$stock_t <- tbl_stock$POM_t_cumsum - tbl_stock$WEEE_t_cumsum
tbl_stock$stock_pieces <- tbl_stock$POM_pieces_cumsum - tbl_stock$WEEE_pieces_cumsum
tbl_stock <- as.data.frame(tbl_stock)

# Stock lower than zero cannot exist.
# The value of the ones lower than zero are added to the WEEE.
# This will only happen in far future for products that are not produced anymore for a long time.
selection <- which (tbl_stock$stock_t < 0 )
if (length(selection) > 0){
  tbl_stock[selection, "WEEE_t"] <- tbl_stock[selection, "WEEE_t"] - tbl_stock[selection, "stock_t"]
  tbl_stock[selection, "WEEE_pieces"] <- tbl_stock[selection, "WEEE_pieces"] - tbl_stock[selection, "stock_pieces"]
  tbl_stock[selection, "stock_t"] <- 0
  tbl_stock[selection, "stock_pieces"] <- 0
}

# Not needed anymore
tbl_stock$POM_t <- NULL
tbl_stock$POM_pieces <- NULL
tbl_stock$POM_t_cumsum <- NULL
tbl_stock$POM_pieces_cumsum <- NULL
tbl_stock$WEEE_t_cumsum <- NULL
tbl_stock$WEEE_pieces_cumsum <- NULL


### Create WEEE table
# Copy stock to WEEE table in cases WEEE had been changed for products that are long
# time not used anymore.
tbl_WEEE_all <- tbl_stock[1:8]
# Calculate kpi and ppi
tbl_WEEE_all$kpi <- tbl_WEEE_all$WEEE_t / tbl_WEEE_all$Inhabitants * 1000
tbl_WEEE_all$ppi <- tbl_WEEE_all$WEEE_pieces / tbl_WEEE_all$Inhabitants

tbl_WEEE_all$flag <- NA

# Order of rows:
sortorder <- order( tbl_WEEE_all$UNU_Key, tbl_WEEE_all$Country, -rank(tbl_WEEE_all$Year) )

# Order of columns: 
sortorder_c <- c("UNU_Key", "UNU_Key_Description", "Stratum", "Country", "Year", "Inhabitants", "kpi", "ppi",
                 "WEEE_t", "WEEE_pieces", "flag")

tbl_WEEE_all <- tbl_WEEE_all[sortorder, sortorder_c]


### Create Stock table
tbl_Stock_all <- tbl_stock[c(1:6, 9, 10)]
# Calculate kpi and ppi
tbl_Stock_all$kpi <- tbl_Stock_all$stock_t / tbl_Stock_all$Inhabitants * 1000
tbl_Stock_all$ppi <- tbl_Stock_all$stock_pieces / tbl_Stock_all$Inhabitants

tbl_Stock_all$flag <- NA

# Order of rows:
sortorder <- order( tbl_Stock_all$UNU_Key, tbl_Stock_all$Country, -rank(tbl_Stock_all$Year) )

# Order of columns: 
sortorder_c <- c("UNU_Key", "UNU_Key_Description", "Stratum", "Country", "Year", "Inhabitants", "kpi", "ppi",
                 "stock_t", "stock_pieces", "flag")

tbl_Stock_all <- tbl_Stock_all[sortorder, sortorder_c]

# Clean-up
rm(Population)
rm(htbl_Key_Aggregates)




# ----------------------------------------------------------------------------------
#                                       REFERENTIAL DATA 
# ----------------------------------------------------------------------------------

# At Eurostat there is data available about the Waste electrical and electronic equipment (WEEE)
# by waste operations [env_waselee]. We put this in the dataset as well as referential data.

require(eurostat)
id <-"env_waselee"
eurostatdata <- get_eurostat(id, type="code")

# select MKT "Products put on the market", unit Kg per inhabitant and variables needed
selection <- which (eurostatdata$wst_oper=="MKT" & eurostatdata$unit=="KG_HAB")
selection_c <- c("waste", "geo", "time", "values")
eurostatdata <- eurostatdata[selection, selection_c]

eurostatdata <- plyr::rename(eurostatdata,c("geo"="CountryCode2", "time"="Year", "values"="kpi"))


# Convert country codes
Stratum <- read.csv("tbl_Countries.csv",quote = "\"",
                    colClasses = c("character", "NULL", "NULL", "character"))

# Convert country codes to uppercase.
Stratum$Country <- toupper(Stratum$Country)

# Convert 2 letter country codes to 3 letter country codes
eurostatdata <- merge (eurostatdata, Stratum, by="CountryCode2")
eurostatdata$CountryCode2 <- NULL
rm(Stratum)

# Change time into 4 digits
eurostatdata$Year <- substr(eurostatdata$Year, 1, 4)

# Recode waste categories to UNU_Keys.
eurostatdata[eurostatdata$waste=="TOTAL", "UNU_Key"] <- "0"
eurostatdata[eurostatdata$waste=="EE_LHA", "UNU_Key"] <- "01"
eurostatdata[eurostatdata$waste=="EE_SHA", "UNU_Key"] <- "02"
eurostatdata[eurostatdata$waste=="EE_ITT", "UNU_Key"] <- "03"
eurostatdata[eurostatdata$waste=="EE_CPV", "UNU_Key"] <- "04"
eurostatdata[eurostatdata$waste=="EE_LIT", "UNU_Key"] <- "05"
eurostatdata[eurostatdata$waste=="EE_LIT_GDL", "UNU_Key"] <- "05"
eurostatdata[eurostatdata$waste=="EE_EET", "UNU_Key"] <- "06"
eurostatdata[eurostatdata$waste=="EE_TLS", "UNU_Key"] <- "07"
eurostatdata[eurostatdata$waste=="EE_MED", "UNU_Key"] <- "08"
eurostatdata[eurostatdata$waste=="EE_MON", "UNU_Key"] <- "09"
eurostatdata[eurostatdata$waste=="EE_ATD", "UNU_Key"] <- "10"

# Now waste can go, and aggregate per UNU_Key.
eurostatdata$waste <- NULL
eurostatdata <- ddply(eurostatdata, c("UNU_Key", "Country", "Year"), summarise,
                      kpi = sum(kpi, na.rm=TRUE) )





# ----------------------------------------------------------------------------------
#  tbl_ANALYSIS: Save
# ----------------------------------------------------------------------------------

# Put all the data in single list
tbl_ANALYSIS <- list(tbl_POM_all, tbl_WEEE_all, tbl_Stock_all, eurostatdata)

# Save data
save(tbl_ANALYSIS, file="tbl_ANALYSIS.RData")

# Also save POM, WEEE and Stock data including the aggregates as CSV (European style).
write.csv2(tbl_POM_all, file = "tbl_POM_all.csv", quote = TRUE, row.names = FALSE, na = "")
write.csv2(tbl_WEEE_all, file = "tbl_WEEE_all.csv", quote = TRUE, row.names = FALSE, na = "")
write.csv2(tbl_Stock_all, file = "tbl_Stock_all.csv", quote = TRUE, row.names = FALSE, na = "")



