setwd(DATA_PATH)
options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)

# ----------------------------------------------------------
#  PVpanels: Collect Eurostat PV panel data (UNU_Key 0002)
# ----------------------------------------------------------

# At Eurostat there is data available about the Net maximum capacity of Solar Photovoltaic in Megawatt.
# It can be found on the Eurostat website http://ec.europa.eu/eurostat/data/database
# Search for [nrg_113a]
# Or look in the navigation tree under:
# Environment and energy -> Engergy (nrg) -> Energy statistics - Infrastructure (nrg_11) ->
# Infrastructure - electricity - annual data [nrg_113a]
# Look for the indicator "Net maximum capacity - Solar Photovoltaic".

# This script will download the data automatically with Open Data.
# After that some conversion will be done based on values for number of panels and kg per megawatt.

# URL generated with query builder:
# http://ec.europa.eu/eurostat/web/json-and-unicode-web-services/getting-started/query-builder

require(eurostat)
# url <- "http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/nrg_113a?precision=1&product=9007&unit=MW&indic_nrg=12_1176233&unitLabel=code"
id <-"nrg_113a"
PVpanels <- get_eurostat(id, type="code")

# select "Net maximum capacity - Solar Photovoltaic"
selection <- which (PVpanels$indic_nrg=="12_1176233")

# Data is all "Net maximum capacity - Solar Photovoltaic" in "Megawatt"
# Now we don't need all that information in the dataset
selection_c <- c("geo", "time", "values")

PVpanels <- PVpanels[selection, selection_c]
PVpanels <- plyr::rename(PVpanels,c("values"="max_capacity", "geo"="CountryCode2", "time"="Year"))

# Convert country codes
Stratum <- read.csv("tbl_Countries.csv",quote = "\"",
                    colClasses = c("character", "NULL", "NULL", "character"))

# Convert country codes to uppercase.
Stratum$Country <- toupper(Stratum$Country)

# Convert 2 letter country codes to 3 letter country codes
PVpanels <- merge (PVpanels, Stratum, by="CountryCode2")
PVpanels$CountryCode2 <- NULL
rm(Stratum)

# Change time into 4 digits
PVpanels$Year <- substr(PVpanels$Year, 1, 4)

sortorder <- order(PVpanels$Country, PVpanels$Year)
PVpanels <- PVpanels[sortorder, ]

# Read table with conversion factors from MegaWatt to kg and from kg to number of units.
PVconversion <- read.csv("tbl_solar_panel_conversion_factors.csv",quote = "\"",
                    colClasses = c("character", "numeric", "numeric"))

PVpanels <- merge (PVpanels, PVconversion, by="Year")


# Calculate number of kg installed capacity
PVpanels$stock_kg <- PVpanels$max_capacity * PVpanels$kg_per_Mw
PVpanels$stock_units <- PVpanels$stock_kg / PVpanels$kg_per_panel

PVpanels$max_capacity <- NULL
PVpanels$kg_per_Mw <- NULL
PVpanels$kg_per_panel <- NULL

# This data now contains information about the stock of weight and number of units of PV panels.
# Not about what has been sold in the given year.
# This can be calculated by substracting from every year the value of the previous year.

# Data is available from 1990 onwards. To calculate the change per year we add year 1989
# with installed capacity values all 0.
selection <- which (PVpanels$Year == "1990")
df1989 <- PVpanels[selection, ]
df1989$Year <- "1989"
df1989$stock_kg <- 0
df1989$stock_units <- 0
PVpanels <- rbind.fill(PVpanels, df1989)
rm (df1989)

# Sort dataframe rows by Country (A) and Year (D)
sortorder <- order(PVpanels$Country, -rank(PVpanels$Year))
PVpanels <- PVpanels[sortorder, ]

# Calculate change per year
rownumber <- 1:nrow(PVpanels)
selection <- which(PVpanels[rownumber, "Country"] == PVpanels[rownumber + 1, "Country"])

PVpanels[selection, "kg"] <- PVpanels[selection, "stock_kg"] - PVpanels[selection + 1, "stock_kg"]
PVpanels[selection, "units"] <- PVpanels[selection, "stock_units"] - PVpanels[selection + 1, "stock_units"]

rm(rownumber)

# Now Year 1989 can be removed again.
selection <- which (PVpanels$Year == "1989")
PVpanels <- PVpanels[-selection, ]


# Remove negative sales.
selection <- which(PVpanels$kg < 0)
PVpanels[selection, "kg"] <- 0
PVpanels[selection, "units"] <- 0

PVpanels$stock_kg <- NULL
PVpanels$stock_units <- NULL

# Add the extrapolation data from IRENA.ORG.
PVextrapol <- read.csv("tbl_solar_panel_extrapolation_data.csv",quote = "\"",
                       colClasses = c("character", "character", "numeric", "NULL"))

PVpanels <- rbind.fill(PVpanels, PVextrapol)

# Add the conversion values again for the extrapolation years.
PVpanels <- merge (PVpanels, PVconversion, by="Year")

# Take over kg data
sortorder <- order(PVpanels$Country, PVpanels$Year)
PVpanels <- PVpanels[sortorder, ]

selection <- which (!is.na(PVpanels$POM_t))
PVpanels[selection, "kg"] <- PVpanels[selection, "POM_t"] * 1000 
PVpanels[selection, "units"] <- PVpanels[selection, "kg"] / PVpanels[selection, "kg_per_panel"]

PVpanels$POM_t <- NULL
PVpanels$kg_per_Mw <- NULL
PVpanels$kg_per_panel <- NULL

PVpanels$UNU_Key <- "0002"

sortorder_c <- c("UNU_Key", "Country", "Year", "kg", "units")

write.csv(PVpanels[, sortorder_c], file = "solar_panel_data.csv",
           quote = TRUE, row.names = FALSE)

rm(PVconversion)