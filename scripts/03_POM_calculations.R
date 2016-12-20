# -----------------------------------------------------------------------------------------------------------
#
#   Name:           03POM_calculations.R
#
#   Description:    This script read the CSV file with the calculated POM data.
#                   Then it will remove outliers and make estimates for missing data.
#
#
#   Author:         V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------


setwd(DATA_PATH)
options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)

# ----------------------------------------------------------
# UNU_countries: Read raw version of POM data
# ----------------------------------------------------------
UNU_countries <- read.csv("UNU_countries.csv",
                          colClasses = c("character", "character", "character",
                                         "numeric", "numeric"))

# Make copy so at the end of the calculations, we can see the impact of the alterations.
original_UNU_countries <- UNU_countries

# ----------------------------------------------------------
# Stratum: Read table with country stratums
# ----------------------------------------------------------
Stratum <- read.csv("tbl_Countries.csv",quote = "\"",
                    colClasses = c("character", "NULL", "numeric", "NULL"))

# Convert country codes to uppercase.
Stratum$Country <- toupper(Stratum$Country)


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
Population <- rename(Population,c("Value"="Inhabitants"))


# ----------------------------------------------------------
# htbl_Key_Weight: Read table with average weights per UNU_Key
# ----------------------------------------------------------
htbl_Key_Weight <- read.csv("htbl_Key_Weight.csv", quote = "\"",
                         colClasses = c("character", "numeric", "character"))


# ----------------------------------------------------------
# PurPow: Read table with purchasing power per country
# ----------------------------------------------------------
PurPow <- read.csv("tbl_Data.csv", quote = "\"",
                   colClasses = c("character", "numeric", "numeric", "character"))

# Only keep data about number of inhabitants
PurPow <- PurPow[PurPow$Destination == "ppp ph17", ]

# Convert country codes to uppercase.
PurPow$Country <- toupper(PurPow$Country)

PurPow$Destination <- NULL
PurPow <- rename(PurPow,c("Value"="PPP"))





# ----------------------------------------------------------
#   **** Perform the calculations ***
# ----------------------------------------------------------

# ----------------------------------------------------------
# UNU_countries - Preparations
# ----------------------------------------------------------

# Make records for each combination
myUNU_Keys <- as.data.frame(table(UNU_countries$UNU_Key))
names(myUNU_Keys)[1] <- "UNU_Key"
myUNU_Keys[2] <- NULL

myYears <- as.data.frame(table(UNU_countries$Year))
names(myYears)[1] <- "Year"
myYears[2] <- NULL

myCountries <- as.data.frame(table(UNU_countries$Country))
names(myCountries)[1] <- "Country"
myCountries[2] <- NULL

UNU_Key_Year <- merge(myUNU_Keys, myYears,  all = TRUE)
UNU_Key_Year_Country <- merge(UNU_Key_Year, myCountries,  all = TRUE)

UNU_countries <- merge(UNU_countries, UNU_Key_Year_Country,  by=c("UNU_Key", "Year", "Country"),  all = TRUE)


# Create flag variable and give value of 0 for original data and 1 for missing data.
UNU_countries$flag <- ifelse(is.na(UNU_countries$POM_kg) & is.na(UNU_countries$POM_pieces),1,0)


rm(myUNU_Keys, myYears, myCountries, UNU_Key_Year, UNU_Key_Year_Country)


# ----------------------------------------------------------
# UNU_countries: Perform some manual corrections
# ----------------------------------------------------------
# This file contains manual corrections and a few values that are not corrections, but including them here
# makes sure that that data will not be considered an outlier later in the process.
manualdata <- read.csv("manual_corrections.csv", quote = "\"",
                       colClasses = c("character", "character", "character", "numeric", "numeric",
                                      "NULL", "NULL", "NULL"))

# attach manual data to original data
UNU_countries <- merge(UNU_countries, manualdata,  by=c("UNU_Key", "Year", "Country"),  all.x = TRUE)

# overwrite original data with the manual data
selection <- !is.na(UNU_countries$POM_kg_new)
UNU_countries[selection, "POM_kg"] <- UNU_countries[selection, "POM_kg_new"]
UNU_countries[selection, "POM_pieces"] <- UNU_countries[selection, "POM_pieces_new"]
UNU_countries[selection, "flag"] <- 60

# Clean-up
UNU_countries$POM_kg_new <- NULL
UNU_countries$POM_pieces_new <-NULL
rm(manualdata)


# ----------------------------------------------------------
# UNU_countries: Add variables needed in the calculations
# ----------------------------------------------------------
# Attach number of inhabitants for each country
UNU_countries <- merge(UNU_countries, Population,  by=c("Country", "Year"),  all.x = TRUE)
UNU_countries <- merge(UNU_countries, Stratum,  by="Country",  all.x = TRUE)

rm(Stratum) # Population is needed later for the estimation of past and future values

# Calculate kg and pieces per inhabitant
UNU_countries$kpi = UNU_countries$POM_kg / UNU_countries$Inhabitants
UNU_countries$ppi = UNU_countries$POM_pieces / UNU_countries$Inhabitants

# For now the total POM_kg and POM_pieces are removed. They will be calculated again later in the script.
UNU_countries$POM_kg <- NULL
UNU_countries$POM_pieces <- NULL


# ----------------------------------------------------------
# UNU_countries: Some corrections on the original data based on knowledge of the market
# ----------------------------------------------------------
# CRT televisions have not been sold in recent years.
# For stratum 1 countries they are set to zero starting in 2007.
# For stratum 2 countries they are set to zero starting in 2008.
# For stratum 3 countries they are set to zero starting in 2009.

selection <- which (UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 1 & UNU_countries$Year >= 2007)
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0
UNU_countries[selection, "flag"] <- 52
selection <- which(UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 2 & UNU_countries$Year >= 2008)
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0
UNU_countries[selection, "flag"] <- 52
selection <- which(UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 3 & UNU_countries$Year >= 2009)
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0
UNU_countries[selection, "flag"] <- 52

# To make the transition more smoothly the previous year gets half the value of the year before that one.
# First sort dataframe rows by UNU_Key, Country and Year.
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]

selection <- which(UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 1 & UNU_countries$Year == 2006)
UNU_countries[selection, "kpi"] <- UNU_countries[(selection -1), "kpi"] / 2
UNU_countries[selection, "ppi"] <- UNU_countries[(selection -1), "ppi"] / 2
UNU_countries[selection, "flag"] <- 52
selection <- which(UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 2 & UNU_countries$Year == 2007)
UNU_countries[selection, "kpi"] <- UNU_countries[(selection -1), "kpi"] / 2
UNU_countries[selection, "ppi"] <- UNU_countries[(selection -1), "ppi"] / 2
UNU_countries[selection, "flag"] <- 52
selection <- which(UNU_countries$UNU_Key == "0407" & UNU_countries$Stratum == 3 & UNU_countries$Year == 2008)
UNU_countries[selection, "kpi"] <- UNU_countries[(selection -1), "kpi"] / 2
UNU_countries[selection, "ppi"] <- UNU_countries[(selection -1), "ppi"] / 2
UNU_countries[selection, "flag"] <- 52


# There are too many unrealistic data points in UNU_Key 0802. For now values of over 20 kg per inhabitant ar removed.
# Otherwise only the largest outliers get removed, but much smaller - however still very big - outliers remain.
# More research is needed to see if average weight per prodcom code could be improved.
selection <- which(UNU_countries$UNU_Key == "0802" & UNU_countries$kpi > 20 )
UNU_countries[selection, "kpi"] <- NA
UNU_countries[selection, "ppi"] <- NA




# ----------------------------------------------------------
# UNU_countries: Search for extremes in Year per Country and UNU_Key and remove them
# ----------------------------------------------------------

# threshold for how many times the mad a value can be away from the median to be considered an outlier
distance_mad <- 4

source(file.path(SCRIPT_PATH, "03a_remove_year_outliers.R"))


# ----------------------------------------------------------
# UNU_countries: Search for extremes in Country per Stratum, UNU_Key and Year and remove them
# ----------------------------------------------------------

if ( PROCESS_ALL_COUNTRIES ) {
   source(file.path(SCRIPT_PATH, "03b_remove_stratum_outliers.R"))
}

# ----------------------------------------------------------
# UNU_countries: Estimate missing values based on averages of surrounding years
# ----------------------------------------------------------

# Set inputfile for calculations. This way we can use same calculations later for missing stratum means.
mydf <- UNU_countries
mydf <- rename(mydf,c("UNU_Key"="catA", "Country"="catB"))

# Perform calculations and write in mydf.
source(file.path(SCRIPT_PATH, "03c_estimate_missings_using_years.R"))

# Set back to original names.
mydf <- rename(mydf,c("catA"="UNU_Key", "catB"="Country"))
UNU_countries <- mydf
rm(mydf)


# ----------------------------------------------------------
# UNU_countries: Calulate stratum means for estimation missing values based on stratum means
# ----------------------------------------------------------

# Calculate stratum means and write into strat_tot
if ( PROCESS_ALL_COUNTRIES ) {
  source(file.path(SCRIPT_PATH, "03d_calculate_stratum_means.R"))
}

# ----------------------------------------------------------
# strat_tot: Estimate missing stratum values based on surrounding years
# ----------------------------------------------------------

# Perform calculations and write in mydf
if ( PROCESS_ALL_COUNTRIES ) {
  # Set inputfile for calculations
  mydf <- strat_tot
  mydf <- rename(mydf,c("UNU_Key"="catA", "Stratum"="catB", "kpi_stratum"="kpi", "ppi_stratum"="ppi"))
  
  source(file.path(SCRIPT_PATH, "03c_estimate_missings_using_years.R"))
  
  # Copy data to UNU_countries
  strat_tot <- rename(mydf,c("catA"="UNU_Key", "catB"="Stratum", "kpi"="kpi_stratum", "ppi"="ppi_stratum"))
  # flag is not needed here
  strat_tot$flag <- NULL
  
  # Reshape stratum values to wide form.
  # For the following calculations we need for every record all stratum values available.
  # Estimation of missing values will be with the purchasing power in such a way that 
  # the estimated value will be between the 2 stratum means of 2 adjacent stratums.
  
  # KPI stratum
  strat_tot_wide_kpi <- dcast(strat_tot, UNU_Key + Year ~ Stratum, value.var = "kpi_stratum")
  names(strat_tot_wide_kpi)[3:ncol(strat_tot_wide_kpi)] <- 
    paste("kpi_s", names(strat_tot_wide_kpi)[3:ncol(strat_tot_wide_kpi)], sep = "")
  
  # PPI stratum
  strat_tot_wide_ppi <- dcast(strat_tot, UNU_Key + Year ~ Stratum, value.var = "ppi_stratum")
  names(strat_tot_wide_ppi)[3:ncol(strat_tot_wide_ppi)] <- 
    paste("ppi_s", names(strat_tot_wide_ppi)[3:ncol(strat_tot_wide_ppi)], sep = "")
  
  # merge together
  strat_tot_wide <- merge(strat_tot_wide_kpi, strat_tot_wide_ppi,
                          by=c("UNU_Key", "Year"), all.x = TRUE)
  
  # Merge stratum means with UNU_countries data
  UNU_countries <- merge(UNU_countries, strat_tot_wide,
                         by=c("UNU_Key", "Year"), all.x = TRUE)
  
  rm(mydf, strat_tot, strat_tot_wide_kpi, strat_tot_wide_ppi, strat_tot_wide)
}


# ----------------------------------------------------------
# UNU_countries: Estimate missing values based on stratum ratio's
# ----------------------------------------------------------

if ( PROCESS_ALL_COUNTRIES ) {
  source(file.path(SCRIPT_PATH, "03e_estimate_missings_using_stratums.R"))
}

# ----------------------------------------------------------
# UNU_countries: Eliminate big jumps from one year to the next
# ----------------------------------------------------------

# Make copy of dataset before elimination of big jumps.
# In case data is removed which cannot afterwards be estimated with surrounding years, we can put the
# original value back.
UNU_countries_before_smooth <- UNU_countries[ , c("UNU_Key", "Year", "Country", "kpi", "ppi")]
UNU_countries_before_smooth <- rename(UNU_countries_before_smooth,c("kpi"="kpi_before_smooth",
                                                                    "ppi"="ppi_before_smooth"))


source(file.path(SCRIPT_PATH, "03f_smooth_years.R"))


# ----------------------------------------------------------
# UNU_countries: For second time estimate missing values based on averages of surrounding years
# ----------------------------------------------------------
# This syntax will be run a second time because of the removal of big jumps from year to year in the previous step.

# Set inputfile for calculations. This way we can use same calculations later for missing stratum means.
mydf <- UNU_countries
mydf <- rename(mydf,c("UNU_Key"="catA", "Country"="catB"))

# Perform calculations and write in mydf.
source(file.path(SCRIPT_PATH, "03c_estimate_missings_using_years.R"))
# Set back to original names.
mydf <- rename(mydf,c("catA"="UNU_Key", "catB"="Country"))
UNU_countries <- mydf
rm(mydf)

# Now check if data has been removed in big jumps removal procedure, which was not estimated afterwards.
# If so, put the removed values back
UNU_countries <- merge(UNU_countries, UNU_countries_before_smooth,  all.x = TRUE)
selection <- is.na(UNU_countries$kpi) & !is.na(UNU_countries$kpi_before_smooth)
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_before_smooth"]
UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_before_smooth"]
UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] - 100

UNU_countries$kpi_before_smooth <- NULL
UNU_countries$ppi_before_smooth <- NULL
rm(UNU_countries_before_smooth)


# ----------------------------------------------------------
# UNU_countries: Change values in UNU_Key 0308 (CRT monitors)
# ----------------------------------------------------------

source(file.path(SCRIPT_PATH, "03g_CRT_monitors.R"))


# ----------------------------------------------------------
# UNU_countries: Add Eurostat PV panel data (UNU_Key 0002)
# ----------------------------------------------------------

source(file.path(SCRIPT_PATH, "03h_add_PV_panel_data.R"))


# ----------------------------------------------------------
# UNU_countries: Extend time series
# ----------------------------------------------------------

source(file.path(SCRIPT_PATH, "03i_extend_time_series.R"))


# ----------------------------------------------------------
# UNU_countries: Look for effect of changes compared with the original source data
# ----------------------------------------------------------

source(file.path(SCRIPT_PATH, "03j_changes_compared_to_original_data.R"))


# ----------------------------------------------------------
# UNU_countries: LED lamps didn't exist before 2007
# ----------------------------------------------------------

selection <- which (UNU_countries$UNU_Key == "0505" & UNU_countries$Year <= 2006)
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0
UNU_countries[selection, "flag"] <- 52

# To make the transition more smoothly the previous year gets half the value of the year before that one.
# First sort dataframe rows by UNU_Key, Country and Year.
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]

selection <- which(UNU_countries$UNU_Key == "0505" & UNU_countries$Year == 2007)
UNU_countries[selection, "kpi"] <- UNU_countries[(selection +1), "kpi"] / 2
UNU_countries[selection, "ppi"] <- UNU_countries[(selection +1), "ppi"] / 2
UNU_countries[selection, "flag"] <- 52



# ----------------------------------------------------------
# UNU_countries: Last alterations to datafile
# ----------------------------------------------------------

# Calculate total weight (kg) and number of pieces out of the kilo's and units per inhabitant
# They have to be recalculated after the last corrections in the kpi and ppi.
UNU_countries$POM_kg = UNU_countries$kpi * UNU_countries$Inhabitants
UNU_countries$POM_pieces = UNU_countries$ppi * UNU_countries$Inhabitants

# Convert kg to tonnes.
UNU_countries$POM_t = UNU_countries$POM_kg / 1000
UNU_countries$POM_kg <- NULL

# Sort order for columns
sortorder_c <- c("Stratum", "Country", "UNU_Key", "Year", "POM_t", "POM_pieces",
                 "Inhabitants", "kpi",  "ppi", "flag")

# Sort dataframe rows by Country, UNU_Key and Year
sortorder <- order(UNU_countries$Country, UNU_countries$UNU_Key, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, sortorder_c]

# Save data into tbl_POM.csv
write.csv(UNU_countries, file = "tbl_POM.csv", quote = TRUE, row.names = FALSE)
