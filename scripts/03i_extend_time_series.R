# ----------------------------------------------------------
#  UNU_countries: Extend time series back to 1980 and into the future for as far as
#  there are purchasing power estimations available.
# ----------------------------------------------------------


# ----------------------------------------------------------
# PurPow: Read table with purchasing power per country. Needed for estimating kpi based on purchasing power.
# ----------------------------------------------------------
PurPow <- read.csv("tbl_Data.csv", quote = "\"",
                   colClasses = c("character", "numeric", "numeric", "character"))

# Only keep data about the purchasing power parity
PurPow <- PurPow[PurPow$Destination == "ppp ph17", ]

# Convert country codes to uppercase.
PurPow$Country <- toupper(PurPow$Country)

PurPow$Destination <- NULL
PurPow <- plyr::rename(PurPow,c("Value"="PPP"))


# ----------------------------------------------------------
# UNU_countries: Prepare dataset for extended data
# ----------------------------------------------------------

# The first data in our dataset is from 1995. Now we make records for all years back to 1980 and 9 years into the future.

# Grab all records from one year (does not matter which one). Only first 4 columns.
selection <- which(UNU_countries$Year == "2010")
oneyear <- UNU_countries[selection, 1:4]

# Remove Year. Will be recreated.
oneyear$Year <- NULL

# Last year in original data
LatestYear <- as.integer(max(UNU_countries$Year,1))

# Change the year
years<- c(1980:1994, (LatestYear+1): (max(PurPow$Year)))

# Multiply Get data for every year.
allyears <- merge(oneyear, years, all = TRUE)

names(allyears)[ncol(allyears)] <- "Year"

# Add it to the dataset
UNU_countries <- rbind.fill(UNU_countries, allyears)

# Add PPP to dataset
UNU_countries <- merge(UNU_countries, PurPow, by=c("Country", "Year"), all.x = TRUE)

# Now fill the added years with data.



# ----------------------------------------------------------
# UNU_countries: Estimation data of future years
# ----------------------------------------------------------

# Standard approach:
# The relation between KPI (kg EEE per inhabitant) and PPP (Purchasing Power Parity) is used 
# to predict future sales. For PPP estimations of the next 10 years are available.

# Alternative approach:
# Ad indicated in the file 'tbl_Extrapolation_exceptions' different methods can be used for instance
# keeping the last available datapoint value as a constant or using a polynom trend instead.
# These exceptions are set on combination of UNU_Key and Country.

# Now we add these extrapolation exceptions to the dataset they can be used.

tbl_Extrapolation_exceptions <- read.csv("tbl_Extrapolation_exceptions.csv", quote = "\"",
                                         colClasses = c("character", "character","character", "numeric", "numeric"))

# In case of double records just use the first one so there won't be errors later in the script.
require(dplyr)
tbl_Extrapolation_exceptions <-tbl_Extrapolation_exceptions %>% group_by(UNU_Key, Country) %>% slice(unique(1))




# When an empty Country is provided it means that the method counts for all countries
# belonging to that UNU_Key unless a specific country is also added. First add all those
# general methods.

selection <- which( tbl_Extrapolation_exceptions$Country == "" )

# in case of empty vector don't do command
if (length(selection) > 0){
  UNU_Keys_Extrapolation_Method <- tbl_Extrapolation_exceptions[selection,]
  UNU_Keys_Extrapolation_Method$Country <- NULL
  UNU_Keys_Extrapolation_Method <- plyr::rename(UNU_Keys_Extrapolation_Method,
                                                c("ExtrapolationApproach"="ExtrapolationApproachUNU",
                                                  "ConnectionYear"="ConnectionYearUNU",
                                                  "YearsForTrend"="YearsForTrendUNU"))
  
  # and remove the same rows from the orginal
  tbl_Extrapolation_exceptions <- tbl_Extrapolation_exceptions[-selection,]
  
  # Merge data for all countries per UNU_Key.
  UNU_countries <- merge(UNU_countries, UNU_Keys_Extrapolation_Method, by="UNU_Key", all.x = TRUE)
  }

# Merge the values per UNU_Key and specific country.
UNU_countries <- merge(UNU_countries, tbl_Extrapolation_exceptions, by=c("UNU_Key", "Country"), all.x = TRUE)

# Now copy the values per UNU_Key and all countries when there is no country specific value.
if (length(selection) > 0){
  selection <- which (!is.na(UNU_countries$ExtrapolationApproachUNU) & is.na(UNU_countries$ExtrapolationApproach) )
  if (length(selection) > 0){
    UNU_countries[selection, "ExtrapolationApproach"] <- UNU_countries[selection, "ExtrapolationApproachUNU"]
    UNU_countries[selection, "ConnectionYear"] <- UNU_countries[selection, "ConnectionYearUNU"]
    UNU_countries[selection, "YearsForTrend"] <- UNU_countries[selection, "YearsForTrendUNU"]
    UNU_countries$ExtrapolationApproachUNU <- NULL
    UNU_countries$ConnectionYearUNU <- NULL
    UNU_countries$YearsForTrendUNU <- NULL
  }
}

# When left empty the default values are used for ExtrapolationApproach, ConnectionYear and YearsForTrend
# YearsForTrend only works for kpi trend at this moment.
selection <- which(is.na(UNU_countries$ExtrapolationApproach) | UNU_countries$ExtrapolationApproach == "")
UNU_countries[selection, "ExtrapolationApproach"] <- "KPI_PPP"
selection <- which(is.na(UNU_countries$ConnectionYear))
UNU_countries[selection, "ConnectionYear"] <- LatestYear 
selection <- which(is.na(UNU_countries$YearsForTrend))
UNU_countries[selection, "YearsForTrend"] <- 9 



# ----------------------------------------------------------
# UNU_countries: Extrapolations (1) with standard approach (KPI_PPP)
# ----------------------------------------------------------
# The standard KPI vs PPP approach is calculated first.
# First calculate the lineair regression line for all KPI vs PPP for each UNU_Key
# for all countries except LUX in the last 10 years with exception of the very last available year.
# Also only use values with flag = 0 or 60, which means no alterations have taken place or manual alterations
# have been done.
# That way we are not extrapolating on extrapolations.
# Also no empty kpi values should be used.
# Select the right data:
selection <- which ( UNU_countries$Country != "LUX" & as.integer(UNU_countries$Year) < LatestYear &
                       as.integer(UNU_countries$Year) >= LatestYear - 9 & UNU_countries$flag %in% c(0, 60) &
                       UNU_countries$kpi >= 0)
UNU_countries_selection <- UNU_countries[selection, ]

# Column not needed for calulations. Only needed after using the calculations in original UNU_countries file 
UNU_countries_selection$ExtrapolationApproach <- NULL

# Set an X before UNU_Key, so they will be read as character after the melt process.
UNU_countries_selection$XUNU_Key <- paste0("X", UNU_countries_selection$UNU_Key)
# Calculate the regression parameters for the trend over all countries.
models <- by(UNU_countries_selection, UNU_countries_selection$XUNU_Key, 
                 function(UNU_countries_selection) lm(UNU_countries_selection$kpi ~ UNU_countries_selection$PPP) )
models <- sapply(models, coefficients)

# Reshape
models <- melt(models)
models <- dcast(models, Var2 ~ Var1, value.var = "value")

# Now the X before UNU_Key can go again
models$Var2 <- substr(models$Var2, 2, 5)
UNU_countries_selection$XUNU_Key <- NULL

models <- plyr::rename(models,c("Var2"="UNU_Key", "(Intercept)"="intercept", "UNU_countries_selection$PPP"="slope"))


# --------------- Gather some information on the model to see if it can be used -----------------
# Calculate correlation coefficients to derterimine if the model can be used for estimations.
correlations <- by(UNU_countries_selection, UNU_countries_selection$UNU_Key, 
                   function(UNU_countries_selection)
                     cor(UNU_countries_selection$kpi, UNU_countries_selection$PPP, 
                         use="complete.obs", method="pearson") )

correlations <- data.frame(UNU_Key = dimnames(correlations)[[1]], corr = as.vector(correlations))

# Merge with model coefficients
kpi_ppp_model <- merge(models, correlations, by="UNU_Key")

# save coefficients for Jaco
write.csv2(kpi_ppp_model[, c("UNU_Key", "slope", "intercept", "corr")],
           file = "../kpi_ppp.csv", quote = TRUE, row.names = FALSE)
# --------------- End gather some information on the model to see if it can be used -----------------

# When correlation coefficient is smaller than 33% we will not use this approach but the LastReliableConstant instead.
# Approach is changed here.
UNU_countries <- merge(UNU_countries, correlations, by="UNU_Key", all.x = TRUE)
selection <- which ( abs(UNU_countries$corr) < 0.33 & UNU_countries$ExtrapolationApproach=="KPI_PPP")
UNU_countries[selection, "ExtrapolationApproach"] <- "LastReliableConstant"



# Calculate the regression parameters for the trend for each specific country so it can be used as a way to 
# connect above model to the specific country data.
# The subset is created again because now we need LUX and the ConnectionYear variable is taken into account
# so alternative years can be given for subsets of the data to connect real data to extrapolations.
selection <- which ( as.integer(UNU_countries$Year) < UNU_countries$ConnectionYear &
                       as.integer(UNU_countries$Year) >= LatestYear - 9 &
                       UNU_countries$kpi >= 0)
UNU_countries_selection <- UNU_countries[selection, ]

UNU_countries_selection$ExtrapolationApproach <- NULL
UNU_countries_selection$ConnectionYear <- NULL

# Set an X before UNU_Key, so they will be read as character after the melt process.
UNU_countries_selection$XUNU_Key <- paste0("X", UNU_countries_selection$UNU_Key)

# Make a variable for each UNU_Key and Country combination.
UNU_countries_selection$UNU_Key_Country <- paste(UNU_countries_selection$UNU_Key, UNU_countries_selection$Country, sep="_")

models_per_country <- by(UNU_countries_selection, UNU_countries_selection$UNU_Key_Country, 
                         function(UNU_countries_selection) lm(UNU_countries_selection$kpi ~ UNU_countries_selection$PPP) )
models_per_country <- sapply(models_per_country, coefficients)

# Reshape
models_per_country <- melt(models_per_country)
models_per_country <- dcast(models_per_country, Var2 ~ Var1, value.var = "value")

models_per_country <- plyr::rename(models_per_country,c("Var2"="UNU_Key_Country", "(Intercept)"="intercept", 
                                                        "UNU_countries_selection$PPP"="slope"))


# Add data from the last model per UNU_Key and Country to UNU_Countries
UNU_countries$UNU_Key_Country <- paste(UNU_countries$UNU_Key, UNU_countries$Country, sep="_")
UNU_countries <- merge(UNU_countries, models_per_country, by="UNU_Key_Country", all.x = TRUE)
UNU_countries$UNU_Key_Country <- NULL

# When there are no extrapolation parameters available, assign it to KPI method instead.
selection <- which( UNU_countries$ExtrapolationApproach == "KPI_PPP" & 
                      is.na(UNU_countries$slope) )
UNU_countries[selection, "ExtrapolationApproach"] <- "KPI"

# Now first extrapolation can be calculated with the added model parameters.
selection <- which( UNU_countries$ExtrapolationApproach == "KPI_PPP" & 
                      as.numeric(UNU_countries$Year) == (UNU_countries$ConnectionYear + 1) )
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "PPP"] * UNU_countries[selection, "slope"] +
  UNU_countries[selection, "intercept"]
# Set PPI missing for now because it might be that kpi is replacing real data when 
# ConnectionYear is lower than LatestYear. In that case the ppi is not accurate anymore as well.
UNU_countries[selection, "ppi"] <- NA

UNU_countries$intercept <- NULL
UNU_countries$slope <- NULL




# Some values get negative, so they should be set to zero.
# Howeve since these values are used to connect the trend over all countries with the real data, 
# zero's will result in all future values zero.
# So therefore negatives will be replaced with half the value of the previous year.
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]

selection <- which ( UNU_countries$Year == (UNU_countries$ConnectionYear + 1) & UNU_countries$kpi < 0)
# in case of empty vector don't do command
if (length(selection) > 0)
  {for (i in selection)
    {UNU_countries[i, "kpi"] <- UNU_countries[i-1, "kpi"] / 2
    }
  }

# Now add these calculated connecting values to all rows so that they can be used connect the real data to the 
# extrapolation trendline based on all countries. This connecting is done by scaling the trendline up or down
# based on the connecting point.
selection <- which ( UNU_countries$Year == (UNU_countries$ConnectionYear + 1) )
UNU_countries_selection <- UNU_countries[selection, c("UNU_Key", "Country", "kpi")]

UNU_countries_selection <- plyr::rename(UNU_countries_selection,c("kpi"="connection_value_numerator"))

# merge it back
UNU_countries <- merge(UNU_countries, UNU_countries_selection, by=c("UNU_Key", "Country"), all.x = TRUE)

# Create a subset with PPP for just the first year to extrapolate so this can be the denominator for connecting the
# extrapolation data to the real data.
selection <- which ( UNU_countries$Year == (UNU_countries$ConnectionYear + 1) )
UNU_countries_selection <- UNU_countries[selection, c("UNU_Key", "Country", "PPP")]

# Add the parameters from the first model per UNU_Key to UNU_Countries
UNU_countries_selection <- merge(UNU_countries_selection, models, by="UNU_Key", all.x = TRUE)

# Calculate model outcome for first year to extrapolate
UNU_countries_selection$connection_value_denominator <- UNU_countries_selection[, "PPP"] *
  UNU_countries_selection[, "slope"] + UNU_countries_selection[, "intercept"]

UNU_countries_selection$PPP <- NULL
UNU_countries_selection$slope <- NULL
UNU_countries_selection$intercept <- NULL

# Add the denominator to the UNU_countries
UNU_countries <- merge(UNU_countries, UNU_countries_selection, by=c("UNU_Key", "Country"), all.x=TRUE)

# Add the parameters from the first model per UNU_Key to UNU_Countries to all Years in UNU_countries
UNU_countries <- merge(UNU_countries, models, by="UNU_Key", all.x = TRUE)

# Calculate model outcome for all years to extrapolate
selection <- which ( UNU_countries$ExtrapolationApproach == "KPI_PPP" &
                       as.numeric(UNU_countries$Year) >= (UNU_countries$ConnectionYear + 1) )
UNU_countries[selection, "kpi"] <- ( ( UNU_countries[selection, "PPP"] * UNU_countries[selection, "slope"] 
                                     + UNU_countries[selection, "intercept"] )
                                     * UNU_countries[selection, "connection_value_numerator"] 
                                     / UNU_countries[selection, "connection_value_denominator"] )
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 200 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)


UNU_countries$connection_value_numerator <- NULL
UNU_countries$connection_value_denominator <- NULL
UNU_countries$slope <- NULL
UNU_countries$intercept <- NULL



# ----------------------------------------------------------
# UNU_countries: Extrapolations (2) with using the calculated connection value as a constant
# ----------------------------------------------------------
# In the first approach the trendline over all countries was fitted to existing data by using a connection
# value that was calculated by the trendline for the respective single country. This second approach uses
# that value as a constant for all future values.

# These values are actually already calculated and available in models_per_country. 
# Therefore add that data again.
UNU_countries$UNU_Key_Country <- paste(UNU_countries$UNU_Key, UNU_countries$Country, sep="_")
UNU_countries <- merge(UNU_countries, models_per_country, by="UNU_Key_Country", all.x = TRUE)
UNU_countries$UNU_Key_Country <- NULL

# When there are no extrapolation parameters available, assign it to LastReliableConstant method instead.
selection <- which( UNU_countries$ExtrapolationApproach == "ConnectionConstant" & 
                      is.na(UNU_countries$slope) )
UNU_countries[selection, "ExtrapolationApproach"] <- "LastReliableConstant"

# Now first extrapolation can be calculated with the added model parameters.
selection <- which( UNU_countries$ExtrapolationApproach == "ConnectionConstant" & 
                      as.numeric(UNU_countries$Year) == (UNU_countries$ConnectionYear + 1) )
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "PPP"] * UNU_countries[selection, "slope"] +
  UNU_countries[selection, "intercept"]
# Set PPI missing for now because it might be that kpi is replacing real data when 
# ConnectionYear is lower than LatestYear. In that case the ppi is not accurate anymore as well.
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 300 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)

UNU_countries$intercept <- NULL
UNU_countries$slope <- NULL

# Now use this calculated connection value for all Years after the ConnectionYear.
selection <- which ( UNU_countries$ExtrapolationApproach == "ConnectionConstant" &
                       as.numeric(UNU_countries$Year) == (UNU_countries$ConnectionYear + 1) )
UNU_countries_selection <- UNU_countries[selection, c("UNU_Key", "Country", "kpi")]

UNU_countries_selection <- plyr::rename(UNU_countries_selection,c("kpi"="ConnectionConstant"))

# merge it back
UNU_countries <- merge(UNU_countries, UNU_countries_selection, by=c("UNU_Key", "Country"), all.x = TRUE)

# Set ConnectionConstant for all future values
selection <- which ( UNU_countries$ExtrapolationApproach == "ConnectionConstant" &
                       as.numeric(UNU_countries$Year) > (UNU_countries$ConnectionYear + 1) )
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "ConnectionConstant"]
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 300 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)

UNU_countries$ConnectionConstant <- NULL
UNU_countries$PPP <- NULL




# ----------------------------------------------------------
# UNU_countries: Extrapolations (3) with just KPI trendline for single country over years.
# ----------------------------------------------------------
# Calculate the regression parameters for the trend for each specific country for only KPI.
selection <- which ( as.integer(UNU_countries$Year) < UNU_countries$ConnectionYear &
                       as.integer(UNU_countries$Year) >= (LatestYear - UNU_countries$YearsForTrend) &
                       #UNU_countries$flag %in% c(0, 60) & 
                       UNU_countries$kpi >= 0)
UNU_countries_selection <- UNU_countries[selection, ]

# Set an X before UNU_Key, so they will be read as character after the melt process.
UNU_countries_selection$XUNU_Key <- paste0("X", UNU_countries_selection$UNU_Key)

# Make a variable for each UNU_Key and Country combination.
UNU_countries_selection$UNU_Key_Country <- paste(UNU_countries_selection$UNU_Key, UNU_countries_selection$Country,
                                                 sep="_")

# Change Year so we start set the first year in the calculations as 1 and the LatestYear as 10 (in case of the default
# 9 years that are taken into account. For some keys there are exceptions, for instance 0502 and 0503.
UNU_countries_selection$YearReformat <- as.numeric(UNU_countries_selection$Year) - 
  (LatestYear - (UNU_countries_selection$YearsForTrend + 1) )

UNU_countries_selection$ExtrapolationApproach <- NULL
UNU_countries_selection$ConnectionYear <- NULL
UNU_countries_selection$YearsForTrend <- NULL

models_per_country_kpi <- by(UNU_countries_selection, UNU_countries_selection$UNU_Key_Country, 
                         function(UNU_countries_selection) lm(UNU_countries_selection$kpi ~ 
                                                                UNU_countries_selection$YearReformat) )
models_per_country_kpi <- sapply(models_per_country_kpi, coefficients)

# Reshape
models_per_country_kpi <- melt(models_per_country_kpi)
models_per_country_kpi <- dcast(models_per_country_kpi, Var2 ~ Var1, value.var = "value")

models_per_country_kpi <- plyr::rename(models_per_country_kpi,c("Var2"="UNU_Key_Country", "(Intercept)"="intercept", 
                                                        "UNU_countries_selection$YearReformat"="slope"))

# Add data from the last model per UNU_Key and Country to UNU_Countries
UNU_countries$UNU_Key_Country <- paste(UNU_countries$UNU_Key, UNU_countries$Country, sep="_")
UNU_countries <- merge(UNU_countries, models_per_country_kpi, by="UNU_Key_Country", all.x = TRUE)
UNU_countries$UNU_Key_Country <- NULL

# When there are no extrapolation parameters available, assign it to LastReliableConstant method instead.
selection <- which( UNU_countries$ExtrapolationApproach == "KPI" & 
                      is.na(UNU_countries$slope) )
UNU_countries[selection, "ExtrapolationApproach"] <- "LastReliableConstant"

# Now extrapolations based on KPI by Year only can be calculated with the added model parameters.
# So here Year has to be changed too so we start set the first year in the calculations as 1 and the LatestYear as 10
# (in case of the default 9 years that are taken into account.
# For some keys there are exceptions, for instance 0502 and 0503.
UNU_countries$YearReformat <- as.numeric(UNU_countries$Year) -
  (LatestYear - (UNU_countries$YearsForTrend + 1) )

selection <- which( UNU_countries$ExtrapolationApproach == "KPI" & 
                      as.numeric(UNU_countries$Year) >= (UNU_countries$ConnectionYear + 1) )
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "YearReformat"] * UNU_countries[selection, "slope"] +
  UNU_countries[selection, "intercept"]
# Set PPI missing for now because it might be that kpi is replacing real data when 
# ConnectionYear is lower than LatestYear. In that case the ppi is not accurate anymore as well.
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 400 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)

UNU_countries$YearReformat <- NULL
UNU_countries$intercept <- NULL
UNU_countries$slope <- NULL




# ----------------------------------------------------------
# UNU_countries: Extrapolations (4) with last available year as constant approach
# ----------------------------------------------------------
selection <- which ( UNU_countries$ExtrapolationApproach == "LastReliableConstant" &
                       as.numeric(UNU_countries$Year) == UNU_countries$ConnectionYear )
UNU_countries_selection <- UNU_countries[selection, c("UNU_Key", "Country", "kpi")]

UNU_countries_selection <- plyr::rename(UNU_countries_selection,c("kpi"="LastReliableConstant"))

# merge it back
UNU_countries <- merge(UNU_countries, UNU_countries_selection, by=c("UNU_Key", "Country"), all.x = TRUE)

# Now calculate the value for when a ConnectionYear is given.
# Set LastReliableConstant for all future values
selection <- which ( UNU_countries$ExtrapolationApproach == "LastReliableConstant" &
                       as.numeric(UNU_countries$Year) > UNU_countries$ConnectionYear )
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "LastReliableConstant"]
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 500 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)

UNU_countries$LastReliableConstant <- NULL


# ------------- WORLD DATA ONLY SCRIPT -----------------
# # If there is no data available in the chosen ConnectionYear, then look further back in time to a year with data.
# # This actually does not happen with the Europe data, but does happen in the non-EU data.
# # 
# # Calculate the last year with data here:
# 
# # Calculate cumulative sum of kpi per group, to see when data is latest available.
# # Sort dataframe rows by UNU_Key (A), Country (A) and Year (D)
# 
# require(data.table)
# 
# sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, -rank(UNU_countries$Year))
# UNU_countries <- UNU_countries[sortorder, ]
# 
# dtUNU_countries <- data.table(UNU_countries)
# dtUNU_countries[is.na(dtUNU_countries$kpi), "kpi"] <- 0
# dtUNU_countries[, kpi_cumsum := cumsum(kpi), by=list(UNU_Key, Country)]
# dtUNU_countries <- as.data.frame(dtUNU_countries)
# 
# # select only the ones that are zero
# dtUNU_countries <- dtUNU_countries[(dtUNU_countries$kpi_cumsum != 0), ]
# 
# # Calculate latest year for which there is data at the end the time-series.
# dtUNU_countries <- ddply( dtUNU_countries, c("UNU_Key", "Country"), summarise,
#                           LastYearWithData = max(Year, na.rm=TRUE) )
# 
# # Attach LastYearWithData column to UNU_Countries
# UNU_countries <- merge(UNU_countries, dtUNU_countries, by=c("UNU_Key", "Country"),  all.x = TRUE)
# rm(dtUNU_countries)
# 
# # When there was not a ConnectionYear given and there is no data in the default year, then go back to 
# # find an older year with data. Last year with data is already calculated. Now get the corresponding kpi.
# selection <- which ( UNU_countries$ExtrapolationApproach == "LastReliableConstant" &
#                        as.numeric(UNU_countries$Year) == UNU_countries$LastYearWithData )
# UNU_countries_selection <- UNU_countries[selection, c("UNU_Key", "Country", "kpi")]
# 
# UNU_countries_selection <- plyr::rename(UNU_countries_selection,c("kpi"="LastReliableConstant"))
# 
# # merge it back
# UNU_countries <- merge(UNU_countries, UNU_countries_selection, by=c("UNU_Key", "Country"), all.x = TRUE)
# 
# # Now calculate the value for when there was not a ConnectionYear with a KPI value given.
# selection <- which (UNU_countries$ExtrapolationApproach == "LastReliableConstant" &
#                        as.numeric(UNU_countries$Year) > UNU_countries$LastYearWithData &
#                        is.na(UNU_countries$kpi) )
# 
# UNU_countries[selection, "kpi"] <- UNU_countries[selection, "LastReliableConstant"]
# UNU_countries[selection, "ppi"] <- NA
# UNU_countries[selection, "flag"] <- 500 + (as.numeric(UNU_countries[selection, "ConnectionYear"]) - 2000)

# ------------- END WORLD DATA ONLY SCRIPT -----------------



# Other extrapolation methods can be put in this space.









# ----------------------------------------------------------
# UNU_countries: General finalization for all extrapolations
# ----------------------------------------------------------

# KPI lower than zero may not exist
selection <- which ( UNU_countries$kpi < 0 )
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0


# Now all extrapolations are done based on KPI, the PPI can be estimated 
# with proportions of the year before.
# We check for these occurences in the last 10 years of available data and all future estimations.
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]

selection <- which ( UNU_countries$kpi >= 0 & is.na(UNU_countries$ppi) &
                       as.numeric(UNU_countries$Year) > (LatestYear - 10) ) 

# Calculate the missing values
for (i in selection)
{# We go back one step for the proportions,
 # but in case there is zero data there then go back further in time to look for proportions.
  steps <- 0
  repeat {
      steps <- steps + 1
      if ( (UNU_countries[i-steps, "kpi"] > 0) & !is.na(UNU_countries[i-steps, "kpi"]) ) {break}
      }

  UNU_countries[i, "ppi"] <- UNU_countries[i-steps, "ppi"] / UNU_countries[i-steps, "kpi"] *
    UNU_countries[i, "kpi"]
}

# In case calculations have led to negatives, set them to zero.
UNU_countries[UNU_countries$kpi < 0 & !is.na(UNU_countries$kpi), "ppi"] <- 0
UNU_countries[UNU_countries$kpi < 0 & !is.na(UNU_countries$kpi), "kpi"] <- 0




# Clean-up after all future extrapolations
UNU_countries$ExtrapolationApproach <- NULL
UNU_countries$ConnectionYear <- NULL
UNU_countries$corr <- NULL
UNU_countries$LastReliableConstant <- NULL
rm(models_per_country_kpi)
rm(models_per_country)
rm(models)
rm(correlations)
rm(kpi_ppp_model)








# ----------------------------------------------------------
# UNU_countries: Estimation data of historic years
# ----------------------------------------------------------

Key_IntroductionYear <- read.csv("htbl_Key_IntroductionYear.csv", quote = "\"",
                                     colClasses = c("character", "character") )

# Add this to dataset
UNU_countries <- merge(UNU_countries, Key_IntroductionYear, by = "UNU_Key", all.x = TRUE)
rm (Key_IntroductionYear)

# IntroductionYear in the Key_IntroductionYear file gives the first year for which there can be sales.
# Before that all sales are set to zero.
# There will be a linear increase up until the first available real datapoint.

# Calculate cumulative sum of kpi per group, to see when data is first available.
# Sort dataframe rows by UNU_Key (A), Country (A) and Year (A)

require(data.table)

sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]

dtUNU_countries <- data.table(UNU_countries)
dtUNU_countries[is.na(dtUNU_countries$kpi), "kpi"] <- 0
dtUNU_countries[, kpi_cumsum := cumsum(kpi), by=list(UNU_Key, Country)]
dtUNU_countries <- as.data.frame(dtUNU_countries)

# select only the ones that are zero and exclude future years
dtUNU_countries <- dtUNU_countries[(dtUNU_countries$kpi_cumsum == 0 & dtUNU_countries$Year < LatestYear), ]

# Calculate latest year for which there is no data at the beginning of the time-series.
dtUNU_countries <- ddply( dtUNU_countries, c("UNU_Key", "Country"), summarise,
                         LastYearNoData = max(Year, na.rm=TRUE) )

# Attach LastYearNoData column to UNU_Countries
UNU_countries <- merge(UNU_countries, dtUNU_countries, by=c("UNU_Key", "Country"),  all.x = TRUE)
rm(dtUNU_countries)

# We also need the first kpi and ppi values to work with. But there are sometimes missing PPP values
# even when KPI is avaliable. First solve that with proportions of first later year.
selection <- which ( UNU_countries$kpi > 0 & is.na(UNU_countries$ppi) &
                       as.numeric(UNU_countries$Year) > as.numeric(UNU_countries$LastYearNoData) &
                       as.numeric(UNU_countries$Year) < LatestYear )

# reverse order so data can be filled from the last record to the first.
selection <- rev(selection)

# Calculate the missing values
for (i in selection)
{# We go forward one step for the proportions,
  # but in case there is zero data there then go further in time to look for proportions.
  steps <- 0
  repeat {
    steps <- steps + 1
    if ( (UNU_countries[i+steps, "kpi"] > 0) & !is.na(UNU_countries[i+steps, "kpi"]) ) {break}
  }

  UNU_countries[i, "ppi"] <- UNU_countries[i+steps, "ppi"] / UNU_countries[i+steps, "kpi"] *
    UNU_countries[i, "kpi"]
}

# Now calculte the first available KPI and PPI values.
# The first available real datapoint might be missing or very high compered to the following years, resulting in
# incorrect historic estimations. Therefore instead of the first real datapoint, an average of the 
# first 3 datapoints is used to estimated historic values.
# First real data values:
selection1 <- which (as.numeric(UNU_countries$Year) == as.numeric(UNU_countries$LastYearNoData)+1)
# Second real data values:
selection2 <- selection1 + 1
# Third real data values:
selection3 <- selection1 + 2
# Combined:
selection <- c(selection1, selection2, selection3)
UNU_countries_LYND_values <- ddply( UNU_countries[selection, ], c("UNU_Key", "Country"), summarise,
                          kpi_first_data_point = mean(kpi), 
                          ppi_first_data_point = mean(ppi) )

rm(selection1)
rm(selection2)
rm(selection3)


# Attach to UNU_countries
UNU_countries <- merge(UNU_countries, UNU_countries_LYND_values, by=c("UNU_Key", "Country"),  all.x = TRUE)
rm(UNU_countries_LYND_values)


# Calculate number of years that have to be estimated.
# + 2 for the year before the year of introduction which has to become 0 and for the latest year itself
UNU_countries$YearsToEstimate <- as.numeric(UNU_countries$LastYearNoData) -
  as.numeric(UNU_countries$IntroductionYear) + 2


# Perform the calculations:
# Calculate historic estimations for years starting in year of introduction to first available data.
selection <- which ( as.numeric(UNU_countries$Year) <= as.numeric(UNU_countries$LastYearNoData) &
                       as.numeric(UNU_countries$Year) >= as.numeric(UNU_countries$IntroductionYear ) )
# kpi
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_first_data_point"] /
  UNU_countries[selection, "YearsToEstimate"] * 
  ( as.numeric(UNU_countries[selection, "Year"]) + 1 - as.numeric(UNU_countries[selection, "IntroductionYear"]) )
# ppi
UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_first_data_point"] /
  UNU_countries[selection, "YearsToEstimate"] * 
  ( as.numeric(UNU_countries[selection, "Year"]) + 1 - as.numeric(UNU_countries[selection, "IntroductionYear"]) )
# flag
UNU_countries[selection, "flag"] <- 200


# Years before IntroductionYear are all set to zero.
selection <- which ( as.numeric(UNU_countries$Year) < as.numeric(UNU_countries$IntroductionYear ) )

UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0
UNU_countries[selection, "flag"] <- 200


# Clean-up
UNU_countries$IntroductionYear <- NULL
UNU_countries$LastYearNoData <- NULL
UNU_countries$YearsForTrend <- NULL
UNU_countries$kpi_first_data_point <- NULL
UNU_countries$ppi_first_data_point <- NULL
UNU_countries$YearsToEstimate <- NULL


# all values that are smaller than zero will get 0.
selection <- which (UNU_countries$kpi < 0)
UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0




# ----------------------------------------------------------
# UNU_countries: Add inhabitants to added years 
# ----------------------------------------------------------

# Inhabitants are missing for the estimated historic and future values.
# We remove the current variable and merge it back again.
UNU_countries$Inhabitants <- NULL
UNU_countries <- merge(UNU_countries, Population,  by=c("Country", "Year"),  all.x = TRUE)

# Clean-up
# LatestYear will be used again in 3h.
rm(i, selection, sortorder, years)
rm(allyears, oneyear)

detach("package:dplyr", unload=TRUE)

