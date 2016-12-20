# ----------------------------------------------------------
#  UNU_countries: Extend time series back to 1980 and a few years into the future
# ----------------------------------------------------------

# Growth factor per year is 2 percent. Other values can be given using the 
# csv file with alternative growth factors per UNU_Key.

require(data.table)

# ----------------------------------------------------------
# tsgf: Read raw version of time series growth factor (tsgf)
# ----------------------------------------------------------
tsgf <- read.csv("time_series_growth_factors_per_UNU_Key.csv", quote = "\"",
                 colClasses = c("character", "numeric"))



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
years<- c(1980:1994, (LatestYear+1): (LatestYear+9))

# Multiply Get data for every year.
allyears <- merge(oneyear, years, all = TRUE)

names(allyears)[ncol(allyears)] <- "Year"

# Add it to the dataset
UNU_countries <- rbind.fill(UNU_countries, allyears)


# Attach grow factor for specific UNU_Keys
UNU_countries <- merge(UNU_countries, tsgf, by = "UNU_Key", all.x = TRUE)
rm (tsgf)

# All not mentionend UNU_Keys get growthfactor 0.02
selection <- which(is.na(UNU_countries$growth_factor))
UNU_countries[selection, "growth_factor"] <- 0.02


# ----------------------------------------------------------
# UNU_countries: Estimate data of historic years
# ----------------------------------------------------------

# Calculte cumulative sum of kpi per group, to see when data is first available.
# Sort dataframe rows by UNU_Key (A), Country (A) and Year (A)
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, (UNU_countries$Year))
UNU_countries <- UNU_countries[sortorder, ]

UNU_countries2 <- data.table(UNU_countries)
UNU_countries2[is.na(UNU_countries2$kpi), "kpi"] <- 0
UNU_countries2[, kpi_cumsum := cumsum(kpi), by=list(UNU_Key, Country)]
UNU_countries2 <- as.data.frame(UNU_countries2)

UNU_countries2 <- UNU_countries2[(UNU_countries2$kpi_cumsum == 0 & UNU_countries2$Year < LatestYear), ]

# Calculate latest year for which there is no data at the beginning of the time-series.
UNU_countries2 <- ddply( UNU_countries2, c("UNU_Key", "Country"), summarise,
                        LastYearNoData = max(Year, na.rm=TRUE) )

# Attach LastYearNoData column to UNU_Countries
UNU_countries <- merge(UNU_countries, UNU_countries2, by=c("UNU_Key", "Country"),  all.x = TRUE)
rm(UNU_countries2)

# Sort dataframe rows by UNU_Key (A) , Country (A) and Year (D)
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, -rank(UNU_countries$Year))
UNU_countries <- UNU_countries[sortorder, ]




# Estimation of the first missing historic year is done based on the mean of the 3 next later years
# because the first avaible year can contain missing or bad data.

selection <- which( UNU_countries$Year == UNU_countries$LastYearNoData )

UNU_countries[selection, "kpi"] <- (UNU_countries[selection - 1, "kpi"] + UNU_countries[selection - 2, "kpi"] +
                                        UNU_countries[selection - 3, "kpi"]) /3 *
                                        (1 - UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "ppi"] <- (UNU_countries[selection - 1, "ppi"] + UNU_countries[selection - 2, "ppi"] +
                                        UNU_countries[selection - 3, "ppi"]) /3 *
                                        (1 - UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "flag"] <- 200


# Now estimated all previous historic years from the most recently calculated historic year.
selection <- which( (as.integer(UNU_countries$Year) <= as.integer(UNU_countries$LastYearNoData) -1 ) )

for (i in selection)
  {UNU_countries[i, "kpi"] <- UNU_countries[(i - 1), "kpi"] *
                                        (1 - UNU_countries[i, "growth_factor"])
   UNU_countries[i, "ppi"] <- UNU_countries[(i - 1), "ppi"] *
                                        (1 - UNU_countries[i, "growth_factor"])
   UNU_countries[i, "flag"] <- 200
  }


# Force values of zero for all historic data of solar panels
selection <- which(UNU_countries$UNU_Key == "0002" & UNU_countries$flag == 200)
UNU_countries[selection, "kpi"] <- 0.
UNU_countries[selection, "ppi"] <- 0.

# Force values of zero for all historic data of mobile phones, laptops before 1995
selection <- which( UNU_countries$flag == 200 &
                     (UNU_countries$UNU_Key == "0303" |
                      UNU_countries$UNU_Key == "0306" |
                      UNU_countries$UNU_Key == "0309") )
UNU_countries[selection, "kpi"] <- 0.
UNU_countries[selection, "ppi"] <- 0.

# Because the mean of 3 years is used to attach the predicted data to the real data, it is possible that 
# in cases that the first year with real data is much lower than the next two years,
# there will be a a hump in the time series data.
# This will be corrected here by replacing the value for the first year with real data with the 
# mean of the surrounding years.

# select years and stratums
selection <- which( as.integer(UNU_countries$Year) == as.integer(UNU_countries$LastYearNoData) +1 )

# select when middle year is lower than two surrounding years.
UNU_countries[selection, "selection2"] <-
                            (UNU_countries[selection, "kpi"] < UNU_countries[selection - 1, "kpi"]) &
                            (UNU_countries[selection, "kpi"] < UNU_countries[selection + 1, "kpi"])

# Track location of TRUE values
selection2 <- which (UNU_countries$selection2)

# Do the correction
UNU_countries[selection2, "kpi"] <- (UNU_countries[selection2 - 1, "kpi"] + UNU_countries[selection2 + 1, "kpi"]) / 2
UNU_countries[selection2, "ppi"] <- (UNU_countries[selection2 - 1, "ppi"] + UNU_countries[selection2 + 1, "ppi"]) / 2
UNU_countries[selection2, "flag"] <- 200
                
UNU_countries$selection2 <-NULL
UNU_countries$LastYearNoData <- NULL

# ----------------------------------------------------------
# UNU_countries: Estimate data of future years
# ----------------------------------------------------------

# Sort dataframe rows by UNU_Key (A) , Country (A) and Year (A)
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, rank(UNU_countries$Year))
UNU_countries <- UNU_countries[sortorder, ]

# Estimation of the first future year is done based on the mean of the 3 previous years 
# because the last datayear for which there is data can contain missing or bad data.
# Exception are the PV panels (UNU_Key 0002). Estimation is always on the sole last known data.

selection <- which((as.integer(UNU_countries$Year) == (LatestYear + 1) & UNU_countries$UNU_Key != "0002"))

UNU_countries[selection, "kpi"] <- (UNU_countries[selection - 1, "kpi"] + UNU_countries[selection - 2, "kpi"] +
                                      UNU_countries[selection - 3, "kpi"]) /3 *
                                      (1 + UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "ppi"] <- (UNU_countries[selection - 1, "ppi"] + UNU_countries[selection - 2, "ppi"] +
                                      UNU_countries[selection - 3, "ppi"]) /3 *
                                      (1 + UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "flag"] <- 200


# PV panels were not calculated yet. We do them the same way right from the last known data value.
selection <- which((as.integer(UNU_countries$Year) == (LatestYear + 1) & UNU_countries$UNU_Key == "0002"))

UNU_countries[selection, "kpi"] <- UNU_countries[selection - 1, "kpi"] *
                                      (1 + UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "ppi"] <- UNU_countries[selection - 1, "ppi"] *
                                      (1 + UNU_countries[selection, "growth_factor"])

UNU_countries[selection, "flag"] <- 200



# Now estimated all later future years from the most recently calculated future year.
selection <- which(as.integer(UNU_countries$Year) > (LatestYear + 1))

for (i in selection)
{UNU_countries[i, "kpi"] <- UNU_countries[(i - 1), "kpi"] *
                                    (1 + UNU_countries[i, "growth_factor"])
UNU_countries[i, "ppi"] <- UNU_countries[(i - 1), "ppi"] *
                                    (1 + UNU_countries[i, "growth_factor"])
UNU_countries[i, "flag"] <- 200
}

UNU_countries$growth_factor <- NULL


# Because the mean of 3 years is used to attach the predicted data to the real data, it is possible that 
# in cases that the last year with real data is much lower than the previous two years,
# there will be a a hump in the time series data.
# This will be corrected here by replacing the value for the last year with real data with the 
# mean of the surrounding years.

# select years
selection <-  which( (as.integer(UNU_countries$Year) == LatestYear) )

# select when middle year is higher than two surrounding years.
UNU_countries[selection, "selection2"] <-
  (UNU_countries[selection, "kpi"] > UNU_countries[selection - 1, "kpi"]) &
  (UNU_countries[selection, "kpi"] > UNU_countries[selection + 1, "kpi"])

# Track location of TRUE values
selection2 <- which (UNU_countries$selection2)

# Do the correction
UNU_countries[selection2, "kpi"] <- (UNU_countries[selection2 - 1, "kpi"] + UNU_countries[selection2 + 1, "kpi"]) / 2
UNU_countries[selection2, "ppi"] <- (UNU_countries[selection2 - 1, "ppi"] + UNU_countries[selection2 + 1, "ppi"]) / 2
UNU_countries[selection2, "flag"] <- 200

UNU_countries$selection2 <-NULL


# ----------------------------------------------------------
# UNU_countries: Enhance datafile
# ----------------------------------------------------------

# Inhabitants are missing for the estimated historic and future values.
# We remove the current variable and merge it back again.
UNU_countries$Inhabitants <- NULL
UNU_countries <- merge(UNU_countries, Population,  by=c("Country", "Year"),  all.x = TRUE)

# clean-up
rm(i, LatestYear, selection, selection2, sortorder, years)
rm(allyears, oneyear, Population)



