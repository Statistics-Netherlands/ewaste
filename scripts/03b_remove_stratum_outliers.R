# ----------------------------------------------------------
#  UNU_countries: Remove unreliable values (extremes) based on values of other years per Country and UNU_Key
# ----------------------------------------------------------

# ----------------------------------------------------------
# Outlier detection using Median Absolute Deviation (MAD).
#
# For this outlier detection method, the median of values is calculated
# Then, the difference is calculated between each of these values and this median.
# These differences are expressed as their absolute values, and a new median of these absolute values is calculated.
# This is median absolute deviation (MAD).
# If a value is a certain number of MAD away from the median of the residuals, that value is classified as an outlier.
# The threshold is 4 MAD for the calculations performed in this project.
# Big advantage of the MAD is that it works with heavily tailed data too, which we see a lot in e-waste statistics.
# ----------------------------------------------------------

# *** Collect the neccesary data ***
# 1 - mad kg per inhabitant
dfmad_kpi <- as.data.frame(as.table(by(UNU_countries[, "kpi"],
                               UNU_countries[, c("UNU_Key", "Year", "Stratum")],
                               mad, constant = 1, na.rm=TRUE)))

names(dfmad_kpi)[4] <- "mad_kpi"

# 2 - number of valid values used in mad calculation
dfvalidn <- count(UNU_countries[!is.na(UNU_countries$kpi), ][ , c("UNU_Key", "Year", "Stratum")])
names(dfvalidn)[4] <- "validn"

# 3 - the last value needed is the median.
dfmed_kpi <- as.data.frame(as.table(by(UNU_countries[, "kpi"],
                                       UNU_countries[, c("UNU_Key", "Year", "Stratum")],
                                       median, na.rm=TRUE)))
names(dfmed_kpi)[4] <- "med_kpi"

# merge the first two, removing the NA's of groups with no data
dfmad_kpi <- merge(dfmad_kpi, dfvalidn, by=c("UNU_Key", "Year", "Stratum"))

# merge the median with them
dfmad_kpi <- merge(dfmad_kpi, dfmed_kpi, by=c("UNU_Key", "Year", "Stratum"))

# now merge these values with the original data
UNU_countries <- merge(UNU_countries, dfmad_kpi, by=c("UNU_Key", "Year", "Stratum"), all.x = TRUE)
# Set our sorting order correct again
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, UNU_countries$Year)
UNU_countries <- UNU_countries[sortorder, ]



# *** The outlier calculation ***
# Highs
selection <- which(UNU_countries$flag <50 &
                     UNU_countries$validn >= 6 &
                     UNU_countries$kpi - UNU_countries$med_kpi > distance_mad * UNU_countries$mad_kpi)

UNU_countries[selection, "kpi"] <- NA
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 3.

# and lows
selection <- which(UNU_countries$flag <50 &
                     UNU_countries$validn >= 6 &
                     UNU_countries$med_kpi - UNU_countries$kpi > distance_mad * UNU_countries$mad_kpi)

UNU_countries[selection, "kpi"] <- NA
UNU_countries[selection, "ppi"] <- NA
UNU_countries[selection, "flag"] <- 2.


# clean-up
UNU_countries$mad_kpi <-NULL
UNU_countries$validn <-NULL
UNU_countries$med_kpi <-NULL

rm(dfmad_kpi)
rm(dfmed_kpi)
rm(dfvalidn)
