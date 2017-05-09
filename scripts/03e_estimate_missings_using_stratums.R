# ----------------------------------------------------------
#  UNU_countries: Estimate missing values based on stratum ratio's
# ----------------------------------------------------------

# Preparing the data for the calculations

# Attach purchasing power data
UNU_countries <- merge(UNU_countries, PurPow,  by=c("Country", "Year"), all.x = TRUE)


# Calculate purchasing power per country and year
Purpow2 <- ddply(UNU_countries, c("Country", "Year", "Stratum"), summarise,
                               Inhabitants = median(Inhabitants, na.rm=TRUE),
                               PPP = median(PPP, na.rm=TRUE))
                               
# Calculate total PPP per country and year
Purpow2$totalPPP <- Purpow2$PPP * Purpow2$Inhabitants

# Calculate purchasing power per stratum and year
Purpow_strat <- ddply(Purpow2, c("Stratum", "Year"), summarise,
                 Inhabitants = sum(Inhabitants, na.rm=TRUE),
                 totalPPP = sum(totalPPP, na.rm=TRUE))

# Calculate average purchasing power per stratum and year
selection <- which(Purpow_strat$Inhabitants > 0)
Purpow_strat[selection, "PPP_strat_mean"] <- Purpow_strat[selection, "totalPPP"] / Purpow_strat[selection, "Inhabitants"]

Purpow_strat$Inhabitants <-NULL
Purpow_strat$totalPPP <-NULL

# First reshape the stratum data into wide form
Purpow_strat_wide <- dcast(Purpow_strat, Year ~ Stratum, value.var = "PPP_strat_mean")
names(Purpow_strat_wide)[2:ncol(Purpow_strat_wide)] <- 
  paste("PPP_s", names(Purpow_strat_wide)[2:ncol(Purpow_strat_wide)], "_mean", sep = "")

# Merge with UNU_countries
UNU_countries <- merge(UNU_countries, Purpow_strat_wide,  by="Year", all.x = TRUE)
rm(PurPow, Purpow2, Purpow_strat, Purpow_strat_wide)

# As highests edge for stratum 1 we take the Luxemburg values.
# Select the Luxemburg data and add it to the dataset as separate variables

selection <- which(UNU_countries$Country == "LUX")
high_s1 <- UNU_countries[selection, c("UNU_Key", "Year", "kpi", "ppi", "PPP")]
high_s1 <- plyr::rename(high_s1,c("kpi"="kpi_high", "ppi"="ppi_high", "PPP"="PPP_high"))

# merge
UNU_countries <- merge(UNU_countries, high_s1,  by=c("UNU_Key", "Year"), all.x = TRUE)
rm(high_s1)

# As lowest edge for stratum 3 we take the Romania values.
# Select the Romania data and add it to the dataset as separate variables
selection <- which(UNU_countries$Country == "ROU")
low_s1 <- UNU_countries[selection, c("UNU_Key", "Year", "kpi", "ppi", "PPP")]
low_s1 <- plyr::rename(low_s1,c("kpi"="kpi_low", "ppi"="ppi_low", "PPP"="PPP_low"))

# merge
UNU_countries <- merge(UNU_countries, low_s1,  by=c("UNU_Key", "Year"), all.x = TRUE)
rm(low_s1)

# When values of the highest point are lower than the stratum 1 mean or when
# the values of the lowest point are higher than the stratum 3 mean, they will
# be set equal to the stratum mean.
# This is right to do for UNU_keys with a general increase over years which are by far the most.
# But with a general decreasing slope over years this is not good.
# Perhaps best to make a variable that tells us about the direction of the slope and use
# that here. Maybe even a simple selection as stratum1_mean < stratum2_mean?

# Also missing values are changed for the adjacent stratum mean.

# High end
selection <- which((UNU_countries$kpi_high < UNU_countries$kpi_s1) |
  (is.na(UNU_countries$kpi_high) & !is.na(UNU_countries$kpi_s1)))

UNU_countries[selection, "kpi_high"] <- UNU_countries[selection, "kpi_s1"]
UNU_countries[selection, "ppi_high"] <- UNU_countries[selection, "ppi_s1"]

# Low end
# Perhaps better to change s3 value to a solid name for the last stratum variable.
# In case of more stratum categories later on.
selection <- which((UNU_countries$kpi_low > UNU_countries$kpi_s1) |
                     (is.na(UNU_countries$kpi_low) & !is.na(UNU_countries$kpi_s3)))

UNU_countries[selection, "kpi_low"] <- UNU_countries[selection, "kpi_s3"]
UNU_countries[selection, "ppi_low"] <- UNU_countries[selection, "ppi_s3"]

# In case of a missing PPP value we estimate it with the value of the previous year
# adjusted for change in ratio of stratum PPP

# Sort dataframe rows by UNU_Key, Country and Year
# First sort dataframe rows by UNU_Key, Country and Year descending.
sortorder <- order(UNU_countries$UNU_Key, UNU_countries$Country, -rank(UNU_countries$Year))
UNU_countries <- UNU_countries[sortorder, ]

rownumber <- 2:nrow(UNU_countries)

UNU_countries[rownumber, "selection"] <- is.na(UNU_countries[rownumber, "PPP"]) &
                UNU_countries[rownumber, "UNU_Key"] == UNU_countries[rownumber - 1, "UNU_Key"] & 
                UNU_countries[rownumber, "Country"] == UNU_countries[rownumber - 1, "Country"]  

# Stratum 1
UNU_countries$selection2 <- UNU_countries$selection == TRUE & UNU_countries$Stratum ==1
selection <- which(UNU_countries$selection2)
UNU_countries[selection, "PPP"] <- UNU_countries[selection -1, "PPP"] /
  UNU_countries[selection -1, "PPP_s1_mean"] * UNU_countries[selection, "PPP_s1_mean"]
    
# Stratum 2
UNU_countries$selection2 <- UNU_countries$selection == TRUE & UNU_countries$Stratum ==2
selection <- which(UNU_countries$selection2)
UNU_countries[selection, "PPP"] <- UNU_countries[selection -1, "PPP"] /
  UNU_countries[selection -1, "PPP_s2_mean"] * UNU_countries[selection, "PPP_s2_mean"]

# Stratum 3
UNU_countries$selection2 <- UNU_countries$selection == TRUE & UNU_countries$Stratum ==3
selection <- which(UNU_countries$selection2)
UNU_countries[selection, "PPP"] <- UNU_countries[selection -1, "PPP"] /
  UNU_countries[selection -1, "PPP_s3_mean"] * UNU_countries[selection, "PPP_s3_mean"]

UNU_countries$selection <-NULL
UNU_countries$selection2 <-NULL

# If there is still missing purchasing power, it is set to the stratum mean.
# Stratum 1
UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 1 , "PPP"]<-
  UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 1 , "PPP_s1_mean"]

# Stratum 2
UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 2 , "PPP"]<-
  UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 2 , "PPP_s2_mean"]

# Stratum 3
UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 3 , "PPP"]<-
  UNU_countries[is.na(UNU_countries$PPP) & UNU_countries$Stratum == 3 , "PPP_s3_mean"]


# ----------------------------------------------------------
#  UNU_countries: Estimate missing values based on stratum ratio's
# ----------------------------------------------------------

# Missing values are estimated as follows:
# First determine between which of the 5 purchasing power marking points
# (stratums 1:3 and the lowest and highest borders) the countries purchasing power lies.

# The we calculate:
# C: kpi of next higher marking point - kpi of next lower marking point
# D: PPP of next higher marking point - PPP of next lower marking point
# B: PPP of country - PPP of next lower marking point
# A: Difference between to be estimated kpi of country and
#    kpi of next lower marking point. This i calculated with: C / D * B.

# The kpi to be extimated is now A + the kpi of next lower marking point.

# Stratum 1, PPP higher than stratum mean
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==1 &
                     UNU_countries$PPP > UNU_countries$PPP_s1_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_s1"] +
  (UNU_countries[selection, "kpi_high"] - UNU_countries[selection, "kpi_s1"]) /
  (UNU_countries[selection, "PPP_high"] - UNU_countries[selection, "PPP_s1_mean"]) *
  (UNU_countries[selection, "PPP"]      - UNU_countries[selection, "PPP_s1_mean"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_s1"] +
  (UNU_countries[selection, "ppi_high"] - UNU_countries[selection, "ppi_s1"]) /
  (UNU_countries[selection, "PPP_high"] - UNU_countries[selection, "PPP_s1_mean"]) *
  (UNU_countries[selection, "PPP"]      - UNU_countries[selection, "PPP_s1_mean"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# Stratum 1, PPP lower than stratum mean
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==1 &
                     UNU_countries$PPP <= UNU_countries$PPP_s1_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_s2"] +
  (UNU_countries[selection, "kpi_s1"]       - UNU_countries[selection, "kpi_s2"]) /
  (UNU_countries[selection, "PPP_s1_mean"]  - UNU_countries[selection, "PPP_s2_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s2_mean"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_s2"] +
  (UNU_countries[selection, "ppi_s1"]       - UNU_countries[selection, "ppi_s2"]) /
  (UNU_countries[selection, "PPP_s1_mean"]  - UNU_countries[selection, "PPP_s2_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s2_mean"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# Stratum 2, PPP higher than stratum mean (samer as stratum 1, PPP lower than stratum mean)
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==2 &
                     UNU_countries$PPP > UNU_countries$PPP_s2_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_s2"] +
  (UNU_countries[selection, "kpi_s1"]       - UNU_countries[selection, "kpi_s2"]) /
  (UNU_countries[selection, "PPP_s1_mean"]  - UNU_countries[selection, "PPP_s2_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s2_mean"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_s2"] +
  (UNU_countries[selection, "ppi_s1"]       - UNU_countries[selection, "ppi_s2"]) /
  (UNU_countries[selection, "PPP_s1_mean"]  - UNU_countries[selection, "PPP_s2_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s2_mean"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# Stratum 2, PPP lower than stratum mean
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==2 &
                     UNU_countries$PPP <= UNU_countries$PPP_s2_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_s3"] +
  (UNU_countries[selection, "kpi_s2"]       - UNU_countries[selection, "kpi_s3"]) /
  (UNU_countries[selection, "PPP_s2_mean"]  - UNU_countries[selection, "PPP_s3_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s3_mean"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_s3"] +
  (UNU_countries[selection, "ppi_s2"]       - UNU_countries[selection, "ppi_s3"]) /
  (UNU_countries[selection, "PPP_s2_mean"]  - UNU_countries[selection, "PPP_s3_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s3_mean"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# Stratum 3, PPP higher than stratum mean (samer as stratum 2, PPP lower than stratum mean)
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==3 &
                     UNU_countries$PPP > UNU_countries$PPP_s3_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_s3"] +
  (UNU_countries[selection, "kpi_s2"]       - UNU_countries[selection, "kpi_s3"]) /
  (UNU_countries[selection, "PPP_s2_mean"]  - UNU_countries[selection, "PPP_s3_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s3_mean"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_s3"] +
  (UNU_countries[selection, "ppi_s2"]       - UNU_countries[selection, "ppi_s3"]) /
  (UNU_countries[selection, "PPP_s2_mean"]  - UNU_countries[selection, "PPP_s3_mean"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_s3_mean"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# Stratum 3, PPP lower than stratum mean
selection <- which(is.na(UNU_countries$kpi) & UNU_countries$Stratum==3 &
                     UNU_countries$PPP <= UNU_countries$PPP_s3_mean)

# A + C / D * B
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_low"] +
  (UNU_countries[selection, "kpi_s3"]       - UNU_countries[selection, "kpi_low"]) /
  (UNU_countries[selection, "PPP_s3_mean"]  - UNU_countries[selection, "PPP_low"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_low"]) 

UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_low"] +
  (UNU_countries[selection, "ppi_s3"]       - UNU_countries[selection, "ppi_low"]) /
  (UNU_countries[selection, "PPP_s3_mean"]  - UNU_countries[selection, "PPP_low"]) *
  (UNU_countries[selection, "PPP"]          - UNU_countries[selection, "PPP_low"]) 

UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 20



# clean-up
UNU_countries$kpi_s1 <-NULL
UNU_countries$kpi_s2 <-NULL
UNU_countries$kpi_s3 <-NULL
UNU_countries$ppi_s1 <-NULL
UNU_countries$ppi_s2 <-NULL
UNU_countries$ppi_s3 <-NULL
UNU_countries$PPP <-NULL
UNU_countries$PPP <-NULL
UNU_countries$med_kpi <-NULL
UNU_countries$PPP <-NULL
UNU_countries$PPP_s1_mean <-NULL
UNU_countries$PPP_s2_mean <-NULL
UNU_countries$PPP_s3_mean <-NULL
UNU_countries$kpi_high <-NULL
UNU_countries$ppi_high <-NULL
UNU_countries$PPP_high <-NULL
UNU_countries$kpi_low <-NULL
UNU_countries$ppi_low <-NULL
UNU_countries$PPP_low <-NULL

rm(rownumber, sortorder)




