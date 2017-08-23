# -------------------------------------------------------------------------------------------
# Aggregate kpi for POM, WEEE and Stock data to UNU_Key groups and stratum
# -------------------------------------------------------------------------------------------

# Calculate aggregates based on total weight and number of units.
# They are afterwards used to calculate the values per inhabitant (kpi and ppi).

# First add stratum aggregates, so those can afterwards also be aggrageted to UNU__Key groups.

# ----------------------------------------------------------
#  mydf: Calculate stratum totals
# ----------------------------------------------------------
mydf_strat_tot <- ddply(mydf, c("UNU_Key", "Year", "Stratum"), summarise,
                        var_t = sum(var_t, na.rm=TRUE),
                        var_p = sum(var_p, na.rm=TRUE),
                        Inhabitants = sum(Inhabitants, na.rm=TRUE))

mydf_strat_tot$Country <- "agg"

# Now add this to mydf.
mydf <- rbind.fill(mydf, mydf_strat_tot)



# Now calculate all the UNU_Key aggregates.

# ----------------------------------------------------------
# mydf: Calculate aggregated data of totals per Country and Year
# ----------------------------------------------------------

# Create aggregate per Country and Year
mydf_UNU_tot <- ddply(mydf, c("Stratum", "Country", "Year"), summarise,
                      var_t = sum(var_t, na.rm=TRUE),
                      var_p = sum(var_p, na.rm=TRUE),
                      Inhabitants = median(Inhabitants, na.rm=TRUE) )

mydf_UNU_tot$UNU_Key <- "Total"



# ----------------------------------------------------------
#  mydf: Add EU6PV and EU10PV UNU_Key aggregations
# ----------------------------------------------------------

# Add EU6PV and EU10PV aggregations  to mydf
mydf <- merge(mydf, htbl_Key_Aggregates, by="UNU_Key", all.x = TRUE)

# Add names to categories so it is clear to which aggregate they belong.
mydf$EU6PV <- paste("EU6PV", mydf$EU6PV, sep = "_" )
mydf$EU10PV <- paste("EU10PV", mydf$EU10PV, sep = "_" )

# Aggregate to EU6PV
mydf_EU6PV <- ddply(mydf, c("EU6PV", "Year", "Country", "Stratum"), summarise,
                    var_t = sum(var_t, na.rm=TRUE),
                    var_p = sum(var_p, na.rm=TRUE),
                    Inhabitants = median(Inhabitants, na.rm=TRUE) )

mydf_EU6PV <- plyr::rename(mydf_EU6PV,c("EU6PV"="UNU_Key"))

# Aggregate to EU10PV
mydf_EU10PV <- ddply(mydf, c("EU10PV", "Year", "Country", "Stratum"), summarise,
                     var_t = sum(var_t, na.rm=TRUE),
                     var_p = sum(var_p, na.rm=TRUE),
                     Inhabitants = median(Inhabitants, na.rm=TRUE) )

mydf_EU10PV <- plyr::rename(mydf_EU10PV,c("EU10PV"="UNU_Key"))



# ----------------------------------------------------------
#  mydf: Calculate the totals per country excluding 0001 and 0002
# ----------------------------------------------------------

# Exclude 0001 and 0002 from the calculations
selection <- which(substring(mydf$UNU_Key, 1, 2) != "00")

# Aggregate
mydf_UNU_tot_ex_00 <- ddply(mydf[selection, ], c("Year", "Country", "Stratum"), summarise,
                            var_t = sum(var_t, na.rm=TRUE),
                            var_p = sum(var_p, na.rm=TRUE),
                            Inhabitants = median(Inhabitants, na.rm=TRUE) )

mydf_UNU_tot_ex_00$UNU_Key <- "Total-excl.PV"




# ----------------------------------------------------------
#  mydf_all: Combine all data into mydf_all
# ----------------------------------------------------------

mydf_all <- rbind.fill(mydf,     mydf_UNU_tot)
mydf_all <- rbind.fill(mydf_all, mydf_EU6PV)
mydf_all <- rbind.fill(mydf_all, mydf_EU10PV)
mydf_all <- rbind.fill(mydf_all, mydf_UNU_tot_ex_00)

rm(mydf)
rm(mydf_UNU_tot)
rm(mydf_strat_tot)
rm(mydf_EU6PV)
rm(mydf_EU10PV)
rm(mydf_UNU_tot_ex_00)

