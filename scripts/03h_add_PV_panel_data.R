# ----------------------------------------------------------
#  UNU_countries: Add Eurostat PV panel data (UNU_Key 0002) to use instead of the statistics
# ----------------------------------------------------------

# At Eurostat there is data available about the Net maximum capacity of Solar Photovoltaic in Megawatt.
# It can be found on the Eurostat website http://ec.europa.eu/eurostat/data/database
# Search for [nrg_113a]
# Or look in the navigation tree under:
# Environment and energy -> Engergy (nrg) -> Energy statistics - Infrastructure (nrg_11) ->
# Infrastructure - electricity - annual data [nrg_113a]
# Look for the indicator "Net maximum capacity - Solar Photovoltaic".

# The file that is going to be read after this is already restructured in Excel and multiplied with factors
# for number of panels and kg per megawatt.


# ----------------------------------------------------------
# PVpanels: Read raw version of POM data
# ----------------------------------------------------------
PVpanels <- read.csv("solar_panel_data.csv", quote = "\"",
                     colClasses = c("character", "character", "character", "numeric", "numeric"))

# merge with UNU_countries
UNU_countries <- merge(UNU_countries, PVpanels,  by=c("UNU_Key", "Year", "Country"),  all.x = TRUE)

# Take over data
selection <- which(UNU_countries$UNU_Key == "0002")
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kg"] / UNU_countries[selection, "Inhabitants"]
UNU_countries[selection, "ppi"] <- UNU_countries[selection, "units"] / UNU_countries[selection, "Inhabitants"]
UNU_countries[selection, "flag"] <- 53


# clean-up
UNU_countries$kg <- NULL
UNU_countries$units <- NULL

rm (selection)

