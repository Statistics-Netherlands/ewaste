# ----------------------------------------------------------
#  UNU_countries: Change values in UNU_Key 0308 (CRT monitors)
# ----------------------------------------------------------

# Data from UNU_Key 0308, (CRT monitors) looks unreliable.
# This syntax will change the data with the following formula:
# CRT_Mon[kg] = ((Desktop sales[pieces] * 116% ) - Flatpanels[pieces] ) * average_weight_CRT.
# According to the United Nations University, sales of CRT's are around 116% of desktop sales minus the flatpanels.

# First the total kg and number of pieces have to be calculated
# We will work in a copy of the dataset
UNU_countries2 <- UNU_countries
UNU_countries2$POM_kg <- UNU_countries2$kpi * UNU_countries2$Inhabitants 
UNU_countries2$POM_Pieces <- UNU_countries2$ppi * UNU_countries2$Inhabitants 

# Move number of desktops to column.
selection <- which(UNU_countries2$UNU_Key == "0302")
desktops <- UNU_countries2[selection, ]

desktops <- plyr::rename(desktops, c("POM_Pieces"="POM_Pieces_0302"))
desktops <- desktops[, c("Year", "Country", "POM_Pieces_0302")]

# Move number of flatpanels to column.
selection <- which(UNU_countries2$UNU_Key == "0309")
flatpanels <- UNU_countries2[selection, ]

flatpanels <- plyr::rename(flatpanels, c("POM_Pieces"="POM_Pieces_0309"))
flatpanels <- flatpanels[, c("Year", "Country", "POM_Pieces_0309")]

# Merge desktops and flatpanels with UNU_countries2
UNU_countries2 <- merge(UNU_countries2, desktops, by=c("Year", "Country"), all.x = TRUE)
UNU_countries2 <- merge(UNU_countries2, flatpanels, by=c("Year", "Country"), all.x = TRUE)

rm(desktops, flatpanels)

# Calculate the number of CRT monitors with the formula given above.
selection <- which(UNU_countries2$UNU_Key=="0308")
UNU_countries2[selection, "POM_Pieces"] <- (1.16 * UNU_countries2[selection, "POM_Pieces_0302"]) -
  UNU_countries2[selection, "POM_Pieces_0309"]

# Remove negatives that could arrise
selection <- which(UNU_countries2$UNU_Key=="0308" & UNU_countries2$POM_Pieces < 0)
UNU_countries2[selection, "POM_Pieces"] <- 0

UNU_countries2$POM_Pieces_0302 <- NULL
UNU_countries2$POM_Pieces_0309 <- NULL

# Merge average weight to calculate weight out of pieces
UNU_countries2 <- merge(UNU_countries2, htbl_Key_Weight, by=c("UNU_Key", "Year"), all.x = TRUE)

# Calculate weight (kg) out of the number of pieces
selection <- which(UNU_countries2$UNU_Key=="0308")
UNU_countries2[selection, "POM_kg"] <- UNU_countries2[selection, "POM_Pieces"] *
  UNU_countries2[selection, "AverageWeight"] 

# Calculate kpi and ppi again
UNU_countries2[selection, "kpi"] <- UNU_countries2[selection, "POM_kg"] /
  UNU_countries2[selection, "Inhabitants"] 
UNU_countries2[selection, "ppi"] <- UNU_countries2[selection, "POM_Pieces"] /
  UNU_countries2[selection, "Inhabitants"] 

# Set values of other UNU_Keys at NA
UNU_countries2[-selection, "kpi"] <- NA
UNU_countries2[-selection, "ppi"] <- NA

# Merge results with UNU_countries
UNU_countries2<- plyr::rename(UNU_countries2, c("kpi"="kpi_new", "ppi"="ppi_new"))
UNU_countries2 <- UNU_countries2[, c("UNU_Key", "Year", "Country", "kpi_new", "ppi_new")]

UNU_countries <- merge(UNU_countries, UNU_countries2, by=c("UNU_Key", "Country", "Year"), all.x = TRUE)

# Copy changes
selection <- which(!is.na(UNU_countries$kpi_new))
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_new"]
UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_new"]

# clean-up
UNU_countries$kpi_new <- NULL
UNU_countries$ppi_new <- NULL

rm(UNU_countries2, htbl_Key_Weight)
