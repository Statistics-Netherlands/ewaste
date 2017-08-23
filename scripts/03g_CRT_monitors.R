# ----------------------------------------------------------
#  UNU_countries: Change values in UNU_Key 0308 (CRT monitors)
# ----------------------------------------------------------

# Data from UNU_Key 0308, (CRT monitors) looks unreliable.
# This syntax will change the data with the following formula:
# CRT_Mon[kg] = ((Desktop sales[pieces] * 116% ) - Flatpanels[pieces] ) * average_weight_CRT.
# According to the United Nations University, sales of CRT's are around 116% of desktop sales minus the flatpanels.

# ----------------------------------------------------------
# htbl_Key_Weight: Read table with average weights per UNU_Key
# ----------------------------------------------------------
htbl_Key_Weight <- read.csv("htbl_Key_Weight.csv", quote = "\"",
                            colClasses = c("character", "character", "numeric", "character"))


# ----------------------------------------------------------
# UNU_countries: Perform calculations
# ----------------------------------------------------------

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
# First merge the specific country average weights.

# If there are no specific countries, then not execute that merge.
selection <- which( htbl_Key_Weight$Country != "" )
if (length(selection) > 0){
  UNU_countries2 <- merge(UNU_countries2, htbl_Key_Weight[selection, ],
                          by=c("UNU_Key", "Country", "Year"), all.x = TRUE)
  UNU_countries2 <- plyr::rename(UNU_countries2, c("AverageWeight"="AverageWeight_Country"))

  # Remove those country specific values from htbl_Key_Weight
  htbl_Key_Weight <- htbl_Key_Weight[-selection, ]
}

# In case there are no country specific values, just create empty vector.
if (length(selection) == 0){
  UNU_countries2$AverageWeight_Country <- NA
}

# Now merge the general EU average weights.
UNU_countries2 <- merge(UNU_countries2, htbl_Key_Weight[, c("UNU_Key", "AverageWeight", "Year")],
                        by=c("UNU_Key", "Year"), all.x = TRUE)

# Overwrite general EU weights in case there country specific weights.
selection <- which( !is.na(UNU_countries2$AverageWeight_Country) )
if (length(selection) > 0){
  UNU_countries2[selection, "AverageWeight"] <- UNU_countries2[selection, "AverageWeight_Country"]
}

UNU_countries2$AverageWeight_Country <- NULL


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

# After 2008-2010 (depending on stratum) there are no CRT sales so all should be zero
selection1 <- which( (UNU_countries$UNU_Key=="0308") & (as.numeric(UNU_countries$Year) >= 2008) 
                    & UNU_countries$Stratum == 1) 
selection2 <- which( (UNU_countries$UNU_Key=="0308") & (as.numeric(UNU_countries$Year) >= 2009) 
                     & UNU_countries$Stratum == 2) 
selection3 <- which( (UNU_countries$UNU_Key=="0308") & (as.numeric(UNU_countries$Year) >= 2010) 
                     & UNU_countries$Stratum == 3) 
selection <- c(selection1, selection2, selection3)

rm(selection1)
rm(selection2)
rm(selection3)

UNU_countries[selection, "kpi"] <- 0
UNU_countries[selection, "ppi"] <- 0

# clean-up
UNU_countries$kpi_new <- NULL
UNU_countries$ppi_new <- NULL

rm(UNU_countries2, htbl_Key_Weight)
