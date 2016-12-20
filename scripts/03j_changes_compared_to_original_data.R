# ----------------------------------------------------------
# UNU_countries: Look for effect of changes compared with the original source data
# ----------------------------------------------------------

# Compare original values with the new adjusted data to see if there are very small changes.
# In those cases we keep the original values after all because the impact small so the original values
# seem to be right. 
# Diferences less than 0.05 kpi and less than 10 percent are considered very small here.


# ----------------------------------------------------------
# original_UNU_countries: Prepare datafile for merge
# ----------------------------------------------------------
original_UNU_countries <- rename(original_UNU_countries,
                                 c("POM_kg" = "POM_kg_original", "POM_pieces" = "POM_pieces_original"))


# ----------------------------------------------------------
# UNU_countries: Merge and calculated difference
# ----------------------------------------------------------
UNU_countries <- merge(UNU_countries, original_UNU_countries,  by=c("UNU_Key", "Year", "Country"),  all.x = TRUE)
rm(original_UNU_countries)

# Calculate kg and pieces per inhabitant for original data
UNU_countries$kpi_original = UNU_countries$POM_kg_original / UNU_countries$Inhabitants
UNU_countries$ppi_original = UNU_countries$POM_pieces_original / UNU_countries$Inhabitants

# Look for small differences based upon the kpi and ppi
selection <- abs(UNU_countries$kpi - UNU_countries$kpi_original) < 0.05 &
  abs(UNU_countries$kpi - UNU_countries$kpi_original) > 0 &
  abs(UNU_countries$kpi - UNU_countries$kpi_original) / UNU_countries$kpi_original < 0.10 &
  abs(UNU_countries$ppi - UNU_countries$ppi_original) < 0.05 &
  abs(UNU_countries$ppi - UNU_countries$ppi_original) / UNU_countries$ppi_original < 0.10

# Look for location of TRUE values  
selection <- which(selection)

# Undo the small alterations for the selection
UNU_countries[selection, "kpi"] <- UNU_countries[selection, "kpi_original"]
UNU_countries[selection, "ppi"] <- UNU_countries[selection, "ppi_original"]
UNU_countries[selection, "flag"] <- 0

# clean-up
UNU_countries$POM_kg_original <- NULL
UNU_countries$POM_pieces_original <- NULL
UNU_countries$kpi_original <- NULL
UNU_countries$ppi_original <- NULL

