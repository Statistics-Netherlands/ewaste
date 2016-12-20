# ----------------------------------------------------------
#  UNU_countries: Calculate stratum means for estimation of missing values based on stratum means
# ----------------------------------------------------------

# Only select values that were not marked as outliers
selection <- UNU_countries$flag==0 & !is.na(UNU_countries$kpi) & !is.na(UNU_countries$Inhabitants)

# Calculate total weight and number of pieces
UNU_countries[selection==TRUE, "POM_kg"] <- UNU_countries[selection==TRUE, "kpi"] *
  UNU_countries[selection==TRUE, "Inhabitants"]
UNU_countries[selection==TRUE, "POM_Pieces"] <- UNU_countries[selection==TRUE, "ppi"] *
  UNU_countries[selection==TRUE, "Inhabitants"]

strat_tot <- ddply(UNU_countries[selection==TRUE, ], c("UNU_Key", "Year", "Stratum"), summarise,
                   POM_kg_sum = sum(POM_kg, na.rm=TRUE),
                   POM_Pieces_sum = sum(POM_Pieces, na.rm=TRUE),
                   Inhabitants_sum = sum(Inhabitants, na.rm=TRUE))

strat_tot$flag <- 1

UNU_countries$POM_kg <- NULL
UNU_countries$POM_Pieces <- NULL

# Calculate stratum_means
strat_tot$kpi_stratum <- strat_tot$POM_kg_sum / strat_tot$Inhabitants_sum
strat_tot$ppi_stratum <- strat_tot$POM_Pieces_sum / strat_tot$Inhabitants_sum

strat_tot$POM_kg_sum <- NULL
strat_tot$POM_Pieces_sum <- NULL
strat_tot$Inhabitants_sum <- NULL

# In the next step missing stratum values will be estimated.
# After that they will be merged with the UNU_countries data.