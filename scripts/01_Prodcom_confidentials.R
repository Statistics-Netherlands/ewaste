# -----------------------------------------------------------------------------------------------------------
#
#   Name:           01Prodcom_confidentials.R
#
#   Description:    This script will read a CSV file with prodcom data that contains values that have been
#                   hidden because of confidentiallity.
#                   With help of data of International Trade exports and conversion tables an estimation
#                   is being made of the confidential values.
#
#                   A flag variable with information about the prodcom value will be generated
#                   with the following value labels:
#                     0 - "Original Prodcom value"
#                     1 - "Estimated with Export/Prodcom ratio of other years per country"
#                     2 - "Estimated with Export/Prodcom ratio of similar countries per year"
#                     3 - "Estimated with lineair model original values per Country and PCC"
#
#   Author:         V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------


setwd(DATA_PATH)

options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)

select_countries <- function(df) {
  return( df[which(df$Country %in% c(myoptions$countries, "EU28")),] )
}


# ----------------------------------------------------------
# tbl_data_pcc_conf: Read prodcom data with confidentiallity codes
# ----------------------------------------------------------
# The Prodcom datafile has been assembled in script "00a_Prepare_Prodcom_data.R"
tbl_data_pcc_conf <- read.csv("tbl_data_pcc_conf.csv", header = TRUE,
                               colClasses = c("character", "character", "character", "numeric", "numeric",
                                              "character", "character"))
tbl_data_pcc_conf <- select_countries(tbl_data_pcc_conf)

# ----------------------------------------------------------
# tbl_CN: Read CN (International Trade) data
# ----------------------------------------------------------
tbl_CN <- read.csv("tbl_CN.csv", header = TRUE, quote = "\"",
                    colClasses = c("character", "character", "character", "NULL", "NULL",
                                   "numeric", "numeric", "NULL", "NULL", "NULL"))

# Convert country codes to uppercase.
tbl_CN$Country <- toupper(tbl_CN$Country)

# select countries, which limits the analysis to what was specified with the --countries option of main.R.
tbl_CN <- select_countries(tbl_CN)

# ----------------------------------------------------------
# htbl_PCC_CN: Read Prodcom-CN link table
# ----------------------------------------------------------
htbl_PCC_CN <- read.csv("htbl_PCC_CN.csv", quote = "\"",
                       colClasses = c("character", "character", "character"))

# Every CN code has to exist only once a year. Therefore remove rows with multiple Prodcom codes per CN.
htbl_PCC_CN$Duplicate1 <-duplicated(htbl_PCC_CN[-1])
htbl_PCC_CN$Duplicate2 <-duplicated(htbl_PCC_CN[c(-1,-4)], fromLast = TRUE)
htbl_PCC_CN$CN_duplicates <- htbl_PCC_CN$Duplicate1 == TRUE | htbl_PCC_CN$Duplicate2 == TRUE
htbl_PCC_CN <- htbl_PCC_CN[!htbl_PCC_CN$CN_duplicates,1:3]


# ----------------------------------------------------------
# Stratum: Read table with country stratums
# ----------------------------------------------------------
Stratum <- read.csv("tbl_Countries.csv", quote = "\"",
                          colClasses = c("character", "NULL", "numeric", "NULL"))

# Convert country codes to uppercase.
Stratum$Country <- toupper(Stratum$Country)

# select countries, which limits the analysis to what was specified with the --countries option of main.R.
Stratum <- select_countries(Stratum)

# ----------------------------------------------------------
# tbl_CN: Match Prodcom data with CN data
# ----------------------------------------------------------
tbl_CN <- merge(tbl_CN, htbl_PCC_CN, by=c("Year", "CN"))
rm(htbl_PCC_CN)

# ----------------------------------------------------------
# tbl_CN: Aggregate CN data to Prodcom level
# ----------------------------------------------------------
tbl_CN_prodcom_aggr <- ddply( tbl_CN, c("Year", "PCC", "Country"), summarise,
                                Export_Quantity_Sup_sum = sum(Export_Quantity_Sup, na.rm=TRUE),
                                Export_Value_sum = sum(Export_Value, na.rm=TRUE) )

rm(tbl_CN)


# ----------------------------------------------------------
# tbl_CN_prodcom_aggr:  Estimate number of units when missing and value is available
# ----------------------------------------------------------

# First calculate sum of values and units for all years in which data is available.
selection <- tbl_CN_prodcom_aggr$Export_Quantity_Sup_sum > 0 & tbl_CN_prodcom_aggr$Export_Value_sum > 0
Export_totals_per_prodcom <- ddply (tbl_CN_prodcom_aggr[selection == TRUE, ], "PCC", summarise,
                                    Export_Quantity_Sup_sum = sum(Export_Quantity_Sup_sum, na.rm=TRUE),
                                    Export_Value_sum = sum(Export_Value_sum, na.rm=TRUE) )

# Calculate ratio between value and number of units.
Export_totals_per_prodcom$waarde_units_ratio <- 
  Export_totals_per_prodcom$Export_Value_sum / Export_totals_per_prodcom$Export_Quantity_Sup_sum 

Export_totals_per_prodcom$Export_Quantity_Sup_sum <- NULL
Export_totals_per_prodcom$Export_Value_sum <- NULL

# Attach this ratio to dataframe with all exports.
tbl_CN_prodcom_aggr <- merge(tbl_CN_prodcom_aggr, Export_totals_per_prodcom, all.x = TRUE)
rm(Export_totals_per_prodcom)

# Estimate missing number of units with the calculated ratio
selection <- which(tbl_CN_prodcom_aggr$Export_Quantity_Sup_sum == 0)

tbl_CN_prodcom_aggr[selection, "Export_Quantity_Sup_sum"] <- 
  (tbl_CN_prodcom_aggr[selection, "Export_Value_sum"] / tbl_CN_prodcom_aggr[selection, "waarde_units_ratio"])


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Manual clean-up
# ----------------------------------------------------------

# There are a few very unrealistic values for the EU28 aggregate.
# Because it is only for a few values there is just a manual correction here instead of an automatic procedure.
# Identification is based on surrounding years. 

tbl_data_pcc_conf[tbl_data_pcc_conf$Country=="EU28" &
                    tbl_data_pcc_conf$PCC=="26122000" &
                    tbl_data_pcc_conf$Year=="2013", "prodcom_units"] <- 4262

tbl_data_pcc_conf[tbl_data_pcc_conf$Country=="EU28" &
                    tbl_data_pcc_conf$PCC=="26203000" &
                    tbl_data_pcc_conf$Year=="2013", "prodcom_units"] <- 4262


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Attach CN data
# ----------------------------------------------------------
tbl_data_pcc_conf <- merge(tbl_data_pcc_conf, tbl_CN_prodcom_aggr, by = c("Country", "PCC", "Year"), all.x = TRUE)
rm (tbl_CN_prodcom_aggr)
tbl_data_pcc_conf$waarde_units_ratio <- NULL


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Calculate ratio between export and production for years in which both are known,
#                     and subsequently use this ratio to estimate confidential values
# ----------------------------------------------------------

# Calculate export, Prodcom ratio for unconfidential data
# First make selection of the unconfidential data which can be used for calculating the ratio
selection <- which ( tbl_data_pcc_conf$conf == 0 & tbl_data_pcc_conf$prodcom_units > 0 )
tbl_data_pcc_conf[selection, "Export_prodcom_ratio"] <- 
  tbl_data_pcc_conf[selection, "Export_Quantity_Sup_sum"] /
  tbl_data_pcc_conf[selection, "prodcom_units"]

# Calculate median per PCC, Country group.
tbl_data_pcc_conf_aggr <- ddply( tbl_data_pcc_conf, c("PCC", "Country"), summarise,
                                 Export_prodcom_ratio_med = median(Export_prodcom_ratio, na.rm=TRUE) )

# Attach it to tbl_data_pcc_conf
tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, tbl_data_pcc_conf_aggr, by = c("Country", "PCC"), all.x = TRUE)
rm (tbl_data_pcc_conf_aggr)

# Estimation of confidential values with the export/prodcom ratio
# First create flag variable
tbl_data_pcc_conf$flag <- 0

selection <- which( tbl_data_pcc_conf$conf == 1 & tbl_data_pcc_conf$Export_prodcom_ratio_med > 0 )

# Now the confidential values can be estimated with the export/prodcom ratio
tbl_data_pcc_conf[selection, "prodcom_units"] <- 
  tbl_data_pcc_conf[selection, "Export_Quantity_Sup_sum"] /
  tbl_data_pcc_conf[selection, "Export_prodcom_ratio_med"]
tbl_data_pcc_conf[selection, "flag"] <-  1

tbl_data_pcc_conf$Export_prodcom_ratio <- NULL
tbl_data_pcc_conf$Export_prodcom_ratio_med <- NULL


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Try to calculate remaining missing values by ratio similar countries.
# ----------------------------------------------------------

# Attach country stratum 
tbl_data_pcc_conf <- merge(tbl_data_pcc_conf, Stratum, by = "Country", all.x = TRUE)
rm (Stratum)

# Aggregate to stratum.
tbl_data_pcc_conf_stratum_aggr <- ddply( tbl_data_pcc_conf, c("PCC", "Stratum", "Year"), summarise,
                                         prodcom_units = sum(prodcom_units, na.rm=TRUE),
                                         Export_Quantity_Sup_sum = sum(Export_Quantity_Sup_sum, na.rm=TRUE) )

# Calculate ratio Export and Prodcom
selection <- which( tbl_data_pcc_conf_stratum_aggr$prodcom_units > 0 )
tbl_data_pcc_conf_stratum_aggr[selection, "Export_prodcom_ratio"] <- 
  tbl_data_pcc_conf_stratum_aggr[selection, "Export_Quantity_Sup_sum"] /
  tbl_data_pcc_conf_stratum_aggr[selection, "prodcom_units"]

# Only use existing stratum values
selection <- ifelse(tbl_data_pcc_conf_stratum_aggr$Stratum > 0, 1, 0)
tbl_data_pcc_conf_stratum_aggr<- tbl_data_pcc_conf_stratum_aggr[selection == 1, ]

# Aggregate once more to get the median for each PCC stratum group.
tbl_data_pcc_conf_stratum_aggr2 <- ddply( tbl_data_pcc_conf_stratum_aggr, c("PCC", "Stratum"), summarise,
                                          Export_prodcom_ratio_med = median(Export_prodcom_ratio, na.rm=TRUE) )

# Attach median to prodcom data
tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, tbl_data_pcc_conf_stratum_aggr2,
                            by = c("PCC", "Stratum"), all.x = TRUE )

rm(tbl_data_pcc_conf_stratum_aggr)
rm(tbl_data_pcc_conf_stratum_aggr2)

# Estimate remaining confidential values with the export/prodcom ratio of similar countries
selection <- which( tbl_data_pcc_conf$conf == 1 & is.na(tbl_data_pcc_conf$prodcom_units) &
                      tbl_data_pcc_conf$Export_prodcom_ratio_med > 0 )

tbl_data_pcc_conf[selection, "prodcom_units"] <- 
  tbl_data_pcc_conf[selection, "Export_Quantity_Sup_sum"] /
  tbl_data_pcc_conf[selection, "Export_prodcom_ratio_med"]
tbl_data_pcc_conf[selection, "flag"] <- 2

tbl_data_pcc_conf$Export_prodcom_ratio_med <- NULL


# ----------------------------------------------------------
#  tbl_data_pcc_conf: Fit prodcom data with the EU aggregate
# ----------------------------------------------------------

# Calculate sum of unconfidential prodcom units per prodcom code and year for all EU28 countries
# First create vector of the EU28 countries and add it to prodcom dataframe.
EU28_countries <- c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",
                   "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE")
dfEU28_countries <- data.frame(EU28_countries, Country = EU28_countries)

tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, dfEU28_countries, by = "Country", all.x = TRUE )
rm(EU28_countries)
rm(dfEU28_countries)

# Now EU28 countries with unconfidential data can be selected
selection <- which ( tbl_data_pcc_conf$conf == 0 & tbl_data_pcc_conf$Country == tbl_data_pcc_conf$EU28_countries )

# Aggregate selected data
tbl_data_pcc_conf_no_conf <- ddply( tbl_data_pcc_conf[selection, ], c("PCC", "Year"), summarise,
                                    prodcom_EU_sum_no_conf = sum(prodcom_units, na.rm=TRUE) )

# Attach this result to prodcom data
tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, tbl_data_pcc_conf_no_conf, by = c("PCC", "Year"), all.x = TRUE )
rm(tbl_data_pcc_conf_no_conf)

# Now aggregate the EU28 data itself.
selection <- which( tbl_data_pcc_conf$conf == 0 & tbl_data_pcc_conf$Country == "EU28" )
EU28 <- ddply( tbl_data_pcc_conf[selection, ], c("PCC", "Year"), summarise,
               prodcom_EU28_sum = sum(prodcom_units, na.rm=TRUE) )

# Attach this result to prodcom data
tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, EU28, by = c("PCC", "Year"), all.x = TRUE )
rm(EU28)

# Now calculate the sum of all estimated values so we can see how accurate the estimates are
selection <- which( tbl_data_pcc_conf$conf == 1 & tbl_data_pcc_conf$Country == tbl_data_pcc_conf$EU28_countries )
tbl_data_pcc_conf_conf <- ddply( tbl_data_pcc_conf[selection, ], c("PCC", "Year"), summarise,
                                 prodcom_EU_sum_conf = sum(prodcom_units, na.rm=TRUE) )

# Attach this result to prodcom data
tbl_data_pcc_conf <- merge( tbl_data_pcc_conf, tbl_data_pcc_conf_conf, by = c("PCC", "Year"), all.x = TRUE )
rm(tbl_data_pcc_conf_conf)

# Now fit the estimated data into the space between the EU28 aggregate and the unconfidential values
selection <- which( tbl_data_pcc_conf$conf == 1 &
                      (tbl_data_pcc_conf$prodcom_EU_sum_conf > 0 | tbl_data_pcc_conf$prodcom_EU_sum_conf < 0) &
                      tbl_data_pcc_conf$prodcom_EU28_sum > 0 &
                      tbl_data_pcc_conf$Country == tbl_data_pcc_conf$EU28_countries )

tbl_data_pcc_conf[selection, "prodcom_units"] <- 
  round(tbl_data_pcc_conf[selection, "prodcom_units"] /
          tbl_data_pcc_conf[selection, "prodcom_EU_sum_conf"] *
          (tbl_data_pcc_conf[selection, "prodcom_EU28_sum"] - tbl_data_pcc_conf[selection, "prodcom_EU_sum_no_conf"])
  ,0)

# In case we end up with negative prodcom_units, we just replace them with zero.
tbl_data_pcc_conf[which(tbl_data_pcc_conf$prodcom_units < 0), "prodcom_units"] <- 0

# When the estimated value is zero, nothing has actually been estimated. So we remove the flag.
tbl_data_pcc_conf$flag <- ifelse((tbl_data_pcc_conf$prodcom_units == 0), 0, tbl_data_pcc_conf$flag) 

tbl_data_pcc_conf$prodcom_EU_sum_no_conf <- NULL
tbl_data_pcc_conf$prodcom_EU28_sum <- NULL
tbl_data_pcc_conf$prodcom_EU_sum_conf <- NULL
tbl_data_pcc_conf$EU28_countries <- NULL
tbl_data_pcc_conf$Export_Quantity_Sup_sum <- NULL
tbl_data_pcc_conf$Export_Value_sum <- NULL
tbl_data_pcc_conf$Stratum <- NULL
tbl_data_pcc_conf$conf <- NULL



# ----------------------------------------------------------
#  tbl_data_pcc_conf: Replace estimated values that are deviating too much from trend from calculations 
# ----------------------------------------------------------
# This is similar to extrapolation approach 4 in script 03-i

# Calculate the regression parameters for the trend for each specific country for only KPI.
selection <- which ( tbl_data_pcc_conf$flag == 0 & tbl_data_pcc_conf$prodcom_units >= 0)
tbl_data_pcc_conf_selection <- tbl_data_pcc_conf[selection, ]

# Set an X before PCC, so they will be read as character after the melt process.
tbl_data_pcc_conf_selection$XPCC <- paste0("X", tbl_data_pcc_conf_selection$PCC)

# Make a variable for each PCC and Country combination.
tbl_data_pcc_conf_selection$PCC_Country <- paste(tbl_data_pcc_conf_selection$PCC, tbl_data_pcc_conf_selection$Country,
                                                 sep="_")

# Change Year so we start set the first year in the calculations as 1.
tbl_data_pcc_conf_selection$YearReformat <- as.numeric(tbl_data_pcc_conf_selection$Year) - 1994

models_per_country_prodcom_units <- by(tbl_data_pcc_conf_selection, tbl_data_pcc_conf_selection$PCC_Country, 
                             function(tbl_data_pcc_conf_selection) lm(tbl_data_pcc_conf_selection$prodcom_units ~ 
                                                                    tbl_data_pcc_conf_selection$YearReformat) )
models_per_country_prodcom_units <- sapply(models_per_country_prodcom_units, coefficients)

# Reshape
models_per_country_prodcom_units <- melt(models_per_country_prodcom_units)
models_per_country_prodcom_units <- dcast(models_per_country_prodcom_units, Var2 ~ Var1, value.var = "value")

models_per_country_prodcom_units <- plyr::rename(models_per_country_prodcom_units,c("Var2"="PCC_Country", "(Intercept)"="intercept", 
                                                                "tbl_data_pcc_conf_selection$YearReformat"="slope"))

# Add data from the last model per PCC and Country to tbl_data_pcc_conf
tbl_data_pcc_conf$PCC_Country <- paste(tbl_data_pcc_conf$PCC, tbl_data_pcc_conf$Country, sep="_")
tbl_data_pcc_conf <- merge(tbl_data_pcc_conf, models_per_country_prodcom_units, by="PCC_Country", all.x = TRUE)
tbl_data_pcc_conf$PCC_Country <- NULL

# Now extrapolations based on PCC by Year only can be calculated with the added model parameters.
# So hear Year has to be changed too so we start set the first year in the calculations as 1 and the LatestYear as 10
tbl_data_pcc_conf$YearReformat <- as.numeric(tbl_data_pcc_conf$Year) - 1994

# Calculate model outcome for every record.
tbl_data_pcc_conf$prodcom_units_model <- tbl_data_pcc_conf[, "YearReformat"] * tbl_data_pcc_conf[, "slope"] +
  tbl_data_pcc_conf[, "intercept"]

# Now check for the estimations done before, if they differ much from the model outcome.
# If so they will be replaced by the model outcome

# When difference is larger than 20% they will be changed.
maxdif <- 0.2

selection <- which( tbl_data_pcc_conf$flag > 0 & tbl_data_pcc_conf$prodcom_units_model > 0 &
                      (abs(tbl_data_pcc_conf$prodcom_units - tbl_data_pcc_conf$prodcom_units_model) /
                         tbl_data_pcc_conf$prodcom_units_model) > maxdif )

tbl_data_pcc_conf[selection, "prodcom_units"] <- tbl_data_pcc_conf[selection, "prodcom_units_model"]
tbl_data_pcc_conf[selection, "flag"] <- 3





# ----------------------------------------------------------
#  tbl_data_pcc_conf: Clean up and save result
# ----------------------------------------------------------

# clean-up
tbl_data_pcc_conf$intercept <- NULL
tbl_data_pcc_conf$slope <- NULL
tbl_data_pcc_conf$YearReformat <- NULL
tbl_data_pcc_conf$prodcom_units_model <- NULL
rm(tbl_data_pcc_conf_selection)
rm(models_per_country_prodcom_units)

# Sort order for columns
sortorder_c <- c(3, 2, 1, 4, 5, 6, 7)

# Sort dataframe rows by Country, Year and PCC.
sortorder <- order(tbl_data_pcc_conf$Country, tbl_data_pcc_conf$Year, tbl_data_pcc_conf$PCC)
tbl_data_pcc_conf <- tbl_data_pcc_conf[sortorder, sortorder_c]

write.csv(tbl_data_pcc_conf, file = "tbl_PCC.csv", quote = TRUE, row.names = FALSE)

