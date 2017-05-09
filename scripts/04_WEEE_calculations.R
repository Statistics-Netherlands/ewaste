# -----------------------------------------------------------------------------------------------------------
#
#   Name:             04_WEEE_calculations.R
#
#   Description:      This script reads the CSV file with the altered POM data.
#                     Then the WEEE arrising will be calculated using the Weibull function.
#
#
#   Authors:          dr C.P. Balde - Statistics Netherlands,
#                     H. Meeuwissen - Statistics Netherlands,  
#                     dr. F. Wang - United Nations University
#
#   Revised version:  V.M. van Straalen - Statistics Netherlands
#
# -----------------------------------------------------------------------------------------------------------

setwd(DATA_PATH)
options(stringsAsFactors=FALSE, warn=0, scipen=999, digits=4)

require(plyr)
require(reshape2)



# ----------------------------------------------------------
# tbl_POM: Read POM data
# ----------------------------------------------------------
tbl_POM <- read.csv("tbl_POM.csv", quote = "\"",
                    colClasses = c("numeric", "character", "character", "character", "numeric", "numeric",
                                   "numeric", "numeric", "numeric", "numeric"))


# ----------------------------------------------------------
# tbl_Weibull_parameters: Read scale and shape parameters for Weibull function
# ----------------------------------------------------------
tbl_Weibull_parameters <- read.csv("tbl_data_tool.csv", quote = "\"",
                                   colClasses = c("character", "character", "character", "numeric", "NULL",
                                                  "character", "NULL"))

tbl_Weibull_parameters <- plyr::rename(tbl_Weibull_parameters,c("Destination"="Parameter"))

# Convert country codes to uppercase.
tbl_Weibull_parameters$Country <- toupper(tbl_Weibull_parameters$Country)

# Select data on shapes
selection <- which(toupper(tbl_Weibull_parameters$Parameter) == "SHAPEPARAMETER")
shapedata <- tbl_Weibull_parameters[selection, ]

shapedata <- plyr::rename(shapedata,c("Value"="shape"))
shapedata$Parameter <- NULL


# Select data on scales
selection <- which(toupper(tbl_Weibull_parameters$Parameter) == "SCALEPARAMETER")
scaledata <- tbl_Weibull_parameters[selection, ]

scaledata <- plyr::rename(scaledata,c("Value"="scale"))
scaledata$Parameter <- NULL

# Rebuild tbl_Weibull_parameters with the scale and shape data.
tbl_Weibull_parameters <- merge(scaledata, shapedata,  by=c("UNU_Key", "Country", "Year"),  all = TRUE)
rm(scaledata, shapedata)


# ----------------------------------------------------------
# tbl_POM: Merge Weibull parameters with POM data
# ----------------------------------------------------------
tbl_POM <- merge(tbl_POM, tbl_Weibull_parameters,  by=c("UNU_Key", "Country", "Year"),  all.x = TRUE)


# ----------------------------------------------------------
# tbl_POM: Add variables for WEEE calculation per year
# ----------------------------------------------------------

# Create columns for all years in Weibull range
year_first <- min(as.integer(tbl_POM$Year))
year_last <- max(as.integer(tbl_POM$Year)) + 7

years <- c(year_first:year_last)
empty <- as.data.frame(matrix(NA, ncol = length(years), nrow = nrow(tbl_POM)))
colnames(empty) <- years

# Add them to tbl_POM dataset
tbl_POM <- cbind(tbl_POM, empty)
rm(empty)

  
# ----------------------------------------------------------
# tbl_POM: Perform Weibull function
# ----------------------------------------------------------

for (j in year_first:year_last){
  tbl_POM$WEEE_POM_dif <- j - ( as.integer(tbl_POM[, "Year"]) )
  wb <- dweibull(tbl_POM[(tbl_POM$WEEE_POM_dif >= 0),"WEEE_POM_dif"] + 0.5,
                  shape = tbl_POM[(tbl_POM$WEEE_POM_dif >= 0), "shape"],
                  scale = tbl_POM[(tbl_POM$WEEE_POM_dif >= 0), "scale"],
                  log = FALSE)
  weee <-  wb * tbl_POM[(tbl_POM$WEEE_POM_dif >= 0), "POM_t"]
  tbl_POM[(tbl_POM$WEEE_POM_dif >= 0), as.character(j)] <- weee
}  
  
# Clean-up
tbl_POM$WEEE_POM_dif <- NULL
tbl_POM$scale <- NULL
tbl_POM$shape <- NULL
rm(tbl_Weibull_parameters)
   
# ----------------------------------------------------------
# tbl_POM: Summing the waste arrising from all past years
# ----------------------------------------------------------

# Calculate WEEE-generated for all years as the sum of all past years per UNU_Key and Country

# First melt all years into long form. Other variables exclusing the years and the classifications are removed.
mylong <- melt(tbl_POM[-(3:10)], id = c("UNU_Key", "Country"))

# Remove empty rows to reduce memory burden
mylong <- mylong[!is.na(mylong$value), ]
rm(tbl_POM)

# Then cast them into wide form while calculating the sum of every group
mywide <- dcast(mylong, UNU_Key + Country ~ variable, sum, na.rm=TRUE)

# Finally melt them again to long form for merge with POM dataset.
dfWEEE_gen <- melt(mywide, id = c("UNU_Key", "Country"))

names(dfWEEE_gen)[3] <- "Year"
names(dfWEEE_gen)[4] <- "WEEE_t"

# Save data into tbl_POM.csv
write.csv(dfWEEE_gen, file = "tbl_WEEE.csv", quote = TRUE, row.names = FALSE)

