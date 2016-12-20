# ----------------------------------------------------------
# Estimate missing values based on averages of surrounding years
# ----------------------------------------------------------



# ----------------------------------------------------------
# mydf: Calculation preparations
# ----------------------------------------------------------

# Look for first and last year in dataset
year_first <- min(as.integer(mydf$Year))
year_last <- max(as.integer(mydf$Year))

# Sort data
sortorder <- order(mydf$catA, mydf$catB, -rank(mydf$Year))
mydf <- mydf[sortorder, ]

# Create new kpi and ppi to track changes caused by the calculations
mydf$kpi2 <- mydf$kpi
mydf$ppi2 <- mydf$ppi

# Create a few extra variables containing the data of the surrounding years.
# KPI
mydf$kpi_t_min1 <- c(mydf[2:nrow(mydf), "kpi"], NA)             # year t-1
mydf$kpi_t_min2 <- c(mydf[2:nrow(mydf), "kpi_t_min1"], NA)      # year t-2
mydf$kpi_t_plus1 <- c(NA, mydf[1:nrow(mydf)-1, "kpi"])          # year t+1
mydf$kpi_t_plus2 <- c(NA, mydf[1:nrow(mydf)-1, "kpi_t_plus1"])  # year t+2

# PPI
mydf$ppi_t_min1 <- c(mydf[2:nrow(mydf), "ppi"], NA)
mydf$ppi_t_min2 <- c(mydf[2:nrow(mydf), "ppi_t_min1"], NA)
mydf$ppi_t_plus1 <- c(NA, mydf[1:nrow(mydf)-1, "ppi"])
mydf$ppi_t_plus2 <- c(NA, mydf[1:nrow(mydf)-1, "ppi_t_plus1"])

# Correction of some values:
# year t-1
selection <- which(mydf$Year == year_first)   
mydf[selection, "kpi_t_min1"] <- NA
mydf[selection, "ppi_t_min1"] <- NA

# year t-2
selection <- which(mydf$Year == year_first)
mydf[selection, "kpi_t_min2"] <- NA
mydf[selection, "ppi_t_min2"] <- NA

# year t+1
selection <- which(mydf$Year==year_last)
mydf[selection, "kpi_t_plus1"] <- NA
mydf[selection, "ppi_t_plus1"] <- NA

# year t+2
selection <- which(mydf$Year==year_last)
mydf[selection, "kpi_t_plus2"] <- NA
mydf[selection, "ppi_t_plus2"] <- NA




# ----------------------------------------------------------
# mydf: Calculations
# ----------------------------------------------------------

## First with the kpi variable

# Estimate missing of kpi variables when one year is missing and year before and after are known
selection <- which( is.na(mydf$kpi2) & !is.na(mydf$kpi_t_min1) & !is.na(mydf$kpi_t_plus1) )
mydf[selection, "kpi2"] <- (mydf[selection, "kpi_t_min1"] + mydf[selection, "kpi_t_plus1"]) / 2

# Estimate missing of kpi variables when 2 years consecutive years are missing and year before and after are known
selection <- which( is.na(mydf$kpi2) & is.na(mydf$kpi_t_plus1) & !is.na(mydf$kpi_t_min1) & !is.na(mydf$kpi_t_plus2) )
mydf[selection, "kpi2"]  <- (2 * mydf[selection, "kpi_t_min1"] + mydf[selection, "kpi_t_plus2"]) / 3
mydf[selection - 1, "kpi2"] <- (2 * mydf[selection, "kpi_t_plus2"] + mydf[selection, "kpi_t_min1"]) / 3

# When the first available year is missing, this is estimated using the tendency of the following two years.
selection <- which( mydf$Year == year_first & is.na(mydf$kpi2) & !is.na(mydf$kpi_t_plus1) & !is.na(mydf$kpi_t_plus2) )
mydf[selection, "kpi2"] <- 2 * mydf[selection, "kpi_t_plus1"] - mydf[selection, "kpi_t_plus2"]

# When the last available year is missing, this is estimated using the tendency of the previous two years.
selection <- which( mydf$Year == year_last & is.na(mydf$kpi2) & !is.na(mydf$kpi_t_min1) & !is.na(mydf$kpi_t_min2) )
mydf[selection, "kpi2"] <- 2 * mydf[selection, "kpi_t_min1"] - mydf[selection, "kpi_t_min2"]


##  Now the same with the ppi variable

# Estimate missing of ppi variables when one year is missing and year before and after are known
selection <- which( is.na(mydf$ppi2) & !is.na(mydf$ppi_t_min1) & !is.na(mydf$ppi_t_plus1) )
mydf[selection, "ppi2"] <- (mydf[selection, "ppi_t_min1"] + mydf[selection, "ppi_t_plus1"]) / 2

# Estimate missing of ppi variables when 2 years consecutive years are missing and year before and after are known
selection <- which( is.na(mydf$ppi2) & is.na(mydf$ppi_t_plus1) & !is.na(mydf$ppi_t_min1) & !is.na(mydf$ppi_t_plus2) )
mydf[selection, "ppi2"]  <- (2 * mydf[selection, "ppi_t_min1"] + mydf[selection, "ppi_t_plus2"]) / 3
mydf[selection - 1, "ppi2"] <- (2 * mydf[selection, "ppi_t_plus2"] + mydf[selection, "ppi_t_min1"]) / 3

# When the first available year is missing, this is estimated using the tendency of the following two years.
selection <- which( mydf$Year == year_first & is.na(mydf$kpi2) & !is.na(mydf$ppi_t_plus1) & !is.na(mydf$ppi_t_plus2) )
mydf[selection, "ppi2"] <- 2 * mydf[selection, "ppi_t_plus1"] - mydf[selection, "ppi_t_plus2"]

# When the last available year is missing, this is estimated using the tendency of the previous two years.
selection <- which( mydf$Year == year_last & is.na(mydf$kpi2) & !is.na(mydf$ppi_t_min1) & !is.na(mydf$ppi_t_min2) )
mydf[selection, "ppi2"] <- 2 * mydf[selection, "ppi_t_min1"] - mydf[selection, "ppi_t_min2"]



# In case negative values occur after these calculations we change them to zero.
selection <- which( mydf$kpi2 < 0 )
mydf[selection, "kpi2"] <- 0
selection <- which( mydf$ppi2 < 0 )
mydf[selection, "ppi2"] <- 0

# Copy changes and alter flag variable
selection <- which( is.na(mydf$kpi) & !is.na(mydf$kpi2) )
selection_f <- which( is.na(mydf$kpi) & !is.na(mydf$kpi2) & mydf$flag < 100 )
mydf[selection, "kpi"] <- mydf[selection, "kpi2"]
mydf[selection, "ppi"] <- mydf[selection, "ppi2"]

# If the flag is already above 100 there is no need to change the value anymore. 
mydf[selection_f, "flag"] <- mydf[selection_f, "flag"] + 10  

# clean-up
mydf$kpi2 <- NULL
mydf$ppi2 <- NULL
mydf$kpi_t_min1 <- NULL
mydf$kpi_t_min2 <- NULL
mydf$kpi_t_plus1 <- NULL
mydf$kpi_t_plus2 <- NULL
mydf$ppi_t_min1 <- NULL
mydf$ppi_t_min2 <- NULL
mydf$ppi_t_plus1 <- NULL
mydf$ppi_t_plus2 <- NULL

rm(year_first, year_last, selection_f)
