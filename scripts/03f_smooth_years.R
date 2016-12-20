# ----------------------------------------------------------
#  UNU_countries: Eliminate big jumps from one year to the next
# ----------------------------------------------------------

# Description: This script will eliminate big changes from year to year of values per UNU_Key and Country.
#
# Two calculations are being done.
#
# Method 1 will flag values in year t as unreliable when t differs more than 50% with t-1 and
# t-1 and t+1 are within 20% of each other. This to make sure that t is the outlier and not t-1.
#
# Method 2 is for identifying strange jumps.
# Low side: cases when year t is lower than 20% below the minimum of year t-1 and t+1 and
# more than 40% lower than the maxium of those t-1 and t+1.
# High side:
# Same things happens when t is higher than 20% above the maxium of t-1 and t+1 and
# more than 40% higher than the minimum of t-1 and t+1.
# Exception when t+2 is within 20%  of the value of t. And also if t+2 is bigger than t for calculations on the
# low side and when t+2 is lower than t for calulations on the high side.
# In these cases the method 2 calculation will not be done because t+1 might be the outlier.

# The calculations are only done for the kpi. When they are considered an outlier the same goes for the 
# corresponding ppi value.


# ----------------------------------------------------------
#  UNU_countries: Calculation preparations
# ----------------------------------------------------------

# Look for first and last year in dataset
year_first <- min(as.integer(UNU_countries$Year))
year_last <- max(as.integer(UNU_countries$Year))

# Create new kpi to track changes caused by the calculations
UNU_countries$kpi2 <- UNU_countries$kpi

# First create a few extra variables containing the data of the surrounding years.
# year t-1
UNU_countries$t_min1 <- c(UNU_countries[2:nrow(UNU_countries), "kpi"], NA)

selection <- which(UNU_countries$Year==year_first)
UNU_countries[selection, "t_min1"] <- NA

# year t+1
UNU_countries$t_plus1 <- c(NA, UNU_countries[1:nrow(UNU_countries)-1, "kpi"])

selection <- which(UNU_countries$Year==year_last)
UNU_countries[selection, "t_plus1"] <- NA

# year t+2
UNU_countries$t_plus2 <- c(NA, UNU_countries[1:nrow(UNU_countries)-1, "t_plus1"])

selection <- which(UNU_countries$Year==year_last)
UNU_countries[selection, "t_plus2"] <- NA



# ----------------------------------------------------------
# UNU_countries: Method 1 calculations:
# ----------------------------------------------------------
selection <- which( UNU_countries$t_min1 > 0 & !is.na(UNU_countries$kpi2) &
  abs((UNU_countries$t_plus1 - UNU_countries$t_min1) / UNU_countries$t_min1) < 0.2 &
  abs((UNU_countries$kpi2 - UNU_countries$t_min1) / UNU_countries$t_min1) > 0.5 )

UNU_countries[selection, "kpi2"] <- NA

# ----------------------------------------------------------
# UNU_countries: Method 2 calculations:
# ----------------------------------------------------------

# For the year before the last it is not posible to use t+2 values.
# In those cases we use just the t+1 values by copying the t+1 value into t+2.
UNU_countries[UNU_countries$Year==year_last-1, "t_plus2"] <- 
  UNU_countries[UNU_countries$Year==year_last-1, "t_plus1"]

# too low values
selection_low <- which( UNU_countries$t_min1 > 0 & !is.na(UNU_countries$kpi2) &
                          UNU_countries$t_plus1 > 0 & UNU_countries$t_plus2 > 0 &
                          abs(UNU_countries$kpi2 - pmin(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE)) /
                            pmin(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE) > 0.2 &
                          abs(UNU_countries$kpi2 - pmax(UNU_countries$t_min1, UNU_countries$t_plus2, na.rm=FALSE)) /
                            pmax(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE) > 0.4 &
                          UNU_countries$kpi2 < UNU_countries$t_min1 &  UNU_countries$kpi2 < UNU_countries$t_plus1 &
                          (abs(UNU_countries$t_plus2 - UNU_countries$kpi2) / UNU_countries$kpi2 < 0.2 |
                             UNU_countries$t_plus2 > UNU_countries$kpi2 ) )

UNU_countries[selection_low, "kpi2"] <- NA

# too high values
selection_high <- which( UNU_countries$t_min1 > 0 & !is.na(UNU_countries$kpi2) &
                          UNU_countries$t_plus1 > 0 & UNU_countries$t_plus2 > 0 &
                           abs(UNU_countries$kpi2 - pmax(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE)) /
                            pmax(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE) > 0.2 &
                           abs(UNU_countries$kpi2 - pmin(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE)) /
                            pmin(UNU_countries$t_min1, UNU_countries$t_plus1, na.rm=FALSE) > 0.4 &
                           UNU_countries$kpi2 > UNU_countries$t_min1 &  UNU_countries$kpi2 > UNU_countries$t_plus1 &
                           (abs(UNU_countries$t_plus2 - UNU_countries$kpi2) / UNU_countries$kpi2 < 0.2 | 
                              UNU_countries$t_plus2 < UNU_countries$kpi2 ) )
                             
UNU_countries[selection_high, "kpi2"] <- NA



# At this point there still can be outliers in the last available year.
# Those values will now be removed when they differ more than 50% with the previous year.
selection <- which( UNU_countries$Year == year_last & UNU_countries$kpi2 >= UNU_countries$t_min1 * 1.5 )

# Remove the outliers
UNU_countries[selection, "kpi2"] <- NA


# Copy changes and alter flag variable. We don't copy if the flag variable shows that it is manually added data.
selection <- which( !is.na(UNU_countries$kpi) & is.na(UNU_countries$kpi2) & (UNU_countries$flag < 50) )
UNU_countries[selection, "flag"] <- UNU_countries[selection, "flag"] + 100
UNU_countries[selection, "kpi"] <- NA
UNU_countries[selection, "ppi"] <- NA

# clean-up
UNU_countries$kpi2 <- NULL
UNU_countries$t_min1 <-NULL
UNU_countries$t_plus1 <-NULL
UNU_countries$t_plus2 <-NULL

rm(selection, selection_low, selection_high, year_first, year_last)
