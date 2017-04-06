---
title: "Waste over Time"
author: "V.M. van Straalen"
date: "April 6, 2017"
output: html_document
---

![Statistics Netherlands](https://github.com/Statistics-Netherlands/ewaste/blob/master/images/cbs_logo_en.png)


# Purpose
The Waste over Time (or WOT) script is a script developed by Statistics Netherlands. It uses European production and trade statistics with the 'apparent consumption method' to estimate sales in weight of products. The script also comes with data on life times of electronic and electric equipment, and allows to calculate the urban mine and to forecast waste generated. 

Results from the calculations can be viewed [here](https://statistics-netherlands.shinyapps.io/sales_and_waste/): <BR>
[![screenshot](https://github.com/Statistics-Netherlands/ewaste/blob/master/images/screenshot.png)](https://statistics-netherlands.shinyapps.io/sales_and_waste/)


The script here available at the GitHub shows the calculations to produce this output.

The script can be used for: 

**WEEE-Directive:**

* The methodology in the script follows the so-called 'common methodology' to determine Placed on the Market and WEEE Generated as defined in [article 7 of the EU-WEEE Directive](http://eur-lex.europa.eu/legal-content/NL/TXT/?uri=uriserv:OJ.L_.2012.197.01.0038.01.NLD).
For more information regarding the WEEE-Directive: [WEEE-Directive](http://ec.europa.eu/environment/waste/weee/index_en.htm)
* Governmental authorities of the EU Member States, inspection agencies and WEEE-registers can use the script to obtain an alternative to equipment placed on the market, and forecast waste streams.


**Circular Economy**

* The script has been used in the [H2020 ProSUM Project](http://www.prosumproject.eu/).
* The script allows to study Material Flows and Urban Mining by relating sales, use phase, and forecasts of waste streams. It can be easily extended to other materials. 


The WOT script contains all relevant source data to perform the calculations for each EU-28 Member State for 54 types of electronical and electrical equipment.  The statistical source data comes from Eurostat. The other parameters come from a results of a project of DG Environment in which Statistics Netherlands participated. [Art7_publication](http://ec.europa.eu/environment/waste/weee/pdf/Final_Report_Art7_publication.pdf). 

The script has been designed that it can be easily extended to other types of products. The script has been written in [R](https://cran.r-project.org).

<br>
<br>
<br>

# Installation
**R**

No R knowledge is necessary to run this series of scripts. But R needs to be installed on your computer. For more information on how to install R see [R Installation and Administration](https://cran.r-project.org/doc/manuals/r-release/R-admin.html)

In case you wish to see and understand all the calculations, knowledge of the R language is of course needed.

<br>

**Wast over Time (WOT) script**

From the GitHub e-waste repository (you are there now when reading this file) click on the green button "Clone or download". Click download ZIP. Save file and unpack. Go to the folder .\ewaste-master\scripts. Copy that location to the clipboard. 

Now open de command line (Windows: cmd.exe, aka 'DOS box') by typing cmd into the windows search box and pressing enter.
Here type the command SETX EWASTE_SCRIPT_PATH "[FOLDER SCRIPT PATH]" and enter the location of the folder with the scripts between the "". Right-click and choose paste, to paste this location that was copied to the clipboard before. The command would then look for instance like this: SETX SETX EWASTE_SCRIPT_PATH "F:\Vincent\Mijn documenten\ewaste-master\scripts".

![Set script path](https://github.com/Statistics-Netherlands/ewaste/blob/master/images/set_script_path.png)

Now the DOS box can be closed. In case R was open, please close and re-open it. 

<br>

**R packages**

You need to install the following packages (sets of extra R functions) to be able to run the scripts. To do this open R and copy all of the next R commands - one at a time - into the R Console and press ENTER. If you are asked to select a CRAN mirror, select one of your choice and continue.

```r
install.packages("tcltk")
install.packages("getopt")
install.packages("plyr")
install.packages("reshape2")
install.packages("RODBC")
install.packages("data.table")
```

<br>
<br>
<br>

## Usage
The program can be run from the command line (Windows: cmd.exe, aka 'DOS box').

To run the calculations for all countries, go to the folder of the scrips. Do this by using the CD command. For instance 'cd /d F:\Vincent\Mijn documenten\ewaste-master\scripts'.
Then type the following text in the command editor:  
`Rscript main.R`

Alternatively the above two commands could be performed at once with the command 'Rscript %EWASTE_SCRIPT_PATH%\main.R'

To run the calculations just for one or more specific countries use:
`Rscript main.R --countries=nld;bel`. This will not perform calculations to correct outliers based on values from similar countries.


The above commands assume that the location where `Rscript.exe` lives is on the search path (the `PATH` environment variable).
If this is not the case (you will notice because you get a message saying "'Rscript' is not recognized as an internal or external command, operable program or batch file."), you will need to provide the full path to `Rscript.exe`, e.g. for R version 3.3.1 under MS Windows this would typically be:

* 32 bit computer:
C:\Program Files\R\R-3.3.1\bin\i386

* 64 bit computer:
C:\Program Files\R\R-3.3.1\bin\x64

For instance type '"C:\Program Files\R\R-3.3.1\bin\x64\Rscript.exe" main.R' (the path itself has to be between double-quotes because of the space in the name of the Program Files folder).
Then it looks like this:

![Start Rscript](https://github.com/Statistics-Netherlands/ewaste/blob/master/images/start_rscript.png)

<br>
In case you are comfortable with the R language you can of course also run the script from R. Executing the script main.R will give you the the calculations for all countries.
<br>
<br>
<br>

# Source file listing

* 00a_Prepare_Prodcom_data.R - Create Prodcom data file
* 00b_Prepare_International_Trade_data.R - Create international trade data file
* 01_Prodcom_confidentials.R - Estimation of Prodcom confidential values
* 02_Apparent_Consumption_Method.R - Convert Prodcom data and International trade data to weight (kg) with the use of average weights data
* 03_POM_calculations.R - remove outliers and make estimates for missing POM data
* 03a_remove_year_outliers.R - Outlier detection using Median Absolute Deviation (MAD)
* 03b_remove_stratum_outliers.R - Remove unreliable values (extremes) based on values of other years per Country and UNU_Key
* 03c_estimate_missings_using_years.R - Estimate missing values based on averages of surrounding years
* 03d_calculate_stratum_means.R - Calculate stratum means for estimation of missing values based on stratum means
* 03e_estimate_missings_using_stratums.R - Estimate missing values based on stratum ratios
* 03f_smooth_years.R - Eliminate big jumps from one year to the next
* 03g_CRT_monitors.R - Change values in UNU_Key 0308 (CRT monitors)
* 03h_add_PV_panel_data.R - Add Eurostat PV panel data (UNU_Key 0002) to use instead of the statistics
* 03i_extend_time_series.R - Extend time series back to 1980 and a few years into the future
* 03j_changes_compared_to_original_data.R - Look for effect of changes compared with the original source data
* 04_WEEE_calculations.R - Calculates the WEEE arising using the Weibull function.
* 05_Make_tblAnalysis.R - Aggregates POM and WEEE data, calculates stock and produces output files.
* 05a_calculate_aggregates.R - Script that actually performs the aggregates for POM, WEEE and stock data.

<br>
<br>
<br>

# Output
The program creates a few data files that can be found in the '.\ewaste-master\data' folder.

The first two are also included in this set of scripts so they can be viewed directly:
* tbl_POM.csv - This contains the Put-On-Market results. No aggregates
* tbl_WEEE.csv - This contains the WEEE generated. No aggregates

These two CSV files are USA/UK CSV files (where the decimal separator is a period/full stop and the value separator is a comma)

After running the program you can also find the following data files.
* tbl_ANALYSIS.RData - This file can only be accessed with R and contains a list of 4 dataframes:
  1. POM data
  2. WEEE generated data
  3. Stock data
  4. Eurostat WEEE directive reference data
* tbl_POM_all.csv - This contains the Put-On-Market results including aggregates.
* tbl_WEEE_all.csv - This contains the WEEE generated including aggregates.
* tbl_Stock_all.csv - This contains the WEEE generated including aggregates.

The three CSV files are European formatted CSV files (where the decimal separator is a comma and the value separator is a semicolon). They contain the same data as the first 3 dataframes in the RData file and can be easily used to import in other programs.

<br>
<br>
<br>

# Description of the program components
This series of scripts uses prodcom and international trade data. It calculates the apparent consumption and then estimates missing values and corrects outliers.
When running the script without any alterations, all script files will be executed except for scripta '00a_Prepare_Prodcom_data.R' and '00b_Prepare_International_Trade_data.R - Create international trade data file'.

<br>

## Preparing source data sets of prodcom and international trade
The script uses two main input data files for the calculations:
* tbl_data_pcc_conf.csv - for the prodcom data
* tbl_CN.csv - for the international trade data

The script starts with those files. To run the script with other data those files have to be altered as long as the structure remains the same. The two files have been generated by the scripts '00a_Prepare_Prodcom_data.R' and '00b_Prepare_International_Trade_data.R - Create international trade data file'. Those scripts are not automatically executed, so the scripts will generated results based on the two files provided here with the scripts.

Script '00a_Prepare_Prodcom_data.R - Create Prodcom data file'downloads the prodcom datafiles from the Eurostat website. Data for 1995-2015 will be automatically downloaded and saved in the right location. When data for later years becomes available, the new files have to be added in the script or they have to be manually downloaded and placed in the '..\ewaste-master\data' folder. The output of this script is 'tbl_data_pcc_conf.csv'. In some cases the script can result in errors because of conflicts with 64 bits Excel divers for reading Excel files. In anyway it shows the calculations that lead from the source data to the 'tbl_data_pcc_conf.csv' datafile.

Script "00b_Prepare_International_Trade_data.R - Create international trade data file" shows how to download the international trade data. It is not performed automatically. This International Trade data is very large and has to be downloaded manually because you have to register at the Eurostat Comext website. The resulting datafile from this script is called 'tbl_CN' and is included with this program. It contains data from 1995-2015. When new data is needed it has to be downloaded manually and script '00b_Prepare_International_Trade_data.R' has to be executed to generate a new version of 'tbl_CN'. A detailed instruction to download these source data files is given in Annex 1.


<br>

## Estimation of confidential prodcom data.
Prodcom data contains many hidden values because of confidentiality. These confidential values are being estimated in script '01_Prodcom_confidentials.R'. 
Input data or this script is:
* 'tbl_data_pcc_conf.csv'
* 'tbl_CN'

The estimation of the confidential Prodcom record are carried out in the following way:

#### Step 1:
Calculate ratio between units exported and units produced in case both values are not confidential for every Prodcom code, country and year. Calculate for every Prodcom code and country the median of the calculated ratios (all the years).
Use this median ratio to estimate confidential prodcom units: export units / ratio = prodcom units.

#### Step 2:
Same, but this time countries are grouped in 3 groups based on purchasing power. Values that could not have been estimated with step 1, are now being estimated based on the median ratio of the aggregated values of similar countries. 

#### Finalize:
Estimated values are checked on reliability using the EU27 aggregate. In case the sum of the estimated values is larger than the difference between the EU27 aggregate and the sum of the non-confidential values for the EU 27 countries, the estimated values are scaled downwards proportionally.

The result of this script is 'tbl_PCC.csv'. It contains a flag variable telling where the prodcom values come from:
* Flag 0 - "Original Prodcom value"
* Flag 1 - "Estimated with Export/Prodcom ratio of other years per country"
* Flag 2 - "Estimated with Export/Prodcom ratio of similar countries per year")

<br>

## Calculate apparent consumption.
The apparent consumption is calculated in script '02_Apparent_Consumption_Method.R' using the formula apparent consumption = production + imports - exports.
Therefore data on production and international trade are input of this script:
* 'tbl_data_pcc_conf.csv'
* 'tbl_CN'

To perform this calculation units have to be converted to weight which is done by conversion tables of average weights. 
Also the prodcom and CN data has to be aggregated to UNU_Key level.
These calculations are also done in this script. They result in total value in kilo's and in units for each UNU_Key and are saved in 'UNU_countries.csv'.

<br>

## Description of method to calculate POM (Put on market)
The data resulting from the calculations in step 02 "Apparent_Consumption_Method" need correction of outliers and estimation of missing data. That is performed in script '03_POM_calculations.R'.

The input is 'UNU_countries.csv'

<br>

### First adjustments
Additional data on number of inhabitants and a stratum code to group similar countries (based on purchasing power) is added. With those we can calculate the first version of the kilo's per inhabitant and the units per inhabitant. These ratios will be the basis for the checks and corrections.

<br>

### Manual corrections
A number of manual corrections are being carried out. These result from the analysis of the automatic corrections. When there are a few years in a row with unreliable data, the automatic procedures cannot correct them.
Some unreliable data is corrected using knowledge of the market. For instance CRT TVs have not been sold in recent years, so they are set to 0.

<br>

### Solar panel data
Data on solar panels (UNU_Key 0002) is taken from the Eurostat website. This data is included. This data is only available in megawatts. We calculate the kilo's by assuming a number of kg's per megawatt and an average weight per panel. These values are around 100.000 kg per megawatt and 17 kg per panel, but they vary with every year.

### Automatic corrections for all UNU_Keys, countries and years
#### Remove unreliable data by comparing with other years from the same Country and product category
The outlier detection is done using the Median Absolute Deviation (MAD). For this outlier detection method, the median of the data (all years of a specific country and product category) is calculated. Then, the difference is calculated between each historical value and this median. These differences are expressed as their absolute values, and a new median is calculated of those absolute values. This is the MAD. If a value is 4 times the MAD away from the median of the data points, that value is classified as an outlier.

Example:

| Year                  |  2005 |  2006 |  2007 |  2008 |  2009 |  2010 |  2011 |  median |
|-----------------------|-------|-------|-------|-------|-------|-------|-------|---------|
| kg per inhabitant     |  3.08 |  3.04 |   3.5 |  **4.96** |  2.71 |   2.6 |  2.47 |    **3.04** |
| deviation from median	|  0.04 |  0.00 |  0.46 |  1.92 |  0.33 |  0.43 |  0.56 |    **0.43** |

factor:	4							
lower threshold	1.30		(= 3.04 - 4 * 0.43)		
upper threshold	4.78		(= 3.04 + 4 * 0.43)		

The value of 2008 (4.96) will be classified as an outlier because it lies above the upper threshold of 4 times the median absolute deviation
There have to be at least 6 values for calculating the median. With less than 6 values, no outliers are being determined.

#### Remove unreliable data by comparing data on a specific year and product category with other countries
Also this outlier detection is done using the MAD. The only difference is that the data of a specific year and product group is compared with data from similar countries (given by the stratum).

#### Calculate years by using the average of other years.
In case one year is missing (in original data or after removal of outliers) it is calculated with the average of the previous and following year. 
For instance when 2007 was missing it will be calculated in the following way:

| Year                  |  2005 |  2006 |  2007 |  2008 |
|-----------------------|------:|------:|------:|------:|
| kg per inhabitant     |  1.20 |  1.20 |  **1.40** |  1.60 


With 2 consecutive missing years a missing year gets 2/3 of the value of the adjacent year and 1/3 of the value of the year next to the other missing year.
For instance when 2006 and 2007 were missing they will be calculated in the following way:

| Year                  |  2005 |  2006 |  2007 |  2008 |
|-----------------------|------:|------:|------:|------:|
| kg per inhabitant     |  1.20 |  **1.33** |  **1.47** |  1.60 

If the missing year is the last available year then it is calculated by adding the difference of the two preceding years to the previous year. This way the trend of the previous 2 years is being continued.
For instance when 2012 (the last year for which we have data) is missing they will be calculated in the following way:

| Year                  |  2009 |  2010 |  2011 |  2012 |
|-----------------------|------:|------:|------:|------:|
| kg per inhabitant     |  1.30 |  1.40 |  1.70 |  **2.00** 


#### Calculate missing years by using the average of other similar countries
First the average kilo's per inhabitant and average units per inhabitant are calculated for each stratum. Also the average purchasing power of each stratum is calculated (purchasing power per country multiplied with inhabitants of each country, then divided by total inhabitants in stratum).  
The purchasing power of the country with the missing data is also known. When the purchasing power is between the average of 2 stratums the missing value is calculated by taking the difference between de kilo's/units per inhabitant of the higher stratum and the lower stratum. This will then be divided by the difference in purchasing power between the higher stratum and the lower stratum. After this it will be multiplied with the difference in purchasing power between the country with the missing data and that of the lower stratum. Finally this outcome will be added to the average kg/units per inhabitant of the lower stratum. 
The following example with demonstrate this procedure.

| Country               |   A   |   B   |   C   |   D   |   E   |   F   | stratum | lower stratum |
|-----------------------|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-------:|:-------------:|
| purchasing power      | 19500 | 21928 | 22028 | 23193 | 18023 | 21344 |   21307 |         17480 |
| kg per inhabitant     |  **0.81** |  1.20 |  0.70 |  1.15 |  0.85 |  1.10 |    1.00 |          0.60 |


All countries A to F are from the same stratum. Country A had missing data. The average kilo's per inhabitant for the stratum is 1.00 based on the known values from countries B to F. The average purchasing power is a weighted average using the number of inhabitants of each country. The last column shows the averages for countries in the next lower stratum. 

The calculation is as follows: 0.6 + (1.00 - 0.6)  / (21,307 - 17,480) * (19,500 - 17,480) = 0.81

This way a value will be calculated based on the stratum average, but adjusted for the purchasing power. The country with the lowest purchasing power in this dataset is Rumania. This country is used to adjust for the purchasing power for countries that have a purchasing power that lies between Rumania and the average of the lowest stratum. The country with the highest purchasing power is Luxemburg. This country is used to adjust for the purchasing power of countries that have a purchasing power that lies between the average of the highest stratum and Luxemburg.

#### Eliminate big changes between years
After the last calculations there can be big changes from year to year. These are corrected in two ways. 
The first check is to see if a specific outcome considering the kilo's per inhabitant differs more than 50% from the previous year (t-1) while the previous year and the next year (t+1) don't differ more than 20%. This only done if the value that will possibly be corrected is higher than 0.3. Below this value there is only little impact of the changes on the outcome of the collection categories.  
The second check is to see if a specific outcome is more than 20 percent below the minimum of the two surrounding years and more than 40 percent below the maximum value of those years. Also if a specific outcome is more than 20 percent higher than the maximum of the two surrounding years and more than 40 percent higher than the minimum of those years. This check is not done if the value of year t+2 is within 20 percent of the year that might be corrected. The reason for this is that in this case the year t+1 might be the one with an unreliable outcome. Just as with the first check, the second check is only done if the value that will possibly be corrected is higher than 0.3.
Once a check has had a positive outcome the corresponding value will be deleted.  
After this the empty values will be again calculated in the same way as described in the part "Calculate years by using the average of other years" above. So they will be replaced by the average of surrounding years.

<br>

### Correction of CRT monitors (UNU_Key 0302)
The amount of CRT monitors (UNU_Key 0302) is not reliable with the above method. Therefore we calculate them in another way with the formula: CRT Monitors [kg] = ((Desktop sales[pieces] * 116% ) - Flatpanels [pieces] ) * average weight CRT Monitors. The desktop sales are the outcome of UNU_Key 0302 and the flatpanels are UNU_Key 0309.

<br>

### Calculate past POM
Put-on-Market data is only calculated based on prodcom and International Trade data starting in 1995. For some countries especially those in stratum 3 data for 1995-2000 is also not available. Data for the years from 1980 until the year for which data is available, is calculated based on the POM of the first 3 available years. For instance if data is available from 1995 onwards then data from 1980 to 1994 is calculated based on the data of 1995-1997.  
In this example the data for 1994 is calculated as the average of 1995 to 1997 minus a fixed percentage. This fixed percentage is 2 for every UNU_Key. Every lower year is the same as one year higher minus the fixed percentage.  
In case these calculations form a strange graph when connecting the estimated historic data and the data based on the registers, an extra correction is done. When for instance 1995 is lower than 1996 but also lower than 1994, the year 1995 will be changed to the average of 1994 and 1996.

<br>

### Calculate future POM
The same method is used to predict the future POM starting in the first year for which not data is available yet. For instance in case data is available until 2015 then 2016 is calculated as the average of 2013 to 2015 plus a fixed percentage (at the moment 2 for every UNU_Key). Every next year is an increases with that same percentage from the previous year.  
In case these calculations form a strange graph when connecting the estimated future data and the data based on the registers, an extra correction is done. When for instance 2015 is higher than 2016 and also higher as 2014, the year 2015 will be changed to the average of 2014 and 2016.  
The only exception of this method are the solar panels (UNU_Key 0002). The first future year is only based on the last available data year instead of a three year average.

<br>

### Result
After all the estimations and corrections the result is saved into 'tbl_POM.csv'.

<br>

## Calculation of WEEE generated
The WEEE generated is calculated by using the apparent consumption and life-cycle profiles of each of the product groups (UNU_Keys). This is done with the Weibull function.
Input of this script is 'tbl_POM.csv' and the output is 'tbl_WEEE.csv'.

<br>

## Calculated aggregates, stock, graphs and output files.
Input of this script are the files:
* 'tbl_POM.csv'
* 'tbl_WEEE.csv'

Now some aggregates of the POM and WEEE data are calculated.

After this the stock is being calculated which is the sum of historic POM minus the sum of all historic WEEE.

Data is saved in the following output files
* tbl_ANALYSIS.RData - This file can only be accessed with R and contains a list of 4 dataframes:
  1. POM data
  2. WEEE generated data
  3. Stock data
  4. Eurostat WEEE directive reference data
* tbl_POM_all.csv
* tbl_WEEE_all.csv
* tbl_stock_all.csv

<br>
<br>
<br>

# Reference
When referring to this 'Waste over Time' script please use:

Van Straalen, V.M,  Roskam, A.J., & Baldé, C.P. (2016). Waste over Time [computer software]. The Hague, The Netherlands: Statistics Netherlands (CBS). Retrieved from:
[http://github.com/Statistics-Netherlands/ewaste](http://github.com/Statistics-Netherlands/ewaste)

<br>
<br>
<br>

# Annex 1 - Instruction to update International Trade source datafile.
International trade data is published at the Eurostat website:
http://epp.eurostat.ec.europa.eu/newxtweb/

Make sure you register yourself so you can download the big amount of data.  
Go to "Available datasets" --> "INTERNATIONAL TRADE" -->
"EU Trade Since 1988 By CN8 (DS-016890)". Click on the "New Query" icon.

Fill the following fields in Step 1 "Dimension Selection":
                   
* Reporter:  
  Select all countries except the aggregations starting with EA, EU or EUROZONE.
  Click the "select" button when you are done.
* Partner:  
Select only:
    - EU25_EXTRA-EU25-EXTRA
    - EU25_INTRA-EU25-INTRA
* Product:  
  Select only the following CN codes (the advanced selection box is very helpful):
    - 63011000
    - 84*
    - 85*
    - 87119010 + 87119090
    - 90*
    - 91*
    - 92*
    - 94*
    - 95*
* Flow:  
  Select all
* Period:
  - Select the years for which you would like to download data. You can choose up to 6 years at once which will result in a download of about 20MB.
  -  Choose only the complete years.
For instance for year 2015 I choose: 201552-Jan.Dec.2015.
* Indicators:  
  Select all

After this press the Compress button to remove codes that do not generate data.

In Step 2 "Layout selection" make the following selection:
* Rows:
    - Dimension#1: PERIOD
    - Dimension#2: FLOW
    - Dimension#3: REPORTER
    - Dimension#4: PRODUCT
* Columns:
    - Dimension#1: INDICATORS
    - Dimension#2: PARTNER

* All formats are codes

In Step 3 "Output selection" do the following:
* Select: Batch extraction
* Select: Also generate output
* Select: Show in output: Codes
* Select: output format: CSV

Give extraction name and it is useful to select "Notify me whenever this dataset is uploaded"
Click the "Finish" button.
Once completed you can download the data under the tab "Completed Works".
Place it in the folder '.\ewaste-master\data\international_trade'.

Extract and rename the datafile into something meaningful like 'IT2010-2015.csv'
Open the R script and enter the filenames of datafiles in the list "ITdata" in the script at line 90.

Run the entire '00b_Prepare_International_Trade_data.R' script from R by selecting all rows and pressing CTRL+ENTER. After this 'tbl_CN' has been updated.
