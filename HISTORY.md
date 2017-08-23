#### Release notes v1.2.

**00b_Prepare_International_Trade_data.R**

Switched to using Eurostat Bulkdownload Facility for International trade instead of the Easy Comext website.

**00c_Prepare_Solar_Panel_data.R**

Now using OData for collecting information about solar panels capacity.

**03i_extend_time_series.R**

Extrapolations are completely redone, now various new approaches for forecasting and also a new approach for extrapolations in the past.

**04_WEEE_calculations.R**

Calculation of number of pieces for WEEE is now based on the average weight in the year product was put on the market instead of on the average weight of the product in the year it became WEEE.

**05_Make_tblAnalysis.R and 05a_calculate_aggregates.R**

Calculation of number of pieces for POM, WEEE and STOCK is now also done for the aggregates of the 54 UNU_Keys.

**and more**

* Some links of Prodcom and CN to UNU_Keys have been updated
* Scale and Shape parameters for Weibul function in the WEEE calculations have been updated
* Average Weights have been updated
* Some manual corrections have been added

<br>
<br>
<br>

***
#### Release notes v1.1.

**00a_Prepare_Prodcom_data.R**

This script used the 'RODBC' package to read Excel files. In some cases the script could result in errors because of conflicts with 64 bits Excel drivers. Now a new method is being used using the 'readxl' package. This does not require Excel drivers anymore.
