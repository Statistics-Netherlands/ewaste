Release notes v1.1.

00a_Prepare_Prodcom_data.R 
This script used the 'RODBC' package to read Excel files. In some cases the script could result in errors because of conflicts with 64 bits Excel drivers. Now a new method is being used using the 'readxl' package. This does not require Excel drivers anymore.


