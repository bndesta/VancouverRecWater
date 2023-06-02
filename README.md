# VancouverRecWater
#The R script named 'Vancouver_data preparation and description plot.R' include the code used to clean, manipulate, 
#and prepare the data for the main analysis and to produce the plot for the mean of geometric mean E. coli counts 
#over the years


#The R script named 'Vancouver_Bayesian_Modelling.R' contain all the code used to perform the Bayesian 
#log-linear mixed-effects analysis and plots


#The csv file named 'VancouverHarbourRaw2013_2021.csv' contains the raw data (containing raw air 
#temperature and precipitation data) extracted from Environment and Climate Change Canada 
#historical data repository for this analysis 
#(Vancouver Harbour weather station)
#https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1976-01-20%7C2022-09-18&dlyRange=1925-11-01%7C2022-09-17&mlyRange=1925-01-01%7C2007-02-01&StationID=888&Prov=BC&urlExtension=_e.html&searchType=stnProx&optLimit=yearRange&Month=9&Day=12 
#(accessed on 15 September 2022)


#The csv file named 'Vancouver_Harbour_2005-2021_Weather.csv' contains processed data 
#(containing 24-hr air temperature and 48-hr precipitation data) extracted from 
#Environment and Climate Change Canada historical data repository for this analysis 
#(Vancouver Harbour weather station)
 

#The csv file named 'NorthVancouverWHarvesData.csv' contains the raw data (containing air temperature 
#and precipitation extracted from Environment and Climate Change Canada historical data 
#repository for this analysis to filling some missing data for air temperature 
#and precipitation that obtained from the Vancouver Harbour weather station
#(North Vancouver WHarves weather station)
#https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=%7C&dlyRange=1962-03-01%7C2023-05-28&mlyRange=1962-01-01%7C2007-02-01&StationID=833&Prov=BC&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2023&selRow 
#(accessed on 15 March 2023) 

#The csv file named 'VancouverWaterLevel.csv' contains the raw water level data extracted from
#Department of Oceans and Fisheries Canada data 
#(https://www.isdm-gdsi.gc.ca/isdm-gdsi/twl-mne/inventory-inventaire/data-donnees-eng.asp?user=isdm-gdsi&region=PAC&tst=1&no=7735) 
#(accessed on 15 September 2022)

#The csv file named 'SentryShoalBuoy.csv' contains the raw wave data extracted from the 
#Department of Oceans and Fisheries Canada
#(https://www.meds-sdmm.dfo-mpo.gc.ca/isdm-gdsi/waves-vagues/data-donnees/data-donnees-eng.asp?medsid=C46131)
#(accessed on 15 September 2022)

#The csv file named 'POWER_Point_Hourly_20130101_20210331_049d2764N_123d1493W_LST.csv' 
#contains the raw UV index data extracted from NASA database of solar and meteorological parameters 
#(https://power.larc.nasa.gov/data-access-viewer/) (accessed on 15 September 2022)

#The csv file named 'POWER_Point_Hourly_20130101_20210331_049d2764N_123d1493W_LST.csv' 
#contains the the processed UV index data (containing 24-hr mean UV index data) 
#extracted from NASA database of solar and meteorological parameters 

#Vancouver's beach E. coli and Salinity data can be requested from Metro Vancouver
