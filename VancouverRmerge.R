#Vancouver

library(dplyr)
library(tidyr)
library(lubridate)
library(psych)


##Vancouver 

Vancoli=VancouverEcoli2013_2021_Modified%>%
  group_by(BeachName, Date)%>%
  mutate(Geomean10=geometric.mean(as.numeric(Ecoli10),na.rm = TRUE))%>%
  mutate(MeanSalinty=mean(as.numeric(Salinity_half), na.rm = TRUE))%>%
  distinct(BeachName,Date,Geomean10,MeanSalinty)%>%
  ungroup()%>%
  arrange(Date)%>%
  group_by(BeachName)%>%
  mutate(PrevGeomean=lag(Geomean10))%>%
  ungroup()


#Weather station data
Vanweath= VancouverHarbourRaw2013_2021%>%
  rename(Rainday=`Total Precip (mm)`)%>%
  arrange(., `Date/Time`)%>% #organize for lagging rain
  mutate(Rain24=lag(Rainday))%>% #one day lag of rain
  mutate(Rain48=as.numeric(lag(Rainday))+as.numeric(lag(Rain24)))%>%
  mutate(MeanTemp24=lag(`Mean Temp (°C)`))%>%
  mutate(DidRain=case_when(Rainday!=0~1,TRUE~0))%>%
  mutate(DaysofRain = with(., ave(DidRain, cumsum(DidRain == 0), FUN = cumsum)))%>%
  mutate(DidNOTRain=case_when(Rainday==0~1,TRUE~0))%>%
  mutate(DaysSinceRain = with(., ave(DidNOTRain, cumsum(DidNOTRain == 0), FUN = cumsum)))
VanorthWeather= NorthVancouverWharvesData%>%
  rename(Rainday=`Total Precip (mm)`)%>%
  arrange(., `Date/Time`)%>% #organize for lagging rain
  mutate(Rain24=lag(Rainday))%>% #one day lag of rain
  mutate(Rain48=as.numeric(lag(Rainday))+as.numeric(lag(Rain24)))%>%
  mutate(DidRain=case_when(Rainday!=0~1,TRUE~0))%>%
  mutate(DaysofRain = with(., ave(DidRain, cumsum(DidRain == 0), FUN = cumsum)))%>%
  mutate(DidNOTRain=case_when(Rainday==0~1,TRUE~0))%>%
  mutate(DaysSinceRain = with(., ave(DidNOTRain, cumsum(DidNOTRain == 0), FUN = cumsum)))

  
#UV 
UVVan=POWER_Point_Hourly_20130101_20211231_049d2773N_123d1459W_LST%>%
  group_by(Date)%>%
  mutate(MeanUVday=mean(UV))%>%
  mutate(MaxUVday=max(UV))%>%
  distinct(Date, MeanUVday, MaxUVday)%>%
  ungroup()%>%
  arrange(Date)%>%
  mutate(MeanUV24=lag(MeanUVday))%>%
  mutate(MaxUV24=lag(MaxUVday))

#Bouy data
VanBouy=SentryShoalBuoy%>%
  arrange(as.Date(Date))%>%
  mutate(Waveht24=lag(`VWH$`))

#water level
Vanwtr=Waterlevel_07735_Vancouver_2021%>%
  rename(Obs_date=`Row Labels`)%>%
  arrange(Obs_date)%>%
  mutate(Wtrlvl24=lag(WtrLvl))

#Merge all together
VancouverMerged01152023=left_join(Vancoli, Vanweath, by=c("Date"="Date/Time"))%>%
  left_join(., VanorthWeather, by=c("Date"="Date/Time"))%>%
  left_join(.,VanBouy, by="Date")%>%
  left_join(., UVVan, by="Date")%>%
  left_join(.,Vanwtr, by=c("Date"="Obs_date"))

write.csv(VancouverMerged01202023, file="VancouverMerged01202023")



  
  
