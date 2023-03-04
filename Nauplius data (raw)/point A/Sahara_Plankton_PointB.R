#packages
install.packages("tidyverse")
require(tidyverse)
library(dplyr)
install.packages("ggplot2")
install.packages("raster")
require(ggplot2)
require(raster)
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(cluster)
install.packages("ggpubr")
library(ggpubr)
install.packages("ggdendro")
library(ggdendro)
install.packages("magrittr")
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("sjstats")
install.packages("arm")
install.packages("lme4")
install.packages("lmerTest")
install.packages("readxl")
install.packages("glmmTMB")
install.packages("ggthemes")
require(lmerTest)
require(sjPlot)
require(sjmisc)
require(sjstats)
require(arm)
require(lme4)
require(lmerTest)
require(readxl)
require(magrittr)
require(glmmTMB)
require(ggthemes)
install.packages("maps")
require(maps)

#read in data
Bnasa <- read_csv("dataCHLA_NASAcombo.csv")
BOCCCI <- read_csv("dataCHLA_OCCCI-v50.csv")
BSST <- read_csv("dataRSST_HadISST.csv")
BOSCAR_u_EW <- read_csv("dataCURR_OSCAR-u-EW.csv")
BOSCAR_v_NS <- read_csv("dataCURR_OSCAR-v-NS.csv")
BCbPM2 <- read_csv("dataNPP_CbPM2.csv")
BHen4 <- read_csv("dataPSAL_HEN4-z0005.csv")
BROI2sst <- read_csv("dataRSST_ROI2sst.csv")
BICOADS <- read_csv("dataWIND_ICOADS.csv")
BSeaWinds_SCALAR <- read_csv("dataWIND_SeaWinds-SCALAR.csv")
BSeaWinds_u_EW <- read_csv("dataWIND_SeaWinds-u-EW.csv")
BSeaWinds_v_NS <- read_csv("dataWIND_SeaWinds-v-NS.csv")

#wrangling
#merge datasets

#tidy columns for Bnasa
Bnasa<-Bnasa%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("NASA_Chlorophyll"="CHLA= NASA-combo satellite Chlorophyll (mg/m3)")

#extract year only and convert to numeric
Bnasa$Year <- format(as.Date(Bnasa$Year), "%Y-%m-%d")
#Bnasa$Year <- as.numeric(Bnasa$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#Bnasa <- Bnasa %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))


#tidy columns for BOCCCI
BOCCCI<-BOCCCI%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Satellite_Chlorophyll"="CHLA= OCCCI-v5.0 satellite Chlorophyll (mg/m3)")

#extract year only and convert to numeric
BOCCCI$Year <- format(as.Date(BOCCCI$Year), "%Y-%m-%d")
#BOCCCI$Year <- as.numeric(BOCCCI$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BOCCCI <- BOCCCI %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))


#tidy columns for BSST
BSST<-BSST%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Temperature"="HSST= HadISST Temperature (C)")

#extract year only and convert to numeric
BSST$Year <- format(as.Date(BSST$Year), "%Y-%m-%d")
#BSST$Year <- as.numeric(BSST$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BSST <- BSST %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average temperature by year
#BSST$Temperature<-BSST$Temperature/12


#tidy columns for BOSCAR_u_EW
BOSCAR_u_EW<-BOSCAR_u_EW%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Surface_Currents_u_EW"="OTHR= OSCAR surface currents u-EW-component (m/s)")

#extract year only and convert to numeric
BOSCAR_u_EW$Year <- format(as.Date(BOSCAR_u_EW$Year), "%Y-%m-%d")
#BOSCAR_u_EW$Year <- as.numeric(BOSCAR_u_EW$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BOSCAR_u_EW <- BOSCAR_u_EW %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Surface_Currents by year
#BOSCAR_u_EW$Surface_Currents_u_EW<-BOSCAR_u_EW$Surface_Currents_u_EW/12


#tidy columns for BOSCAR_v_NS
BOSCAR_v_NS<-BOSCAR_v_NS%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Surface_Currents_v_NS"="OTHR= OSCAR surface currents v-NS-component (m/s)")

#extract year only
BOSCAR_v_NS$Year <- format(as.Date(BOSCAR_v_NS$Year), "%Y-%m-%d")
#BOSCAR_v_NS$Year <- as.numeric(BOSCAR_v_NS$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BOSCAR_v_NS <- BOSCAR_v_NS %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Surface_Currents by year
#BOSCAR_v_NS$Surface_Currents_v_NS<-BOSCAR_v_NS$Surface_Currents_v_NS/12


#tidy columns for BCbPM2
BCbPM2<-BCbPM2%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Net_Primary_Production"="CHLA= CbPM2 NPP (mg-C/m2/day)")

#extract year only
BCbPM2$Year <- format(as.Date(BCbPM2$Year), "%Y-%m-%d")
#BCbPM2$Year <- as.numeric(BCbPM2$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BCbPM2 <- BCbPM2 %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))


#tidy columns for BHen4
BHen4<-BHen4%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Sea_Surface_Salinity"="PSAL= Hadley-EN4 Salinity () at 5 m")

#extract year only
BHen4$Year <- format(as.Date(BHen4$Year), "%Y-%m-%d")
#BHen4$Year <- as.numeric(BHen4$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BHen4 <- BHen4 %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))


#tidy columns for BROI2sst
BROI2sst<-BROI2sst%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Sea_Surface_Temperature"="TEMP= Reynolds OI-SST-v2 Temperature (C)")

#extract year only
BROI2sst$Year <- format(as.Date(BROI2sst$Year), "%Y-%m-%d")
#BROI2sst$Year <- as.numeric(BROI2sst$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BROI2sst <- BROI2sst %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Sea_Surface_Temperature by year
#BROI2sst$Sea_Surface_Temperature<-BROI2sst$Sea_Surface_Temperature/12


#tidy columns for BICOADS
BICOADS<-BICOADS%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Sea_Surface_Scalar_Windspeed"="OTHR= ICOADS scalar Windspeed (m/s)")

#extract year only
BICOADS$Year <- format(as.Date(BICOADS$Year), "%Y-%m-%d")
#BICOADS$Year <- as.numeric(BICOADS$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BICOADS <- BICOADS %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Sea_Surface_Scalar_Windspeed by year
#BICOADS$Sea_Surface_Scalar_Windspeed<-BICOADS$Sea_Surface_Scalar_Windspeed/12


#tidy columns for BSeaWinds_SCALAR
BSeaWinds_SCALAR<-BSeaWinds_SCALAR%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("Ocean_Surface_Seawinds_Scalar_Windspeed"="OTHR= SeaWinds scalar Windspeed (m/s)")

#extract year only
BSeaWinds_SCALAR$Year <- format(as.Date(BSeaWinds_SCALAR$Year), "%Y-%m-%d")
#BSeaWinds_SCALAR$Year <- as.numeric(BSeaWinds_SCALAR$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BSeaWinds_SCALAR <- BSeaWinds_SCALAR %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Sea_Surface_Scalar_Windspeed by year
#BSeawinds_SCALAR$Ocean_Surface_Seawinds_Scalar_Windspeed<-BSeaWinds_SCALAR$Ocean_Surface_Seawinds_Scalar_Windspeed/12


#tidy columns for BSeaWinds_u_EW
BSeaWinds_u_EW<-BSeaWinds_u_EW%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("EW_Ocean_Surface_Seawinds_Scalar_Windspeed"="OTHR= SeaWinds Windspeed u-EW-component (m/s)")

#extract year only
BSeaWinds_u_EW$Year <- format(as.Date(BSeaWinds_u_EW$Year), "%Y-%m-%d")
#BSeaWinds_u_EW$Year <- as.numeric(BSeaWinds_u_EW$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BSeaWinds_u_EW <- BSeaWinds_u_EW %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Sea_Surface_Scalar_Windspeed by year
#BSeaWinds_u_EW$EW_Ocean_Surface_Seawinds_Scalar_Windspeed<-BSeaWinds_u_EW$EW_Ocean_Surface_Seawinds_Scalar_Windspeed/12


#tidy columns for BSeaWinds_v_NS
BSeaWinds_v_NS<-BSeaWinds_v_NS%>%
  dplyr::select(-...3 )%>%
  dplyr::rename("Year"="DATE-YMD")%>%
  #rename("Month"="...2")%>%
  dplyr::rename("NS_Ocean_Surface_Seawinds_Scalar_Windspeed"="OTHR= SeaWinds Windspeed v-NS-component (m/s)")

#extract year only
BSeaWinds_v_NS$Year <- format(as.Date(BSeaWinds_v_NS$Year), "%Y-%m-%d")
#BSeaWinds_v_NS$Year <- as.numeric(BSeaWinds_v_NS$Year)

#impute year values
#Anasa<-Anasa%>%
#group_by(Year)%>%
#  fill(Year, .direction = "downup")

#group by year
#BSeaWinds_v_NS <- BSeaWinds_v_NS %>% group_by(Year)  %>%
  #  dplyr::select(-Month)%>%
  #summarise("NASA Chlorophyll (mg/m3)" = sum(Anasa$`NASA Chlorophyll (mg/m3)`),
  #          .groups = 'drop')%>%
  #summarise_each(funs(sum))

#average Sea_Surface_Scalar_Windspeed by year
#BSeaWinds_v_NS$NS_Ocean_Surface_Seawinds_Scalar_Windspeed<-BSeaWinds_v_NS$NS_Ocean_Surface_Seawinds_Scalar_Windspeed/12


'''
#tidy columns for AOCCCI
AOCCCI<-AOCCCI%>%
  rename("Year"="...1")%>%
  rename("Month"="...2")%>%
  rename("Satellite Chlorophyll (mg/m3)"="CHLA= OCCCI-v5.0 satellite Chlorophyll (mg/m3)")

#impute year values
AOCCCI<-AOCCCI%>%
  #group_by(Year)%>%
  fill(Year, .direction = "downup")

#group by year
AOCCCI_Yearly <- AOCCCI %>% group_by(Year)  %>%
  dplyr::select(-Month)%>%
  summarise_each(funs(sum))

#tidy columns for ASST
ASST<-ASST%>%
  rename("Year"="...1")%>%
  rename("Month"="...2")%>%
  rename("Temperature"="HSST= HadISST Temperature (C)")

#impute year values
ASST<-ASST%>%
  #group_by(Year)%>%
  fill(Year, .direction = "downup")

#group by year
ASST_Yearly <- ASST %>% group_by(Year)  %>%
  dplyr::select(-Month)%>%
  summarise_each(funs(sum))


#merge datasets
PointA<-merge(Anasa, AOCCCI, by=c("Year", "Month"))
#average cholorophyll
PointA$Chlorophyll <- rowMeans(subset(PointA, select = c("NASA Chlorophyll (mg/m3)", "Satellite Chlorophyll (mg/m3)")), na.rm = TRUE)
#merge with temperature
PointA<-merge(PointA, ASST, by=c("Year", "Month"))

#merge yearly datasets
PointA_Yearly<-merge(Anasa_Yearly, AOCCCI_Yearly, by="Year")
#average cholorophyll
PointA_Yearly$Chlorophyll <- rowMeans(subset(PointA_Yearly, select = c("NASA Chlorophyll (mg/m3)", "Satellite Chlorophyll (mg/m3)")), na.rm = TRUE)
#merge with temperature
PointA_Yearly<-merge(PointA_Yearly, ASST_Yearly, by="Year")
#Average yearly temperature (divide by 12)
PointA_Yearly$Temperature<-PointA_Yearly$Temperature/12

#rank from top to bottom
#PointA$Rank_Chlorophyll<-rank(desc(PointA$Chlorophyll))
#PointA_Yearly$Rank_Chlorophyll<-rank(desc(PointA_Yearly$Chlorophyll))
'''

#merge data inner_join
PointB<-list(Bnasa,BOCCCI,BSST,BOSCAR_u_EW,BOSCAR_v_NS,BCbPM2,BHen4,BROI2sst,BICOADS,BSeaWinds_SCALAR,BSeaWinds_u_EW,BSeaWinds_v_NS)%>% 
  reduce(inner_join, by='Year')

#merge data inner_join
PB<-list(Bnasa,BOCCCI,BSST,BOSCAR_u_EW,BOSCAR_v_NS,BCbPM2,BHen4,BROI2sst,BICOADS,BSeaWinds_SCALAR,BSeaWinds_u_EW,BSeaWinds_v_NS)%>% 
  reduce(full_join, by='Year')

#average and remove common columns
PointB$Chlorophyll <- rowMeans(subset(PointB, select = c("NASA_Chlorophyll", "Satellite_Chlorophyll")), na.rm = TRUE)
PointB$Temperature <- rowMeans(subset(PointB, select = c("Temperature", "Sea_Surface_Temperature")), na.rm = TRUE)
PointB$Currents <- rowMeans(subset(PointB, select = c("Surface_Currents_u_EW", "Surface_Currents_v_NS")), na.rm = TRUE)
PointB$Windspeed <- rowMeans(subset(PointB, select = c("Sea_Surface_Scalar_Windspeed", "Ocean_Surface_Seawinds_Scalar_Windspeed", "EW_Ocean_Surface_Seawinds_Scalar_Windspeed", "NS_Ocean_Surface_Seawinds_Scalar_Windspeed")), na.rm = TRUE)
PointB<-PointB%>%
  dplyr::select(-c("NASA_Chlorophyll", "Satellite_Chlorophyll", "Sea_Surface_Temperature", "Surface_Currents_u_EW", "Surface_Currents_v_NS", "Sea_Surface_Scalar_Windspeed", "Ocean_Surface_Seawinds_Scalar_Windspeed", "EW_Ocean_Surface_Seawinds_Scalar_Windspeed", "NS_Ocean_Surface_Seawinds_Scalar_Windspeed"))

PB$Chlorophyll <- rowMeans(subset(PB, select = c("NASA_Chlorophyll", "Satellite_Chlorophyll")), na.rm = TRUE)
PB$Temperature <- rowMeans(subset(PB, select = c("Temperature", "Sea_Surface_Temperature")), na.rm = TRUE)
PB$Currents <- rowMeans(subset(PB, select = c("Surface_Currents_u_EW", "Surface_Currents_v_NS")), na.rm = TRUE)
PB$Windspeed <- rowMeans(subset(PB, select = c("Sea_Surface_Scalar_Windspeed", "Ocean_Surface_Seawinds_Scalar_Windspeed", "EW_Ocean_Surface_Seawinds_Scalar_Windspeed", "NS_Ocean_Surface_Seawinds_Scalar_Windspeed")), na.rm = TRUE)
PB<-PB%>%
  dplyr::select(-c("NASA_Chlorophyll", "Satellite_Chlorophyll", "Sea_Surface_Temperature", "Surface_Currents_u_EW", "Surface_Currents_v_NS", "Sea_Surface_Scalar_Windspeed", "Ocean_Surface_Seawinds_Scalar_Windspeed", "EW_Ocean_Surface_Seawinds_Scalar_Windspeed", "NS_Ocean_Surface_Seawinds_Scalar_Windspeed"))

#imputation
install.packages('mice')
library(mice)
PB$Chlorophyll<-complete(mice(PB, method = "cart"))$Chlorophyll
PB$Currents<-complete(mice(PB, method = "cart"))$Currents
PB$Windspeed<-complete(mice(PB, method = "cart"))$Windspeed
PB$Sea_Surface_Salinity<-complete(mice(PB, method = "cart"))$Sea_Surface_Salinity
PB$Net_Primary_Production<-complete(mice(PB, method = "cart"))$Net_Primary_Production


#summary
summary(PointB)
summary(PB)


#plots
#scatterplot
ggplot(data = PointB, aes(x=Temperature,y=Chlorophyll,colour=factor(Windspeed))) +
  # ggplot with the desired data
  geom_point(aes(x=Temperature,y=Chlorophyll) + # Specifying that we want it to be a scatter plot
               geom_smooth(method="lm") + # Indicating we want to add a linear trend to the plot
               labs(x="Temperature", y="Chlorophyll",shape="Windspeed")) # Axes labels


#regression model
#Chlorophyll vs Year
PointB_CYlm <- lm(Chlorophyll~Year,data=PointB)
summary(PointV_CYlm)

ggplot(data=PointB,aes(x=Year,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "red") +
         xlab("Year") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between Year and Chlorophyll"))

#Chlorophyll vs Salinity
PointB_CSlm <- lm(Chlorophyll~Sea_Surface_Salinity,data=PointB)
summary(PointB_CSlm)

ggplot(data=PointB,aes(x=Sea_Surface_Salinity,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "green") +
         xlab("Sea_Surface_Salinity") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between Salinity and Chlorophyll"))

#Chlorophyll vs Temperature
PointB_CTlm <- lm(Chlorophyll~Temperature,data=PointB)
summary(PointB_CTlm)

ggplot(data=PointB,aes(x=Temperature,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "blue") +
         xlab("Temperature") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between Temperature and Chlorophyll"))

#Chlorophyll vs Windspeed
PointB_CWlm <- lm(Chlorophyll~Windspeed,data=PointB)
summary(PointB_CWlm)

ggplot(data=PointB,aes(x=Windspeed,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "blue") +
         xlab("Windspeed") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between Windspeed and Chlorophyll"))

#Chlorophyll vs Currents
PointB_CClm <- lm(Chlorophyll~Currents,data=PointB)
summary(PointB_CClm)

ggplot(data=PointB,aes(x=Currents,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "blue") +
         xlab("Currents") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between Currents and Chlorophyll"))

#Chlorophyll vs NPP
PointB_CNPPlm <- lm(Chlorophyll~Net_Primary_Production,data=PointB)
summary(PointB_CNPPlm)

ggplot(data=PointB_CNPPlm,aes(x=Net_Primary_Production,y=Chlorophyll) +
         geom_point() +
         stat_smooth(method = "lm", col = "blue") +
         xlab("NPP") +
         ylab("Chlorophyll") +
         ggtitle("Relationship between NPP and Chlorophyll"))


#relationship
install.packages("Rmisc")
install.packages("GGally")
library(Rmisc)
library(GGally)

#log PointB
logPointB<-data.frame(
  Year=PointB$Year,
  logChlorophyll=log(PointB$Chlorophyll),
  logTemperature=log(PointB$Temperature),
  logCurrents=log(PointB$Currents),
  logWindspeed=log(PointB$Windspeed),
  logSea_Surface_Salinity=log(PointB$Sea_Surface_Salinity),
  logNet_Primary_Production=log(PointB$Net_Primary_Production))


#plot cholorophyll levels vs year
#p1<-ggplot(PointB)+
  #geom_point(aes(x=Year,y=Chlorophyll))+
  #labs(x="Year",y="Chlorophyll")

#plot chlorophyll levels vs temperature
p2<-ggplot(PointB)+
  geom_point(aes(x=Temperature,y=Chlorophyll))+
  labs(x="Temperature",y="Cholorhyll")

#plot chlorophyll levels vs Windspeed
p3<-ggplot(PointB)+
  geom_point(aes(x=Windspeed,y=Chlorophyll))+
  labs(x="Windspeed",y="Cholorophyll")

#plot chlorophyll levels vs Currents
p4<-ggplot(PointB)+
  geom_point(aes(x=Currents,y=Chlorophyll))+
  labs(x="Currents",y="Cholorophyll")

#plot chlorophyll levels vs NPP
p5<-ggplot(PointB)+
  geom_point(aes(x=Net_Primary_Production,y=Chlorophyll))+
  labs(x="NPP",y="Cholorophyll")

#plot chlorophyll levels vs SSS
p6<-ggplot(PointB)+
  geom_point(aes(x=Sea_Surface_Salinity,y=Chlorophyll))+
  labs(x="SSS",y="Cholorophyll")

#scatterplot matrix
multiplot(p2,p3,p4,p5,p6,cols=2)
plot(PointA,pch=20,ces=1.5,col="steelblue")

#correlation coefficients
cor(PointB[,c("Chlorophyll","Temperature", "Windspeed", "Currents", "Net_Primary_Production", "Sea_Surface_Salinity")])
#based on scatterplots and correlation coefficients,
#strong positive correlation Ozone-temperature,
#low positive correlation Ozone-radiation,
#strong negative correlation Ozone-Wind

#all relationships in one plot
PointB2<-PointB%>%dplyr::select(-Year)
ggpairs(PointB2,
        lower=list(continuous="smooth"),
        diag=list(continuous="barDiag"),
        axisLabels="show")

PB2<-PB%>%dplyr::select(-Year)
ggpairs(PB2,
        lower=list(continuous="smooth"),
        diag=list(continuous="barDiag"),
        axisLabels="show")

#Comment on your findings and what these plots suggest about the likely relationships between
#the response variable (ozone) and the other variables.
#when temperature increases, Ozone levels increase
#when radiation increases, Ozone levels increase a little
#when wind increases, Ozone levels decrease
#Ozone levels are more likely to be influenced by temperature/wind

#(b) [9 marks] Fit a multiple regression of ozone as the response variable, against radiation,
#temperature and wind as the explanatory variables (use all three, when fitting the model).
mod<-lm(Chlorophyll~Temperature+Windspeed+Currents+Net_Primary_Production+Sea_Surface_Salinity,data=PointB)

#ggplot(PointB,aes(x=Year,y=Chlorophyll))+
#  geom_point()+
#  geom_line(aes(x=Year,y=fitted(mod)),colour="red",)+
#  labs(x="Year",y="Chlorophyll")

ggplot(PointB,aes(x=Temperature,y=Chlorophyll))+
  geom_point()+
  geom_line(aes(x=Temperature,y=fitted(mod)),colour="blue",)+
  labs(x="Temperature",y="Chlorophyll")

ggplot(PointB,aes(x=Windspeed,y=Chlorophyll))+
  geom_point()+
  geom_line(aes(x=Windspeed,y=fitted(mod)),colour="green",)+
  labs(x="Windspeed",y="Chlorophyll")

ggplot(PointB,aes(x=Currents,y=Chlorophyll))+
  geom_point()+
  geom_line(aes(x=Currents,y=fitted(mod)),colour="red",)+
  labs(x="Currents",y="Chlorophyll")

ggplot(PointB,aes(x=Net_Primary_Production,y=Chlorophyll))+
  geom_point()+
  geom_line(aes(x=Net_Primary_Production,y=fitted(mod)),colour="blue",)+
  labs(x="NPP",y="Chlorophyll")

ggplot(PointB,aes(x=Sea_Surface_Salinity,y=Chlorophyll))+
  geom_point()+
  geom_line(aes(x=Sea_Surface_Salinity,y=fitted(mod)),colour="green",)+
  labs(x="SSS",y="Chlorophyll")

#summary
summary(mod)
#based on respective p-values:
#radiation:insignificant association
#temperature:significant association
#wind:significant association
#findings are consistent with earlier descriptive plots

#R^2:0.6062
#adjusted R^2:0.5952
#decent fitting model between explanatory and response variables
#explains 60% of ozone level variance

#radiation coef(0.05980):the more the radiation, the more the ozone levels
#>ozone level increases by 0.05980 ppb per langley
#temperature coef (1.65121): the higher the temperature, the more the ozone levels
#>ozone level increases by 1.65121 ppb per farenheit
#wind coef (-3.33760): the faster the wind, the less the ozone levels
#>ozone level decreases by 3.33760 ppb per mile per hour

#check residuals
par(mfrow=c(1,1))
par(mfrow=c(1,2))
par(mfrow=c(1,3))
par(mfrow=c(2,2))
par(mfrow=c(2,3))
par(mfrow=c(3,3))

#plots
plot(mod,which=c(1,2))
#residuals vs fitted: clear pattern/shape, outliers, change in variance
#>organized/aligned points
#>variance of residuals not constant
#>non-flat average line reveals some linearity
#normal q-q: mostly linear but outliers at extremities
#>not all points on the diagonal: data not perfectly modelled by linear relationship
#>standardised residuals from an almost/approximate normal distribution
#>slight non-linear structure in residuals 
#>slight deviance from normal distribution

#prediction model for ozone levels where variables are max
predict(mod,
        newdata=data.frame(Chlorophyll=NA,Temperature=27.4260,Windspeed=4.65125,Currents=0.2755,Sea_Surface_Salinity=36.398,Net_Primary_Production=1425.422))
#ozone levels: 46.8

#prediction model for ozone levels where variables are min
predict(mod,
        newdata=data.frame(Chlorophyll=NA,Year=NA,Temperature=57,Month=NA))
#ozone levels: 22.6

#ggplot(df,aes(x=radiation,y=ozone,color=temperature)) + geom_point() +
#  geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2],color="salmon") +
#  geom_abline(intercept = coef(mod)[1] + coef(mod)[3], slope = coef(mod)[2] +
#                coef(mod)[4], color="cyan")

#Comment on the summary of the model. What do these coefficients suggest about the relationship
#between ozone and the other variables? Are these findings consistent with your earlier descriptive
#plots? Also include suitable residual plots, commenting as appropriate.

#(c) [10 marks] A colleague suggests you implement the following model,
#log(ozonei) = β0+β1 log(radiationi)+β2 log(temperaturei
#)+β3 log(windi)+ϵi where ϵi ∼ N(0, σ2).
mod2<-lm(log(Chlorophyll)~log(Temperature)+log(Windspeed)+log(Currents)+log(Sea_Surface_Salinity)+log(Net_Primary_Production),data=PointB)

#ggplot(PointB,aes(x=log(Year),y=log(Chlorophyll)))+
#  geom_point()+
#  geom_line(aes(x=log(Year),y=fitted(mod)),colour="red",)+
#  labs(x="log(Year)",y="log(Chlorophyll)")

ggplot(PointB,aes(x=log(Temperature),y=log(Chlorophyll)))+
  geom_point()+
  geom_line(aes(x=log(Temperature),y=fitted(mod)),colour="blue",)+
  labs(x="log(Temperature)",y="log(Chlorophyll)")

ggplot(PointB,aes(x=log(Windspeed),y=log(Chlorophyll)))+
  geom_point()+
  geom_line(aes(x=log(Windspeed),y=fitted(mod)),colour="green",)+
  labs(x="log(Windspeed)",y="log(Chlorophyll)")

ggplot(PointB,aes(x=log(Currents),y=log(Chlorophyll)))+
  geom_point()+
  geom_line(aes(x=log(Currents),y=fitted(mod)),colour="red",)+
  labs(x="log(Currents)",y="log(Chlorophyll)")

ggplot(PointB,aes(x=log(Net_Primary_Production),y=log(Chlorophyll)))+
  geom_point()+
  geom_line(aes(x=log(Net_Primary_Production),y=fitted(mod)),colour="blue",)+
  labs(x="log(NPP)",y="log(Chlorophyll)")

ggplot(PointB,aes(x=log(Sea_Surface_Salinity),y=log(Chlorophyll)))+
  geom_point()+
  geom_line(aes(x=log(Sea_Surface_Salinity),y=fitted(mod)),colour="green",)+
  labs(x="log(SSS)",y="log(Chlorophyll)")

#ggplot(df,aes(x=log(wind),y=log(ozone)))+
#  geom_point()+
#  geom_smooth(method = "lm")+
#  labs(x="test",y="tset")

#summary
summary(mod2)
#based on respective p-values:
#log(radiation): significant associations
#log(temperature): significant association
#log (wind):significant association

#R^2: 0.6876
#adjusted R^2: 0.6788
#good fitting model between expalanatory and response variables
#explains 68% of log(ozone levels) variance

#log radiation coef(0.30500): the more the log radiation, the more the log ozone levels
#log ozone levels increase by 0.30500 log ppb per log langley
#log temperature coef(3.20478): the higher the log temperature, the more the log ozone levels
#log ozone levels increase by 3.20478 log ppb per log farenheit
#log wind coef(-0.66305): the faster the log wind, the less the log ozone levels
#log ozone levels decrease by 0.66305 log ppb per log mile per hour

#compare coefficients:
#higher log coefficient values
#>log coefficients more meaningful
#log coefficients are more significant
#>log shows distinctive and more fitting relationships

#Fit this new model to the data to obtain estimates for the regression coefficients. Produce a plot of
#the residuals against the fitted values, and a Q-Q plot of the residuals. 

#residuals
par(mfrow=c(1,1))
par(mfrow=c(1,2))
par(mfrow=c(1,3))
par(mfrow=c(2,2))
par(mfrow=c(2,3))
par(mfrow=c(3,3))

#plots
plot(mod2,which = c(1,2))
#residuals vs fitted: no pattern, no obvious outliers, no change in variance, 
#>shapeless cloud of points
#>constant variance of residuals
#>non-flat average line reveals slight linearity
#normal q-q:very good linearity
#>all points arranged on the diagonal: data perfectly modelled by linear relationship
#>standardised residuals from standard normal distribution
#>linear structure of residuals
#>no deviance from normal distribution

#compare plots:
#r vs f: log model fits better than original model
#q-q: log model fits better than original model
#log model better confirms and explain assumptions and patterns
#>non linear relationships/skewed variables transormed to normal-shaped 
#reduce skewness, overfitting, errors
#>improve fitting and prediction

#predict model for ozone levels where variables are max
predict(mod2,
        newdata=data.frame(Chlorophyll=NA,Year=NA,Temperature=5.811141,Month=NA))
#log ozone levels: -5.152713 
#Chlorophyll vs NPP,SSS

#predict model for ozone levels where variables are min
predict(mod2,
        newdata=data.frame(Chlorophyll=NA,Year=NA,Temperature=1.945910,Month=NA))                                          
#log ozone levels: -Inf

#Comment on the outputs
#from the modelling (comparing it to the previously fitted model), paying particular attention to
#the interpretation of the coefficients. Express the impact of the explanatory


#heatmap
heatmap(as.matrix(PointB2, ColV = NA, Rowv = NA, scale = "column"))

# Libraries
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("plotly")
install.packages("d3heatmap")
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)

# Select a few country
data <- data %>% 
  filter(Country %in% c("France", "Sweden", "Italy", "Spain", "England", "Portugal", "Greece", "Peru", "Chile", "Brazil", "Argentina", "Bolivia", "Venezuela", "Australia", "New Zealand", "Fiji", "China", "India", "Thailand", "Afghanistan", "Bangladesh", "United States of America", "Canada", "Burundi", "Angola", "Kenya", "Togo")) %>%
  arrange(Country) %>%
  mutate(Country = factor(Country, Country))

# Matrix format
mat <- data
rownames(mat) <- mat[,1]
mat <- mat %>% dplyr::select(-Country, -Group, -Continent)
mat <- as.matrix(PointB)

# Heatmap
d3heatmap(mat, scale="column", dendrogram = "none", width="800px", height="80Opx", colors = "Blues")

library(heatmaply)
p <- heatmaply(mat, 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Country", "Feature:", "Value"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(mat),
               labRow = rownames(mat),
               heatmap_layers = theme(axis.line=element_blank())
)



# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
ggplot(PointB, aes(Chlorophyll, Net_Primary_Production, fill= Sea_Surface_Salinity)) + 
  geom_tile()


# Library
library(ggplot2)
library(hrbrthemes)
library(plotly)

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# new column: text for tooltip:
data <- data %>%
  mutate(text = paste0("x: ", x, "\n", "y: ", y, "\n", "Value: ",round(Z,2), "\n", "What else?"))

# classic ggplot, with text in aes
p <- ggplot(data, aes(X, Y, fill= Z, text=text)) + 
  geom_tile() +
  theme_ipsum()

ggplotly(p, tooltip="text")


# Library
library(ggplot2)
library(tidyr)
library(tibble)
library(hrbrthemes)
library(dplyr)

# Volcano dataset
#volcano

# Heatmap 
volcano %>%
  
  # Data wrangling
  as_tibble() %>%
  rowid_to_column(var="X") %>%
  gather(key="Y", value="Z", -1) %>%
  
  # Change Y to numeric
  mutate(Y=as.numeric(gsub("V","",Y))) %>%
  
  # Viz
  ggplot(aes(X, Y, fill= Z)) + 
  geom_tile() +
  theme_ipsum() +
  theme(legend.position="none")


write.csv(PointB, "C:\\Users\\karan\\OneDrive\\Documents\\OneDrive\\Documents\\Karan\\MTHM507\\PointB.csv", row.names=FALSE)
write.csv(PB, "C:\\Users\\karan\\OneDrive\\Documents\\OneDrive\\Documents\\Karan\\MTHM507\\PB.csv", row.names=FALSE)












