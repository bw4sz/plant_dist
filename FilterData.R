#Filter data

library(dplyr)

###2013-2017 data
#hummingbird interactions
interactions_2013<-read.csv("data/HummingbirdInteractions.csv",row.names=1)
interactions_2013<-interactions_2013 %>% filter(!is.na(ID)) %>% select(ID,Date=DateP,Hummingbird,Plant=Iplant_Double,Time,Sex,piercing=Pierce,lon,lat,ele,waypoint=name)

interactions_2017<-read.csv("data/Interactions.csv",row.names=1)

#TOdo cehck revised
interactions_2017<-interactions_2017 %>% filter(site %in% c("Maquipucuna","SantaLuciaUpper","SantaLuciaLower")) %>% select(Date=date,Time=time,timestamp,lon,lat,ele,site,waypoint,Hummingbird=hummingbird,piercing,Plant=final_plant_name)
interactions<-bind_rows(interactions_2013,interactions_2017)

#Plant list
c("Columnea ciliata","Columnea kucyniakii","Drymonia tenuis","Columnea mastersonii","Gasteranthus quitensis","Drymonia teuscheri","Columnea strigosa")
unique(interactions$Plant)

#flower phenology
fl<-read.csv("data/FlowerTransectClean.csv")

