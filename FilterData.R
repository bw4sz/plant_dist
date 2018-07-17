#Filter data

library(dplyr)
library(anytime)
library(chron)
library(ggplot2)
library(stringr)
library(tidyr)
library(taxize)
library(lubridate)
###2013-2017 data
#hummingbird interactions
interactions_2013<-read.csv("data/HummingbirdInteractions.csv",row.names=1)
interactions_2013<-interactions_2013 %>% filter(!is.na(ID)) %>% select(ID,Date=DateP,Hummingbird,Plant=Iplant_Double,Time,Sex,piercing=Pierce,lon,lat,ele,waypoint=name)

#convert to latin names
get_latin<-comm2sci(levels(interactions_2013$Hummingbird),simplify = T)
to_join<-data.frame(Hummingbird=names(get_latin),latin=sapply(get_latin,function(x) word(x,1,2)[[1]]))

interactions_2013<-interactions_2013 %>% inner_join(to_join) %>% select(-Hummingbird) %>% rename(Hummingbird=latin)

#make posix data 
interactions_2013$cDate<-as.POSIXct(interactions_2013$Date)

interactions_2017<-read.csv("data/Interactions.csv",row.names=1)

#Todo check revised
interactions_2017<-interactions_2017 %>% filter(site %in% c("Maquipucuna","SantaLuciaUpper","SantaLuciaLower")) %>% select(Date=date,Time=time,lon,lat,ele,site,waypoint,Hummingbird=hummingbird,piercing,Plant=final_plant_name)

#make posix data 
interactions_2017$cDate<-as.POSIXct(interactions_2017$Date,format="%d/%m/%Y")
interactions_2017$cDate[years(interactions_2017$cDate) %in% c("17","18")]<-as.POSIXct(interactions_2017$Date[years(interactions_2017$cDate) %in% c("17","18")],format="%d/%m/%y")

interactions<-bind_rows(interactions_2013,interactions_2017)

#Plant list
gesner<-c("Columnea ciliata","Columnea kucyniakii","Drymonia tenuis","Columnea mastersonii","Gasteranthus quitensis","Drymonia teuscheri",
  "Columnea strigosa","Glossoloma oblongicalyx","Gasteranthus lateralis","Kohleria affinis","Besleria solanoides","Drymonia brochidodroma",
  "Drymonia collegarum","Alloplectus purpuruem","Columnea picta","Kohleria villosa","Columnea medicinalis","Gasteranthus pansamalanus","Glossoloma purpureum",
  "Kohleria spicata")

#subset gesners and drop flowerpiercer
ginteractions<-interactions %>% filter(Plant %in% gesner,!Hummingbird %in% "Diglossa albilatera") 

#fix one taxonomic disagrement
ginteractions[ginteractions$Plant == "Alloplectus purpuruem" ,"Plant"]<-"Glossoloma purpureum"

#confirm dates are clean
ggplot(ginteractions %>% group_by(cDate) %>% summarize(n=n()),aes(x=cDate,y=n)) + geom_point() + geom_line()

ginteractions<-ginteractions %>% select(-Date,Date=cDate,-site)

#clean elevations?
ginteractions<-ginteractions[!is.na(ginteractions$ele),]

#interaction plot
int_plot<-ginteractions %>% group_by(Hummingbird,Plant) %>% summarise(n=n()) 

#order factors
hord<- ginteractions %>% group_by(Hummingbird) %>% summarize(n=n()) %>% arrange(desc(n)) %>% .$Hummingbird
int_plot$Hummingbird<-factor(int_plot$Hummingbird,levels=rev(hord))
pord<- ginteractions %>% distinct(Plant) %>% arrange(desc(Plant)) %>% .$Plant
int_plot$Plant<-factor(int_plot$Plant,levels=pord)
ggplot(int_plot,aes(x=Hummingbird,y=Plant,fill=n)) + geom_tile() + coord_flip()  + scale_fill_continuous(low="blue",high="red") + theme_bw()+ theme(axis.text.x = element_text(angle=-90))

#flower phenology
#2013
fl<-read.csv("data/FlowerTransectClean.csv")

fl[fl$Iplant_Double %in% "Alloplectus tetragonoides","Iplant_Double"]<- "Drymonia collegarum"

#select columns and gesneriaceae
transects_2013<-fl %>% select(Plant=Iplant_Double,Observer,Flowers=Total_Flowers,Date=Date_F,lon,lat,ele) %>% filter(Plant %in% gesner,!is.na(ele)) %>% mutate(Flowers=as.numeric(Flowers))

#make posix date columns
transects_2013$Date<-as.POSIXct(transects_2013$Date)

#2017
transects_2017<-read.csv("data/PlantTransects.csv",row.names=1)
transects_2017<-transects_2017  %>% filter(final_plant_name %in% gesner,site %in% c("Maquipucuna","SantaLuciaUpper","SantaLuciaLower") ) %>% 
select(Plant=final_plant_name,Flowers=total_flowers,Date=date,lon,lat,ele) 

#date columns
transects_2017$Date<-as.POSIXct(transects_2017$Date,format="%d/%m/%Y")
transects<-bind_rows(transects_2013,transects_2017)

#reduce observer effects, take out karen's data
transects<-transects %>% filter(chron::years(Date) >= 2014,!Observer=="Karen" ) %>% select(-Observer)

#view dates
transects %>% group_by(Date,Plant) %>% summarize(s=sum(Flowers)) %>% ggplot(.,aes(x=Date,y=s)) + facet_wrap(~Plant,scales="free") + geom_point() +theme_bw()

#seaonally
transects %>% mutate(Month=months(Date),Year=years(Date)) %>% group_by(Year,Month,Plant) %>% summarize(s=sum(Flowers)) %>% ggplot(.,aes(x=Month,y=s,col=Year)) + facet_wrap(~Plant,scales="free") + geom_line(aes(group=Year)) +theme_bw() 

#julian day
transects<-transects %>% mutate(Month=months(Date),Year=chron::years(Date),julian=yday(Date))

ggplot(transects,aes(x=julian,y=Flowers)) + facet_wrap(~Plant,scales="free") + geom_line()

#write files
write.csv(ginteractions,"data/cleaned/interactions.csv")
write.csv(transects,"data/cleaned/transects.csv")
