#Filter data
library(anytime)
library(chron)
library(ggplot2)
library(stringr)
library(tidyr)
library(taxize)
library(lubridate)
library(dplyr)

#Plant list
gesner<-c("Columnea ciliata","Columnea kucyniakii","Drymonia tenuis","Columnea mastersonii","Gasteranthus quitensis","Drymonia teuscheri",
  "Columnea strigosa","Glossoloma oblongicalyx","Gasteranthus lateralis","Kohleria affinis","Besleria solanoides","Drymonia brochidodroma",
  "Drymonia collegarum","Alloplectus purpuruem","Columnea picta","Kohleria villosa","Columnea medicinalis","Gasteranthus pansamalanus","Glossoloma purpureum",
  "Kohleria spicata")

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
select(Plant=final_plant_name,Flowers=total_flowers,Date=date,lon,lat,ele)  %>% mutate(Date=as.character(Date))

#Do some date cleaning
transects_2017$Date[str_detect(transects_2017$Date,"/18")]<-as.character(as.POSIXct(transects_2017$Date[str_detect(transects_2017$Date,"/18")],format="%m/%d/%y"))
transects_2017$Date[str_detect(transects_2017$Date,"/17")]<-as.character(as.POSIXct(transects_2017$Date[str_detect(transects_2017$Date,"/17")],format="%m/%d/%y"))

#one crazy date
transects_2017$Date[transects_2017$Date %in% "24//10/2018"]<-"24/10/2018"

#if no slashes, its a number
check_date<-function(x){
  print(x)
  if(str_detect(x,"\\/")){
    newdate<-as.POSIXct(x,format="%d/%m/%Y")
    return(newdate)
  } 
  else{
    newdate<-anydate(x)
    return(newdate)
  }
}

for(x in 1:nrow(transects_2017)){
  transects_2017$Date[x]<-as.character(check_date(transects_2017$Date[x]))
}

#Enfore posix
transects_2017$Date<-as.POSIXct(transects_2017$Date)

#date columns
transects<-bind_rows(transects_2013,transects_2017)

#reduce observer effects, take out karen's data
transects<-transects %>% filter(!Observer %in% "Karen") %>% select(-Observer)

#view dates
transects %>% group_by(Date,Plant) %>% dplyr::summarize(s=sum(Flowers)) %>% ggplot(.,aes(x=Date,y=s)) + facet_wrap(~Plant,scales="free") + geom_point() +theme_bw()

#write files
write.csv(transects,"data/cleaned/transects.csv")
