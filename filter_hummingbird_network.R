#Filter data for hummingbird network

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
levels(to_join$latin)[levels(to_join$latin) %in% "Thalurania colombica"]<-"Thalurania fannyi"

interactions_2013<-interactions_2013 %>% inner_join(to_join) %>% select(-Hummingbird) %>% rename(Hummingbird=latin)

#make posix data 
interactions_2013$cDate<-as.POSIXct(interactions_2013$Date)

interactions_2017<-read.csv("data/Interactions.csv",row.names=1)

#Todo check revised
interactions_2017<-interactions_2017 %>% filter(site %in% c("Maquipucuna","SantaLuciaUpper","SantaLuciaLower")) %>% select(Date=date,Time=time,lon,lat,ele,site,ID=waypoint,Hummingbird=hummingbird,piercing,Plant=final_plant_name)

#make posix data 
interactions_2017$cDate<-as.POSIXct(interactions_2017$Date,format="%d/%m/%Y")
interactions_2017$cDate[years(interactions_2017$cDate) %in% c("17","18")]<-as.POSIXct(interactions_2017$Date[years(interactions_2017$cDate) %in% c("17","18")],format="%d/%m/%y")
interactions<-bind_rows(interactions_2013,interactions_2017)

# drop flowerpiercer
ginteractions<-interactions %>% filter(!is.na(Plant),!Hummingbird %in% c("Diglossa albilatera","Diglossa cyanea","Euphonia saturata")) 

#fix one taxonomic disagrement
ginteractions[ginteractions$Plant == "Alloplectus purpuruem" ,"Plant"]<-"Glossoloma purpureum"

#remove very rare plants

too_rare<-ginteractions %>% group_by(Plant) %>% summarize(days=length(unique(Date))) %>% arrange(desc(days)) %>% filter(days < 5)
ginteractions<-ginteractions %>% filter(!Plant %in% too_rare$Plant,!is.na(Hummingbird))

#confirm dates are clean
ggplot(ginteractions %>% group_by(cDate) %>% summarize(n=n()),aes(x=cDate,y=n)) + geom_point() + geom_line()
ginteractions<-ginteractions %>% select(-Date,Date=cDate,-site)

#interaction plot
int_plot<-ginteractions %>% group_by(Hummingbird,Plant) %>% summarise(n=n()) 

#order factors
hord<- ginteractions %>% group_by(Hummingbird) %>% summarize(n=n()) %>% arrange(desc(n)) %>% .$Hummingbird
int_plot$Hummingbird<-factor(int_plot$Hummingbird,levels=rev(hord))
pord<- ginteractions %>% distinct(Plant) %>% arrange(desc(Plant)) %>% .$Plant
int_plot$Plant<-factor(int_plot$Plant,levels=pord)
ggplot(int_plot,aes(x=Hummingbird,y=Plant,fill=n)) + geom_tile() + coord_flip()  + scale_fill_continuous(low="blue",high="red") + theme_bw()+ theme(axis.text.x = element_text(angle=-90))

write.csv(ginteractions,"data/cleaned/interactions_all.csv")
