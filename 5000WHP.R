#load library
library(tidyverse)
library(viridis)
library(cowplot)
library(imager)
library(lubridate)

Sprints<-read.delim("Sprints.txt",header=TRUE,sep = ",",fileEncoding="UTF-8-BOM",stringsAsFactors = FALSE)
Sprints<-Sprints %>%
  mutate(WPH =Count*(60/Time))%>%
  mutate_all(~replace(., is.na(.), 0))%>% 
  mutate(Date=as.Date(Date, format="%d/%m/%Y"))%>%
  arrange(Date)

Progress <- Sprints%>%
  group_by(Date)%>%
  summarise(WPH=mean(WPH))

Consistency <- Sprints%>%
  group_by(Date)%>%
  summarise(WPD=sum(Count))

WriMo<-Consistency%>%
  mutate(month = format(Date,"%m"))%>%
  group_by(month)%>%
  mutate(monthly=cumsum(WPD))

Summary <- merge(Progress,Consistency, by="Date")
Averages <- Summary %>% mutate(month = format(Date,"%m"),
                               year  = format(Date,"%Y"))%>% 
  group_by(month,year) %>% 
  summarise(WPH = mean(WPH),WPD=mean(WPD)) 

progress <- ggplot(Progress, aes(x=Date, y= WPH, fill=WPH))+
  geom_hline(yintercept=383, colour="blue")+
  geom_bar(stat="identity", width=1)+
  scale_fill_viridis(option="turbo")+
  ylab("average words per hour")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+theme_bw()

consistency<-
ggplot(Consistency, aes(x=Date, y= WPD, fill=WPD))+
  geom_hline(yintercept=25, colour="blue")+
  geom_bar(stat="identity", width=1)+
  scale_fill_viridis(option="plasma",direction=-1)+
  ylab("daily words")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+theme_bw()

png("daily.png")
consistency+theme(legend.position="none")
dev.off()

WriMos<-ggplot(WriMo,aes(x=Date,y=monthly,fill=monthly))+
  geom_bar(stat="identity")+
  scale_fill_viridis(option="turbo")+
  ylab("progress")+
  theme_bw()

October<-WriMo%>%
  filter(month=="10")%>%
  complete(Date=seq.Date(min(Date),as.Date("2021-10-31"),by="day"))%>% 
  mutate(year=format(Date,"%Y"))%>%group_by(year)%>%mutate(goal=seq(from=1667,to=50000,length.out=31)) 
October%>%
  ggplot(aes(Date))+
  geom_bar(aes(y=goal,fill=goal),stat="identity",position="dodge",width=1,alpha=0.5)+
  geom_bar(aes(y=monthly,fill=monthly),stat="identity",position="dodge",width=1)+
  scale_fill_viridis(option="turbo")+
  labs(fill="words")+ylab("Words")


NaNoWriMo <- data.frame(goal=seq(from=1667,to=50000,by=1667))

View(October)
View(Averages)
load.image("daily.png")%>%plot(axes=F)
