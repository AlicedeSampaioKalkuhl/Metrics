#load library
library(tidyverse)
library(viridis)
library(cowplot)
library(imager)
library(lubridate)

Sprints <- read.delim("Sprints.txt", header=TRUE, sep = ",",fileEncoding="UTF-8-BOM",stringsAsFactors = FALSE)
Sprints <- Sprints %>%
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
  geom_hline(yintercept=604, colour="blue")+
  geom_bar(stat="identity", width=1)+
  scale_fill_viridis(option="plasma", direction=-1)+
  ylab("average words per hour")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+theme_bw()

png("daily.png",)
ggplot(Consistency, aes(x=Date, y= WPD, fill=WPD))+
  geom_hline(yintercept=125, colour="blue")+
  geom_bar(stat="identity", width=1)+
  scale_fill_viridis(option="plasma", direction=-1)+
  ylab("daily words")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+theme_bw()
dev.off()

monthly <- plot_grid(
  ggplot(Averages, aes(x=month, y= WPH, fill=WPH))+
    geom_bar(stat="identity", width=1)+
    scale_fill_viridis(option="plasma", direction=-1)+
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    facet_wrap(~year),
  ggplot(Averages, aes(x=month, y= WPD, fill=WPD))+
    geom_bar(stat="identity", width=1)+
    scale_fill_viridis(option="plasma", direction=-1)+
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    facet_wrap(~year),
  ncol=1, align="v")



WriMos<-ggplot(WriMo,aes(x=Date,y=monthly,fill=monthly))+
  geom_bar(stat="identity")+
  scale_fill_viridis(option="plasma", direction=-1)+
  theme_bw()




View(Averages)
View(Summary)
load.image("daily.png")%>%plot(axes=F)

