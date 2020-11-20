#GLOBAL TERRORISM EDA - Fathima Sadique 


#IMPORTING THE REQUIRED LIBRARIES FOR THIS ANALYSIS:
library(rgeos)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggpubr)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("data.table")


#IMPORTING THE GLOBAL TERRORISM DATASET:
setwd("C:/Users/91886/Desktop/internship/TSF")
task_data<-read_excel("Terrorism.xlsx")
View(head(task_data))
ncol(task_data)#No. of columns 
nrow(task_data)#No. of rows 


#DESCRIPTION OF THE DATASET:
#The above data set contains information about terrorists attack that ranged from the years 1970 to 2017. It contains 136 fields and 181691 records. Following a unique eventid for each attack, it contains detailed information about the location, region, mode of attack, targeted categories and weapons used among many other fields. To analyse this data as a Security Analyst, we will be looking into the most affected regions around the globe and the needful security measures that can be taken. 


#MOST ACTIVE YEARS OF TERRORISM: 

#The following graph shows the rate of active terrorism during different years:
ggdensity(task_data, x = "iyear", xlab='Years', ylab='Density', 
          fill = "#6C93B4FF", color = "#6C93B4FF", rug = TRUE)
#From the above graph it is evident that a peak in terrorism is seen somewhere in between 2010 and 2020. 


#Since an evident peak is seen after 2010, let us take a closer look at the number of events from 2010 to 2017:
years<-subset(task_data, task_data$iyear>=2010)
years_tab<-table(years$iyear)
years_df<-as.data.frame(years_tab)
years_df %>%
  ggplot(aes(x=Var1, y=Freq, fill=Freq))+
  geom_col() + ggtitle("No. of attacks vs years (2010 - 2017") +
  xlab("Years") + ylab("Count") + labs(fill = "Count")


#REGIONS MOST AFFECTED SINCE 1970:-

Regions_tab<-table(task_data$region_txt)
Regions<-as.data.frame(Regions_tab)
ggplot(data=Regions, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity",fill="snow1") + coord_flip() + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid")) + ggtitle("Regions vs No. of total attacks") +
  xlab("Regions") + ylab("Frequency")
#From the above frequency plot it is clear that the Middle East and North Africa is the most affected region followed by South Asia and South America

#If we visualize with the help of a map, the region with the most number of attacks can be marked as follows:-

#Middle-East and North Africa
world <- ne_countries(scale = "medium", returnclass = "sf")
MiddEast<-subset(task_data,task_data$region_txt=='Middle East & North Africa')
ggplot(data = world) +
  geom_sf() +
  geom_point(data = MiddEast, aes(x = longitude, y = latitude), na.rm=T,size = 1, 
             shape = 23, fill = "indianred3", color="indianred4") + coord_sf(xlim = c(-20, 80), ylim = c(-40, 55), expand = FALSE) + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))

#South Asia
SouthAsia<-subset(task_data,task_data$region_txt=='South Asia')
ggplot(data = world) +
  geom_sf() +
  geom_point(data = SouthAsia, aes(x = longitude, y = latitude), na.rm=T,size = 1, 
             shape = 23, fill = "indianred3", color="indianred4") + coord_sf(xlim = c(50, 120), ylim = c(0, 50), expand = FALSE) + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))

#South America
SouthAmerica<-subset(task_data,task_data$region_txt=='South America')
ggplot(data = world) +
  geom_sf() +
  geom_point(data = SouthAmerica, aes(x = longitude, y = latitude), na.rm=T,size = 1, 
             shape = 23, fill = "indianred3", color="indianred4") + coord_sf(xlim = c(-100, 0), ylim = c(-65, 20), expand = FALSE) + theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",size = 2, linetype = "solid"))


#Top 5 most affected countries:
task_data$country_txt=as.factor(task_data$country_txt)
Countries_tab<-table(task_data$country_txt)
Countries<-as.data.frame(Countries_tab)
Countries <- Countries[order(Countries$Freq,decreasing = TRUE),]
top5<- Countries[1:5,]
names(top5)[names(top5) == "Var1"] <- "Countries"
names(top5)[names(top5) == "Freq"] <- "Number of attacks"
setDT(top5)

#To visualize this we have a frequency chart showing the extent of terrorism in the top 5 affected countries:
top5 = top5[order(top5$`Number of attacks`),]
dotchart(top5$`Number of attacks`, labels = top5$`Countries`, col = c("skyblue","skyblue1","skyblue2","skyblue3","skyblue4"),
         cex = 0.8,pch=19, xlab = "Count of terrorist attacks")

#From the above analysis we can say that the major hotspots are Iraq, Pakistan and Afghanistan. To look further into this let us visualize what the count of attacks looks like, in the provinces of each of these countries :-

#First we make 3 different subsets of the original data set, for each of the countries:
Iraq<-subset(task_data,task_data$country_txt=='Iraq')
Pak<-subset(task_data,task_data$country_txt=='Pakistan')
Afg<-subset(task_data,task_data$country_txt=='Afghanistan')

#Extracting the top 5 provinces of Iraq:
Iraq_tab<-table(Iraq$provstate)
Iraq_prov<-as.data.frame(Iraq_tab)
Iraq_prov <- Iraq_prov[order(Iraq_prov$Freq,decreasing = TRUE),]
Iraq_5 <- Iraq_prov[1:5,]
Iraq_5$Country<-"Iraq"
#Extracting the top 5 provinces of Pakistan:
Pak_tab<-table(Pak$provstate)
Pak_prov<-as.data.frame(Pak_tab)
Pak_prov$Country<-"Pakistan"
Pak_prov <- Pak_prov[order(Pak_prov$Freq,decreasing = TRUE),]
Pak_5 <- Pak_prov[1:5,]
#Extracting the top 5 provinces of Afghanistan: 
Afg_tab<-table(Afg$provstate)
Afg_prov<-as.data.frame(Afg_tab)
Afg_prov$Country<-"Afghanistan"
Afg_prov <- Afg_prov[order(Afg_prov$Freq,decreasing = TRUE),]
Afg_5 <- Afg_prov[1:5,]
#Binding all the three tables together row after row:
prov_tab<-rbind(Iraq_5,Pak_5,Afg_5)
prov_tab$Country<-as.factor(prov_tab$Country)

#Visualization of the most affected provinces:-
grps <- prov_tab$Country
my_cols <- c("turquoise4", "aquamarine4", "paleturquoise4")
dotchart(prov_tab$Freq, 
         groups = grps, gcolor = my_cols, ,
         color = my_cols[grps], labels=prov_tab$Var1,
         cex = 0.85,  pch = 19, main="Provinces of the 3 major hotspots", xlab = "Frequency")


#INFERENCE : From the above visualizations we can conclude that the major hotspots of terrorist attacks are Iraq, Pakistan and Afghanistan with provinces like Baghdad, Saladin, Balochistan, Sindh, Kandahar and Helmand.  


#SECURITY ISSUES AND OTHER INSIGHTS DERIVED FROM THE DATASET :-

#Different modes of attack and the extent to which they were successful:
succ<-with(task_data, table(attacktype1_txt, success))
success_df<-data.frame("Success"=succ[,2],"Failure"=succ[,1])
success_df$Percent_Of_Success<-round((success_df$Success/(success_df$Success+success_df$Failure))*100)
success_df$tot=success_df$Success+success_df$Failure
success_df<-success_df[order(success_df$tot,decreasing=T),]
Success_rate<- subset(success_df, select = -c(Success, Failure) )
Success_rate
#The above table shows that almost 95% of the bombing events were successful and also the most frequent. To look more into the success rates we have a visualization below.


#Visualization of the success rate of each attack mode: 
task_data$attacktype1_txt=as.factor(task_data$attacktype1_txt)
bar1=ggplot(data=task_data, mapping=aes(x=attacktype1_txt, fill=as.factor(success)))
bar1+geom_bar(width=0.4) + coord_flip() + scale_fill_manual(values=c("seashell", "thistle4"))+ theme(panel.background = element_rect(fill = "gray16", colour = "#6D9EC1",size = 2, linetype = "solid")) + ggtitle("Mode of attack vs Number of attacks") + xlab("Mode of Attack") + ylab("Count") + labs(fill = "Success - 1 / Failure - 0")
#Even though bombing, armed assaults and assassination were the most frequent modes of attack, we see that almost all the attacks that involved hostage taking and hijacking were successful whereas in the former three not all were successful indicating an improvement in airport security. 


#Density Plot to show the spread of the three most common modes of attack over the years from 1970 to 2017:
attack_freq<-table(task_data$attacktype1_txt)
attack_freq<-as.data.frame(attack_freq)
attack_freq<-attack_freq[order(attack_freq$Freq,decreasing = TRUE),]
topattack_sub<-subset(task_data,task_data$attacktype1_txt=="Bombing/Explosion"|task_data$attacktype1_txt=="Armed Assault"|task_data$attacktype1_txt=="Assassination")
topattack_sub$attacktype1_txt<-factor(topattack_sub$attacktype1_txt)
ggplot(data=topattack_sub, mapping=aes(x=iyear, fill=as.factor(attacktype1_txt)))+geom_density(alpha=0.5) + theme(panel.background = element_rect(fill = "azure2", colour = "#6D9EC1",size = 2, linetype = "solid")) + ggtitle("Density plot for the top 3 frequent modes of attack") + xlab("Years") + ylab("Density") + labs(fill = "Mode of attack")
#We see that attacks involving bombing and armed assault start showing a sudden peak after 2005, increasing to a greater extent than before, unlike assassinations that go back to their normal rate after the dip between 2000 to 2005. The 'dip' in the rates of all three modes of attack between 2000 and 2005 might shed light on advanced security measures internally and externally after the 9/11 attacks in New York that occured in 2001.  


#The most targeted categories and its visualization using a frequency dot chart:
target_tab<-table(task_data$targtype1_txt)
target_type<-as.data.frame(target_tab)
dotchart(target_type$Freq, labels = target_type$Var1,
         cex = 0.6, main='Targeted categories frequecy chart', xlab='Count', pch = 19, col = c("darkblue","dodgerblue"),
         cex.main = 2, cex.lab = 1.5)


#Since the targetted categories are vast we condense them into smaller classification (Public related/ Political related / Others) for an easier analysis

task_data$Classification <- ifelse(task_data$targtype1_txt == 'Utilities' | task_data$targtype1_txt == 'Transportation' | task_data$targtype1_txt == 'Tourists' | task_data$targtype1_txt == 'Religious Figures/Institutions' | task_data$targtype1_txt == 'Educational Institution' | task_data$targtype1_txt == 'Airports & Aircraft' | task_data$targtype1_txt == 'Telecommunication' | task_data$targtype1_txt == 'Private Citizens & Property' | task_data$targtype1_txt == 'NGO' | task_data$targtype1_txt == 'Business', 'Public',
                  ifelse(task_data$targtype1_txt == 'Violent Political Party' | task_data$targtype1_txt == 'Terrorists/Non-State Militia' | task_data$targtype1_txt == 'Police' | task_data$targtype1_txt == 'Military' | task_data$targtype1_txt == 'Maritime' | task_data$targtype1_txt == 'Journalists & Media' | task_data$targtype1_txt == 'Government (General)' | task_data$targtype1_txt == 'Government (Diplomatic)' | task_data$targtype1_txt == 'Food or Water Supply', 'Political', 'Other'))


#Weapons used under different circumstances: 
task_data$weaptype1_txt[task_data$weaptype1_txt == 'Vehicle (not to include vehicle-borne explosives, i.e., car or truck bombs)'] <- 'Vehicular'
task_data %>% ggplot(aes(x = weaptype1_txt, fill=Classification)) + geom_bar(position='dodge',width=0.6) + scale_fill_manual(values=c("lightblue3", "lightcoral","lightblue4"))+coord_flip() +  theme(panel.background = element_rect(fill = "grey90", colour = "grey69",size = 2, linetype = "solid")) + ggtitle("Frequency plot for weapons used under different circumstances") + xlab("Type of Weapon") + ylab("Count of attacks") + labs(fill = "Classification")
#We see that for public related attacks, the use of explosives is vast followed by firearms, though they are used to a larger extent in political related attacks. There are a few cases of melee and chemical substance attacks as well under public attacks though not as much as the former. As stated before, political attacks mainly involves firearms and explosives, though explosives are used to a larger extent in public places.  


#INFERENCE : From the above visualizations, it is evident that most of these attacks involved bombing and explosive gadgets followed by assault and assassination. Though the cases of use of chemical, biological and radiological substances as weapons are uncommon, they aren't completely unheard of. To make this easier I have further classified these events into public related, political related and others since different circumstances call for different security measures. In public places, we again have explosives as the leading weapon and in political related attacks we have explosives as well as firearms. We also see that almost all cases of hijacking, hostage taking and facility attacks were successful over the years with a success rate higher than 97%. 2001 and 2014 were two years that witnessed some brutal terrorist attacks and after both these years we can see a clear dip in the rates of terrorist attacks. Advanced security protocols after the happening of such events can be one of the reasons for this decrease, proving that it is not completely impossible to take the neccessary safety and security measures when absolutely required, by the authorities.  


