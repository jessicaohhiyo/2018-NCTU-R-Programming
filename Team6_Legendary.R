#Install Packages
packageNames <- c("dplyr", "stringr", "data.table", "ggplot2", "maptools", "knitr", 
                  "mapproj", "RColorBrewer","tidyverse" )
install.packages(packageNames)
# Required Lib
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)
library(tidyverse)
#Read File
df <- fread("Pokemon.csv", h = T,encoding = 'UTF-8')
df <- data.frame(df)

#True VS False
LEGENDARY <- df[df$Legendary == "TRUE",]
LEGENDARY_F <- df[df$Legendary == "FALSE",]
sort_F <- LEGENDARY[order(LEGENDARY_F$Total),]

#Legendary vs not Legendary
summary(LEGENDARY)
summary(LEGENDARY_F)

#pie chart
L <- as.integer(count(LEGENDARY))
N <- as.integer(count(LEGENDARY_F))
df2 <- data.frame(Legend=c("Legendary", "NOT Legendary"),perc=c(L,N))
pie <- ggplot(df2, aes(x = "", y=perc, fill = Legend)) + geom_col(width = 1)
pie + coord_polar( "y",start = pi /0.2)

#Based on 【Type.1】to plot 【Rainbow】
ggplot(LEGENDARY, aes(x=Type.1,fill=Type.1))+
  geom_bar(stat="count", width=0.7)+
  theme_minimal()

#【MEAN】legendary VS Normal
features <- c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")
mean <- c(mean(LEGENDARY$HP),mean(LEGENDARY$Attack),mean(LEGENDARY$Defense),
          mean(LEGENDARY$Sp..Atk),mean(LEGENDARY$Sp..Def),mean(LEGENDARY$Speed))

mean_F <- c(mean(LEGENDARY_F$HP),mean(LEGENDARY_F$Attack),mean(LEGENDARY_F$Defense),
          mean(LEGENDARY_F$Sp..Atk),mean(LEGENDARY_F$Sp..Def),mean(LEGENDARY_F$Speed))
type <- c(rep("Legendary", 6), rep("Normal", 6))
values <- c(mean,mean_F)
mydata <-data.frame(features, values)

ggplot(mydata,aes(features,values))+
  geom_bar(stat="identity",aes(fill=type),position="dodge")

ggplot(df,aes(x=Legendary,y=HP))+geom_boxplot(aes(fill=Legendary))
ggplot(data=df,aes(x=Legendary,y=Attack))+geom_boxplot(aes(fill=Legendary))
ggplot(data=df,aes(x=Legendary,y=Defense))+geom_boxplot(aes(fill=Legendary))
ggplot(data=df,aes(x=Legendary,y=Sp..Atk))+geom_boxplot(aes(fill=Legendary))
ggplot(data=df,aes(x=Legendary,y=Sp..Def))+geom_boxplot(aes(fill=Legendary))
ggplot(data=df,aes(x=Legendary,y=Speed))+geom_boxplot(aes(fill=Legendary))
