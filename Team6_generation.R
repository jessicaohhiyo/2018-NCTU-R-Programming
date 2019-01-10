#分析不同世代神奇寶貝的能力值
library(dplyr)
library(tidyverse)
library(ggplot2)
library(data.table)
data <- fread("Pokemon.csv",header=TRUE,sep=",",encoding = "UTF-8")
data$Generation <- as.factor(data$Generation)
#print(data)

#各世代的數量
ggplot(data=data,aes(x=Generation)) + geom_bar()
#整理屬性
tdata <- gather(data, key = st/nd, value = Type , `Type 1`, `Type 2`)
tdata <- filter(tdata, Type != "")

#各世代各屬性的數量
ggplot(data = tdata, aes(x = Type, fill = Generation)) + geom_bar()
#各世代能力值的分布
pairs(select(tdata, HP, Attack, Defense, `Sp. Atk`, `Sp. Def`, Speed), col = tdata$Generation)

ggplot(data = tdata, aes(x = Generation, y = HP)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = HP)) + geom_point(position = "jitter")

ggplot(data = tdata, aes(x = Generation, y = Attack)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = Attack)) + geom_point(position = "jitter")

ggplot(data = tdata, aes(x = Generation, y = Defense)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = Defense)) + geom_point(position = "jitter")

ggplot(data = tdata, aes(x = Generation, y = `Sp. Atk`)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = `Sp. Atk`)) + geom_point(position = "jitter")

ggplot(data = tdata, aes(x = Generation, y = `Sp. Def`)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = `Sp. Def`)) + geom_point(position = "jitter")

ggplot(data = tdata, aes(x = Generation, y = Speed)) + geom_boxplot()
ggplot(data = tdata, aes(x = Generation, y = Speed)) + geom_point(position = "jitter")

#levels(factor(tdata$Type))
#"Bug"      "Dark"     "Dragon"   "Electric" "Fairy"    "Fighting"
#"Fire"     "Flying"   "Ghost"    "Grass"    "Ground"   "Ice"      "Normal"  
#"Poison"   "Psychic"  "Rock"     "Steel"    "Water"
ggplot(data = tdata) + geom_point(mapping = aes(x=Generation,y=Total)) + facet_wrap(~Type)
ggplot(data = tdata[as.logical(tdata$Legendary)==TRUE,], aes(x = Generation, y = Total)) + geom_boxplot()
ggplot(data = tdata[as.logical(tdata$Legendary)==TRUE,], aes(x = Generation, y = Total)) + geom_point(position = "jitter")
