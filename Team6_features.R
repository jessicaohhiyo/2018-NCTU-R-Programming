# Required Lib
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(maptools)
library(knitr)
library(mapproj)
library(RColorBrewer)
library(pracma)

# Read Dataframe
df <- fread("C:/Users/sharon/Desktop/課業/r/Pokemon.csv", h = T)
df <- data.frame(df)

# Type_1 的數量統計圖
type1_sum <- split(df$Type.1,f = df$Type.1)
type1 <- data.frame(summary(type1_sum))
type1$Var2 <- NULL
type1 <- head(type1,length(type1_sum))
ggplot(type1, aes(x = Var1, y = Freq, fill=Var1))+geom_bar(stat = "identity")+
labs(title='主要類型的數量統計', x="類型", y="數量")

#各類型的能力分布
ability <- c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")
type1_skill <- split(df,f = df$Type.1)
for (i in type1$Var1) { #不同類型
  tmp <- list(i)
  for (j in 1:length(ability)) {
    tmp[[j+1]] <- mean(type1_skill[[i]][[ability[j]]])
  }
  if (i == "Bug"){
    type_ability <- data.frame(tmp)
    colnames(type_ability) <- c('type','HP','Attack','Defense','Sp..Atk','Sp..Def','Speed')
  } 
  else{
    tmp <- data.frame(tmp)
    colnames(tmp) <- c('type','HP','Attack','Defense','Sp..Atk','Sp..Def','Speed')
    type_ability <- rbind(type_ability,tmp)
  }
}
#畫圖
#不同類型怪獸能力比較圖
ggplot(data = type_ability, aes(x = type, y = HP, fill=type))+geom_bar(stat = "identity")+
  labs(title="HP", x="類型", y="平均值")
ggplot(data = type_ability, aes(x = type, y = Attack, fill=type))+geom_bar(stat = "identity")+
  labs(title="Attack", x="類型", y="平均值")
ggplot(data = type_ability, aes(x = type, y = Defense, fill=type))+geom_bar(stat = "identity")+
  labs(title="Defense", x="類型", y="平均值")
ggplot(data = type_ability, aes(x = type, y = Sp..Atk, fill=type))+geom_bar(stat = "identity")+
  labs(title="Sp..Atk", x="類型", y="平均值")
ggplot(data = type_ability, aes(x = type, y = Sp..Def, fill=type))+geom_bar(stat = "identity")+
  labs(title="Sp..Def", x="類型", y="平均值")
ggplot(data = type_ability, aes(x = type, y = Speed, fill=type))+geom_bar(stat = "identity")+
  labs(title="Speed", x="類型", y="平均值")

#每類怪獸能力總攬
