rm(list=ls())
#install.packages("ggmosaic")
library(ggplot2)
library(ggmosaic)
library(tidyverse)

#library(tableone)
setwd("E:/MG/317/317")
data <- read.csv("age1.csv", header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
str(data)
data$SEX<-as.factor(data$SEX)
data$Age_groups<-as.factor(data$Age_groups)
data$outcomes<-as.factor(data$outcomes)
str(data)
library(ggplot2)


#-------------
library(ggsci)
# 绘制条形图并使用Nature期刊的配色
data$outcomes<-factor(data$outcomes,order= T, levels=c('Death','In hospital','Discharged'))
p1<-ggplot(data, aes(x = SEX, fill = outcomes)) +
  geom_bar(position = "fill",width = 0.5) +
  facet_grid(. ~ Age_groups) +
  labs(x = NULL, y = "Percent", fill = "Outcome") +
  theme_minimal() +
  scale_fill_npg() +
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format())
p1
#----------------
#library(tableone)
setwd("E:/MG/317/317")
data2 <- read.csv("age2.csv", header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
str(data2)
data2$SEX<-as.factor(data2$SEX)
data2$Age_groups<-as.factor(data2$Age_groups)
data2$outcomes<-as.factor(data2$outcomes)
data2$Disease<-as.factor(data2$Disease)
str(data2)
library(ggplot2)

library(ggsci)
# 绘制条形图并使用Nature期刊的配色
data2$outcomes<-factor(data2$outcomes,order= T, levels=c('Death','In hospital','Discharged'))
p2<-ggplot(data2, aes(x = SEX, fill = outcomes)) +
  geom_bar(position = "fill",width=0.4) +
  facet_grid(. ~ Disease) +
  labs(x = NULL,  y = NULL, fill = "Outcome") +
  theme_minimal() +scale_fill_npg() +
  scale_y_continuous(labels = scales::percent_format())
p2

library(gridExtra)

# Set the size of the plots
options(repr.plot.width = 10, repr.plot.height = 6)

# Combine the two plots
grid.arrange(p1,    
             p2 + theme(axis.title.y = element_blank(), 
                        axis.text.y = element_blank(), 
                        axis.ticks.y = element_blank()),
             ncol = 2,widths = c(1.7, 1.2))

#----------
## 绘制马赛克图
library(vcd)
library(mosaic)
table(data$Age_groups)
data$Age_groups<-factor(data$Age_groups,order= T, levels=c('0-49','50-59','60-69','70-79','>80'))
## 创建三维列联表
tab.read.gender<-xtabs(~SEX+outcomes+Age_groups,data=data)
## 显示平铺后的列联表--gender
ftable(tab.read.gender)
mosaic(~ outcomes + SEX + Age_groups, data = tab.read.gender,
highlighting = "outcomes",

highlighting_fill = c("pink", "lightblue","lightyellow"),

direction = c("h", "h", "v")) #各变量分割顺序



