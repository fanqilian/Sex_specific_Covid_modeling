#install.packages("tableone")
rm(list=ls())
library(tableone)
setwd("/Users/qilianfan/Desktop/sihuo/model/")
data<-read.csv("data0312.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
head(data)
str(data)
#-----------------
data$Critical_ill <- ifelse(grepl("ICU","重症监护室", data$AREA) | data$outcomes == 3 |data$Respiratory_failure_and_mechanical_ventilation == 1,1, 0)
data$Disease<- ifelse(data$Obesity ==1 |data$Hypertension == 1|data$Diabetes == 1|data$Malignancy==1|
                        data$Lung_disease ==1|data$Cardiovascular_disease ==1|data$Chronic_liver_disease==1|
                        data$Chronic_kidney_disease==1|data$Autoimmune_disease==1,1,0)
data$outcomes_3<- ifelse(data$outcomes ==3,1,0)

data

write.csv(data, file = "data1.csv", row.names = FALSE)
str(data)
#-------------
# 提取男性和女性的年龄数据
shapiro.test(data$TNI) # 使用Shapiro-Wilk检验


ks.test(data$Age, "pnorm", mean(data$Age), sd(data$Age)) # 使用Kolmogorov-Smirnov检验

male_age <- data$Age[data$Sex == 1]
female_age <- data$Age[data$Sex == 2]

# 进行U检验
wilcox.test(male_age, female_age, paired = FALSE)

#-------------------------

rm(list=ls())
library(tableone)
setwd("/Users/qilianfan/Desktop/sihuo/model/312")
data<-read.csv("data0312.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
head(data)
str(data)
var_list <- c(names(data[c(92:96,101,102,106,107,108,112:114)]))
library(dplyr)

# 创建一个空的数据框df1
df1 <- data.frame()

# 循环遍历每个变量
for (var_name in var_list) { 
  
  # 根据变量名称构建新的列名
  new_col_names <- paste0(var_name, "_", 1:3)
  
  # 使用 mutate 函数添加新的列，并删除原始列
  df2 <- data %>% 
    mutate(
      !!new_col_names[1] := ifelse(.data[[var_name]] == 1, 1, 0),
      !!new_col_names[2] := ifelse(.data[[var_name]] == 2, 1, 0),
      !!new_col_names[3] := ifelse(.data[[var_name]] == 3, 1, 0)
    ) %>% 
    select(-var_name) 
  
  # 把df2追加到df1中
  df1 <- bind_rows(df1, df2)
}

# 输出df1的内容
print(df1)


# 定义要处理的变量名称列表
var_list2 <- c(names(data[c(97:100,103:105,109:111,115:116)]))

# 创建一个空的数据框df1
df3 <- data.frame()

# 循环遍历每个变量
for (var_name in var_list2) { 
  
  # 根据变量名称构建新的列名
  new_col_names <- paste0(var_name, "_", 1:3)
  
  # 使用 mutate 函数添加新的列，并删除原始列
  df4 <- data %>% 
    mutate(
      !!new_col_names[1] := ifelse(.data[[var_name]] == 1, 1, 0),
      !!new_col_names[2] := ifelse(.data[[var_name]] == 2, 1, 0),
      !!new_col_names[3] := ifelse(.data[[var_name]] == 3, 1, 0)
    ) %>% 
    select(-var_name) 
  
  # 把df2追加到df1中
  df3 <- bind_rows(df3, df4)
}

# 输出df1的内容
print(df3)


#------------------------
n1<-c(names(df1[c(121:160)]))
n2<-c(names(df3[c(121:157)]))

mytable <- CreateTableOne(vars = n1,
                          factorVars = n1,
                          data = df1 ,
                          strata = "Sex",addOverall = TRUE)
#print(mytable, test = TRUE, smd = TRUE)
tab4Mat <- print(mytable, quote = FALSE, noSpaces = TRUE)
write.csv(tab4Mat, file = "n1.csv", row.names = TRUE)
mytable2 <- CreateTableOne(vars = n2,
                          factorVars = n2,
                          data = df3 ,
                          strata = "Sex",addOverall = TRUE)
#print(mytable, test = TRUE, smd = TRUE)
tab4Mat <- print(mytable2, quote = FALSE, noSpaces = TRUE)
write.csv(tab4Mat, file = "n2.csv", row.names = TRUE)
#----------------
rm(list=ls())
library(tableone)
setwd("/Users/qilianfan/Desktop/sihuo/model/312")
data<-read.csv("data0312.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
head(data)
str(data)
n1<-c(names(data[c(4,29)]))

mytable <- CreateTableOne(vars = n1,
                          data = data ,
                          strata = "Sex",addOverall = TRUE)
#print(mytable, test = TRUE, smd = TRUE)
tab4Mat <- print(mytable, quote = FALSE, noSpaces = TRUE,nonnormal = n1)
write.csv(tab4Mat, file = "n4.csv", row.names = TRUE)
