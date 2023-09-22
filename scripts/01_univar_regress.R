rm(list=ls())
#library(tableone)
setwd("E:/MG/results")
new_df<-read.csv("data0313.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
head(new_df)

library(tidyr)
library(dplyr)

#write.csv(new_df, file = "data0312.csv", row.names = FALSE)
# 选择预测变量
predictors <- names(new_df[-c(1,21)])

# 创建一个空向量来存储P-value小于0.05的变量名
significant_vars <- c()

# 循环进行单变量线性回归
for (i in predictors) {
  # 拟合模型
  model <- glm(Critical_ill ~ new_df[,i], data = new_df,family = "binomial")
  # 获取P-value
  p_value <- summary(model)$coefficients[2, 4]
  # 如果P-value小于0.05，则将变量名添加到significant_vars向量中
  if (p_value < 0.05) {
    significant_vars <- c(significant_vars, i)
  }
}
significant_vars 

#
t1<-read.csv("clinical_table1.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
t2<-read.csv("Lab_table2_0313.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
t1_v<-subset(t1, t1$p.Value < 0.05, select = 1)
t1_v <- as.character(t1_v[, 1])
t2_v<-subset(t2, t2$p.Value < 0.05, select = 1)
t2_v <- as.character(t2_v[, 1])

t1_v
t2_v
significant_vars 
intersect(t1_v, significant_vars)
intersect(t2_v, significant_vars)
sex_critical_ill<-c(intersect(t1_v, significant_vars),intersect(t2_v, significant_vars))
sex_critical_ill

new_data<-c(sex_critical_ill, "Critical_ill","Sex","Age_group_5")
df <- subset(new_df, select = new_data)
str(df)

####
male_df <- df[df$Sex == "1", ]
female_df <- df[df$Sex == "2", ]
# 男性模型
male_model <- glm(Critical_ill ~ Age_group_1 + Hypertension + Cardiovascular_disease + Chronic_kidney_disease + Disease + Cough + UREA + TNI + TBIL + SO2 + PO2 + PCT + NEU.LYM + NEU + MYO + LYM1 + LYM + LDH + IL6 + IL10 + CRP + CK + CD8 + CD4 + CD3CD4CD8 + CD3CD19 + CD3CD1656 + CD3_A + CD3 + AST, data = male_df, family = binomial)
male_model <- glm(Critical_ill ~ Age_group_1 + Hypertension  + Chronic_kidney_disease  + Cough + UREA + TNI + TBIL + SO2 + PO2 + PCT + NEU.LYM + NEU + MYO + LYM1 + LYM + LDH + IL6 + IL10 + CRP + CK + CD8 , data = male_df, family = binomial)
# 女性模型
female_model <- glm(Critical_ill ~ Age_group_1 + Age_group_5 + Hypertension + Cardiovascular_disease + Chronic_kidney_disease + Disease +  NEU.LYM + NEU + LYM1 + LYM , data = female_df, family = binomial)

summary(male_model)

summary(female_model)

##---------
# 提取回归系数
coef <- summary(male_model)$coefficients[, 1]

# 提取标准误
se <- summary(male_model)$coefficients[, 2]

# 提取z值
z <- summary(male_model)$coefficients[, 3]

# 提取Wald chi-square
wald <- summary(male_model)$coefficients[, 4]

# 提取p值
p <- summary(male_model)$coefficients[, 5]

# 提取OR值
OR <- exp(coef)

# 提取OR值的95%置信区间
OR_lower <- exp(coef - 1.96 * se)
OR_upper <- exp(coef + 1.96 * se)
