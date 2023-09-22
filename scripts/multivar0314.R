rm(list=ls())
#library(tableone)
setwd("E:/MG/results/0314")
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

##筛选性别差异显著的因子
t1<-read.csv("clinical_table1_0313.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
t2<-read.csv("Lab_table2_0313.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
t1_v<-subset(t1, t1$p.Value < 0.05, select = 1)
t1_v <- as.character(t1_v[, 1])
t2_v<-subset(t2, t2$p.Value < 0.05, select = 1)
t2_v <- as.character(t2_v[, 1])
##与单因素分析结果取交集
t1_v
t2_v
significant_vars 
intersect(t1_v, significant_vars)
intersect(t2_v, significant_vars)
sex_critical_ill<-c(intersect(t1_v, significant_vars),intersect(t2_v, significant_vars))
sex_critical_ill

new_data<-c(sex_critical_ill, "Critical_ill","Sex")
df <- subset(new_df, select = new_data)
str(df)

####----亚组分析（男女）
male_df <- df[df$Sex == "1", ]
female_df <- df[df$Sex == "2", ]

# 计算每列的缺失值比例
missing_ratios_m <- colMeans(is.na(male_df))
missing_ratios_f <- colMeans(is.na(female_df))
# 选出缺失值比例大于50%的列
cols_to_drop_m <- which(missing_ratios_m > 0.5)
cols_to_drop_f <- which(missing_ratios_f > 0.5)
# 删除选出的列
male_df <- male_df[, -cols_to_drop_m]
female_df <- female_df[, -cols_to_drop_f]
##-----------------------------
# 计算每一列的均值
means_m <- colMeans(male_df, na.rm = TRUE)
means_f <- colMeans(female_df, na.rm = TRUE)
# 找出每一列中缺失值的位置
missing_cols_m <- which(colMeans(is.na(male_df)) > 0)
for (col in missing_cols_m) {
  # 用均值替换缺失值
  male_df[, col] <- ifelse(is.na(male_df[, col]), means_m[col], male_df[, col])
}

missing_cols_f <- which(colMeans(is.na(female_df)) > 0)
for (col in missing_cols_f) {
  # 用均值替换缺失值
  female_df[, col] <- ifelse(is.na(female_df[, col]), means_f[col], female_df[, col])
}

colMeans(is.na(male_df))

colMeans(is.na(female_df))
#-------------------------------------

# 男性模型
male_model <- glm(Critical_ill ~ Age_group_1 + Hypertension + Cardiovascular_disease  +Chronic_kidney_disease + Cough + UREA + TNI + TBIL + SO2 + PO2 + PCT + NEU.LYM + NEU + MYO + LYM1 + LYM + LDH + IL6 + IL10 + CRP + CK + AST, data = male_df, family = binomial(link = "logit"))
summary(male_model)

library(pROC)
# 用predict函数计算男性模型的预测概率
male_prob <- predict(male_model, newdata = male_df, type="response")
# 计算AUC值
male_roc <- roc(male_df$Critical_ill, male_prob)
auc_male <- auc(male_roc)
auc_male 
summary(male_model)$coefficients





# 女性模型
female_model <- glm(Critical_ill ~ SEX+Age_group_1  + Cardiovascular_disease + Chronic_kidney_disease + UREA + TNI+ NEU.LYM + NEU + MYO + LYM1 + LYM + LDH + IL6  + CRP + CK + AST, data = female_df, family = binomial(link = "logit"))
summary(female_model)
library(pROC)
female_prob <- predict(female_model, newdata = female_df, type="response")
# 计算AUC值
female_roc <- roc(female_df$Critical_ill, female_prob)
auc_female <- auc(female_roc)
auc_female

female_model <- glm(Critical_ill ~ Sex*Age_group_1 + Cardiovascular_disease + Chronic_kidney_disease + UREA + TNI+ NEU.LYM + NEU + MYO + LYM1 + LYM + LDH + IL6 + CRP + CK + AST, data = new_df, family = binomial(link = "logit"))





summary(female_model)$coefficients
exp(coef(female_model))
exp(confint(female_model))







