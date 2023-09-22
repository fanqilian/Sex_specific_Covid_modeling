rm(list = ls())
#install.packages("glmnet")
library(glmnet)
setwd("E:/MG/results/0314")
data <- read.csv("data0313.csv")

my.miss <- function(mx){
  n <- NULL
  na <- NULL
  for (i in 1:ncol(mx)) {
    dat <- mx[,i]
    nmiss <- sum(is.na(dat))
    percent <- round(nmiss / length(dat) * 100, digits = 1) 
    n[i] <- percent
    na[i] <- colnames(mx)[i]
  }
  df <- cbind(na, n)
  colnames(df) <- c("var", "%")
  return(df)
}
k <- my.miss(data)

# 删除缺失率大于50%的变量
data <- data[,k[,1][as.numeric(k[,2]) < 50]]
my.miss(data)
str(data)
#install.packages("prettyR")
# 缺失值填补（众数）
library(prettyR)
data$Obesity[is.na(data$Obesity)] <- Mode(data$Obesity, na.rm = T)#用Mode函数来填补data数据框中Obesity列的缺失值，Mode函数可以计算一个向量中出现次数最多的值，也就是众数
data$Chronic_kidney_disease[is.na(data$Chronic_kidney_disease)] <- Mode(data$Chronic_kidney_disease, na.rm = T)
data$Autoimmune_disease[is.na(data$Autoimmune_disease)] <- Mode(data$Autoimmune_disease, na.rm = T)
data$RR_30[is.na(data$RR_30)] <- Mode(data$RR_30, na.rm = T)
data$Disease[is.na(data$Disease)] <- Mode(data$Disease, na.rm = T)
data$Obesity <- as.factor(data$Obesity)
data$Chronic_kidney_disease <- as.factor(data$Chronic_kidney_disease)
data$Autoimmune_disease <- as.factor(data$Autoimmune_disease)
data$RR_30 <- as.factor(data$RR_30)
data$Disease <- as.factor(data$Disease)

data <- data[complete.cases(data$Age),]#删除data数据框中年龄缺失的行，complete.cases函数可以返回一个逻辑向量，表示哪些行没有缺失值。

# 编写缺失函数
my.fill <- function(mx){
  # mx:缺失的变量
  # y：依赖的分组
  for (i in 1:ncol(mx)) {
    mx[,i][is.na(mx[,i])] <- mean(is.na(mx[,i]) == FALSE, na.rm = T)
  }
  return(mx)
}
data <- my.fill(data)
my.miss(data)

# lasso
# 性别1
data1 <- data[data$Sex == 1,]
dput(names(data1))
y <- data1$Critical_ill
dat1 <- data1[,c("Obesity", "Hypertension", "Diabetes", 
                  "Malignancy", "Lung_disease", "Cardiovascular_disease", "Chronic_liver_disease", 
                  "Chronic_kidney_disease", "Autoimmune_disease", "Fever_days_3", 
                  "Cough", "Sore_throat", "Dry_throat", "Diarrhea", "RR_30","Age_group_5", 
                  "Age_group_4", "Age_group_3", "Age_group_2", "Age_group_1")]
dat2 <- data1[,c("BMI", "WBC", "NEU", "NEU1", "LYM", 
                 "LYM1", "NEU.LYM", "MON", "MON1", "HGB", "PLT", "CRP", "ESR", 
                 "PCT", "PH", "SO2", "PCO2", "PO2", "DD", "PT", "TT", "LDH", "AST", 
                 "ALT", "TBIL", "CREA", "UREA", "CK", "TNI", "CKMB", "MYO", "BNP", 
                 "CD19", "IFNa", "IFNy", "IL10", "IL12", "IL17", "IL1b", "IL2", 
                 "IL4", "IL5", "IL6", "IL8", "ASTALT", "UREACREA")]
dat1 <- model.matrix(~ .-1,dat1)
dat2 <- apply(dat2, 2, as.numeric)
dat <- as.matrix(cbind(dat1, dat2))
y <- as.factor(y)


# 交叉验证
set.seed(123)
cvfit <- cv.glmnet(dat,y, family = "binomial")
plot(cvfit)
cvfit$lambda.min
coef1<-coef(cvfit, s = "lambda.min")
coef1
#write.csv(as.matrix(coef1), file="coef1.csv", row.names=TRUE)
# 将稀疏矩阵转换为标准矩阵
coef_matrix <- as.matrix(coef1)
# 提取非零系数的变量名称
var_names <- names(which(coef_matrix != 0, arr.ind = TRUE)[, 2])
# 打印结果
var_names

# 性别2
data2 <- data[data$Sex == 2,]
dput(names(data2))
y <- data2$Critical_ill
dat2_1 <- data2[,c("Obesity", "Hypertension", "Diabetes", 
                 "Malignancy", "Lung_disease", "Cardiovascular_disease", "Chronic_liver_disease", 
                 "Chronic_kidney_disease", "Autoimmune_disease", "Fever_days_3", 
                 "Cough", "Sore_throat", "Dry_throat", "Diarrhea", "RR_30","Age_group_5", 
                 "Age_group_4", "Age_group_3", "Age_group_2", "Age_group_1")]
dat2_2 <- data2[,c("BMI", "WBC", "NEU", "NEU1", "LYM", 
                 "LYM1", "NEU.LYM", "MON", "MON1", "HGB", "PLT", "CRP", "ESR", 
                 "PCT", "PH", "SO2", "PCO2", "PO2", "DD", "PT", "TT", "LDH", "AST", 
                 "ALT", "TBIL", "CREA", "UREA", "CK", "TNI", "CKMB", "MYO", "BNP", 
                 "CD19", "IFNa", "IFNy", "IL10", "IL12", "IL17", "IL1b", "IL2", 
                 "IL4", "IL5", "IL6", "IL8", "ASTALT", "UREACREA")]
dat2_1 <- model.matrix(~ .-1,dat2_1)
dat2_2 <- apply(dat2_2, 2, as.numeric)
dat_2 <- as.matrix(cbind(dat2_1, dat2_2))
y <- as.factor(y)


# 交叉验证
set.seed(123)
cvfit2 <- cv.glmnet(dat_2,y, family = "binomial")
plot(cvfit2)
cvfit2$lambda.min
coef2<-coef(cvfit2, s = "lambda.min")
coef2
# 将稀疏矩阵转换为标准矩阵
coef_matrix2 <- as.matrix(coef2)
# 提取非零系数的变量名称
var_names2 <- names(which(coef_matrix2 != 0, arr.ind = TRUE)[, 2])
# 打印结果
var_names2

malevsfemale<-setdiff(var_names,var_names2)
femalevsmale<-setdiff(var_names2,var_names)
sex_specific<-c(malevsfemale,femalevsmale)

#-------------------------
# 重复100次Lasso回归和交叉验证
#coef.list <- list() # 创建一个空列表用于保存特征系数
#for (i in 1:100) {
#  cvfit <- cv.glmnet(dat,y, family = "binomial") # 返回一个模型对象
#  lambda.min <- cvfit$lambda.min # 提取最优惩罚系数lambda.min
#  coef.list[[i]] <- coef(cvfit, s = lambda.min) # 提取特征系数并保存在列表中
#}

# 统计每个特征出现的频率
#coef.matrix <- do.call(cbind, coef.list) # 将列表转换为矩阵
#coef.freq <- apply(coef.matrix[-1, ], 1, function(x) sum(x != 0)) / 100 # 计算每个特征不为0的比例，去掉截距项

# 筛选出现频率高的特征
#threshold <- 0.5 # 设置一个阈值，可以根据需要调整
#coef.selected <- which(coef.freq > threshold) # 筛选出超过阈值的特征序号
#print(coef.selected) # 打印出最终结果
#var_names
#--------------------------




#####----------多元回归分析
# 男性模型
var_m<-var_names[var_names != "(Intercept)"]
var_m <- gsub("Chronic_kidney_disease1", "Chronic_kidney_disease", var_m) # 使用gsub函数替换字符串
var_m <- gsub("RR_301", "RR_30", var_m)
#model_m <- formula(paste0("Critical_ill ~", paste0(var_m, collapse = "+")))
#male_model <- glm(model_m, data = data1, family = "binomial")

male_model <- glm(Critical_ill ~ RR_30 + BMI + NEU1 +  LYM1 +HGB + CRP  +PCO2 + PO2 + LDH + MYO  +IL6 +IL10 , data = data1, family = binomial(link = "logit"))
summary(male_model)

library(pROC)
# 用predict函数计算男性模型的预测概率
male_prob <- predict(male_model, newdata = data1, type="response")
# 计算AUC值
male_roc <- roc(data1$Critical_ill, male_prob)
auc_male <- auc(male_roc)
auc_male 
ci(male_roc)
coords(male_roc, "best", ret=c("threshold", "sensitivity", "specificity"))
write.csv(summary(male_model)$coefficients,file = "male.csv", row.names = TRUE)
library(pROC)
plot(male_roc, 
     print.auc=T, 
     auc.polygon=T, 
     auc.polygon.col="skyblue",
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=T,
     print.thres=T)

# 女性模型
var_f<-var_names2[var_names2 != "(Intercept)"]
var_f <- gsub("Chronic_kidney_disease1", "Chronic_kidney_disease", var_f) # 使用gsub函数替换字符串
model_f <- formula(paste0("Critical_ill ~", paste0(var_f, collapse = "+")))
female_model <- glm(model_f, data = data2, family = "binomial")

female_model <- glm(Critical_ill ~ Chronic_liver_disease + Fever_days_3 +Age_group_5 + BMI + WBC + LYM1 + PCO2 + LDH + BNP + IFNa, data = data2, family = binomial(link = "logit"))
summary(female_model)
female_prob <- predict(female_model, newdata = data2, type="response")
# 计算AUC值
female_roc <- roc(data2$Critical_ill, female_prob)
auc_female <- auc(female_roc)
auc_female
ci(female_roc)
coords(female_roc, "best", ret=c("threshold", "sensitivity", "specificity"))
write.csv(summary(female_model)$coefficients,file = "female.csv", row.names = TRUE)
plot(female_roc, 
     print.auc=T, 
     auc.polygon=T, 
     auc.polygon.col="pink",
     grid=c(0.1, 0.2),
     grid.col=c("green", "red"), 
     max.auc.polygon=T,
     print.thres=T)
#install.packages("verification")
library(verification)
data2$result[data2$Critical_ill == 0] <- 0
data2$result[data2$Critical_ill == 1] <- 1
data2$result
roc.area(data2$result,female_roc$predictor)
library(verification)
data1$result[data1$Critical_ill == 0] <- 0
data1$result[data1$Critical_ill == 1] <- 1
data1$result
roc.area(data1$result,male_roc$predictor)
#-----------------
formatFit<-function(fit){
  #取P值
  p<-summary(fit)$coefficients[,4]
  #wald值
  wald<-summary(fit)$coefficients[,3]^2
  #B值
  valueB<-coef(fit)
  #OR值
  valueOR<-exp(coef(fit))
  #OR值得95%CI
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,3),
    Wald=round(wald,3),
    OR_with_CI=paste(round(valueOR,3),"(",
                     round(confitOR[,1],3),"~",round(confitOR[,2],3),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}
male_other<-formatFit(male_model)
write.csv(male_other, file = "male_other.csv", row.names = TRUE)
female_other<-formatFit(female_model)
write.csv(female_other, file = "female_other.csv", row.names = TRUE)
