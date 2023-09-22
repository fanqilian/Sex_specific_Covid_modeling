#install.packages("pROC")
library(pROC)  # 加载需要用到的库
library(caret)
str(data2)
for (i in names(data2)[c(4:22,69:73)]){data2[,i] <- as.factor(data2[,i])}
for (i in names(data2)[c(23:68)]){data2[,i] <- as.integer(data2[,i])}
str(data2)

set.seed(123)
folds <-createMultiFolds(y = as.factor(data2$Critical_ill),k=10,times=200)
# 创建一个长度为200的空向量来存储每次循环得到的AUC值
auc_values <- numeric(2000)

# 执行循环2000次
for (i in 1:2000) {
  # 取fold 1数据为训练集
  train <- data2[folds[[i]],]
  # 其余为验证集
  test <- data2[-folds[[i]],]
  # 构建逻辑回归模型
  model <- glm(Critical_ill ~ Chronic_liver_disease + Fever_days_3 +Age_group_5 + BMI + WBC + LYM1 + PCO2 + LDH + BNP + IFNa,
               family = binomial(link=logit), 
               data = train)
  # 验证队列做预测
  model_pre <- predict(model,
                       type = 'response',
                       newdata = test)
  # 查看AUC值、敏感性、特异性
  roc1 <- roc((test$Critical_ill), model_pre)
  AUC <- round(auc(roc1), 3)
  # 将当前循环得到的AUC值保存到auc_values向量中
  auc_values[i] <- AUC
}

# 求平均AUC值
mean_auc <- mean(auc_values)
mean_auc
##-------male---------------------------------------
str(data1)
set.seed(123)
folds <-createMultiFolds(y = as.factor(data1$Critical_ill),k=10,times=200)
for (i in names(data1)[c(4:22,69:73)]){data1[,i] <- as.factor(data1[,i])}
for (i in names(data1)[c(23:68)]){data1[,i] <- as.integer(data1[,i])}
folds <-createMultiFolds(y = as.factor(data1$Critical_ill),k=10,times=200)
# 创建一个长度为200的空向量来存储每次循环得到的AUC值
auc_values <- numeric(2000)

# 执行循环2000次
for (i in 1:2000) {
  # 取fold 1数据为训练集
  train <- data1[folds[[i]],]
  # 其余为验证集
  test <- data1[-folds[[i]],]
  # 构建逻辑回归模型
  model <- glm(Critical_ill ~ RR_30 + BMI + NEU1 + LYM1 +HGB + CRP  +PCO2 + PO2 + LDH + MYO  +IL6 +IL10,
               family = binomial(link=logit), 
               data = train)
  # 验证队列做预测
  model_pre <- predict(model,
                       type = 'response',
                       newdata = test)
  # 查看AUC值、敏感性、特异性
  roc1 <- roc((test$Critical_ill), model_pre)
  AUC <- round(auc(roc1), 3)
  # 将当前循环得到的AUC值保存到auc_values向量中
  auc_values[i] <- AUC
}

# 求平均AUC值
mean_auc <- mean(auc_values)
mean_auc 
