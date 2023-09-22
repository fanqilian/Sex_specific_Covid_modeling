rm(list=ls())
#library(tableone)
setwd("/Users/qilianfan/Desktop/sihuo/model/")
data<-read.csv("data0312.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
head(data)
do_univar_logistic_reg <- function(data, vars, outcome) {
  # 筛选出需要分析的变量和结果变量
  df <- data %>% select(vars, outcome)
  
  # 定义结果数据框
  result <- data.frame(var = character(),
                       OR = numeric(),
                       CI_lower = numeric(),
                       CI_upper = numeric(),
                       stringsAsFactors = FALSE)
  
  # 循环处理每个变量
  for (var in vars) {
    # 进行单变量逻辑回归分析
    mod <- glm(paste(outcome, "~", var), data = df, family = "binomial")
    # 计算 OR 和 CI
    OR <- exp(coef(mod))
    CI <- confint(mod)
    CI_lower <- exp(CI[1, ])
    CI_upper <- exp(CI[2, ])
    # 记录结果
    result <- rbind(result, data.frame(var = var,
                                       OR = OR,
                                       CI_lower = CI_lower,
                                       CI_upper = CI_upper,
                                       stringsAsFactors = FALSE))
  }
  
  return(result)
}

outcome<-names(data[c(30)])
predictors <- names(data[-c(1,2,30)])
predictors <- names(data[c(121)])
do_univar_logistic_reg(data,predictors, outcome) 
