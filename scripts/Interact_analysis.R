###在这之前先运行code0317.R
malevsfemale<-setdiff(var_names,var_names2)
femalevsmale<-setdiff(var_names2,var_names)
sex_specific<-c(malevsfemale,femalevsmale)

covar<-c(sex_specific,"Sex","Critical_ill")
covar
# 检查 covar 中的列名是否存在于 data 中
missing_cols <- setdiff(covar, names(data))
if (length(missing_cols) > 0) {
  stop(paste("以下列名不存在于 data 中：", paste(missing_cols, collapse = ", ")))
}
covar <- gsub("RR_301", "RR", covar)
covar
# 从 data 中提取指定的列
str(data)
names(data)[names(data) == "RR_30"] <- "RR"
df <- data[covar]
str(df)
for (i in names(df)[c(1:7,31:32)]){df[,i] <- as.factor(df[,i])}

str(df)
library(dplyr)
library(tidyr)
library(stats)


# 批量构建多元逻辑回归模型包含交互相
covar<-covar[covar != "Sex"]
covar<-covar[covar != "Critical_ill"]

# 创建一个空列表来存储模型
models <- list()

# 循环covar中的变量
for (i in covar) {
  # 用paste函数构建公式
  formula_text <- paste0("Critical_ill ~ ", paste0(covar, collapse = "+"), " + Sex:", i)
  
  # 用assign函数动态创建变量名和公式
  assign(paste0("model_c_", i), formula(formula_text))
  
  # 用get函数获取变量名对应的公式
  cor_model <- glm(get(paste0("model_c_", i)), data = df, family = binomial(link = "logit"))
  
  # 把模型添加到列表中
  models[[i]] <- cor_model
}

# 打印所有模型的结果
lapply(models, function(x) summary(x))

#---------------------提取所有模型交互项回归系数和P值-----
#covar <- c("Diabetes", "Malignancy", "Lung_disease", "Dry_throat", "RR", "Age_group_2", "Age_group_1",
#           "NEU1", "LYM", "LYM1", "MON", "MON1", "HGB", "PLT", "PH", "SO2", "PO2", "DD", "PT", "TT",
#           "IL10", "IL2", "IL4", "IL5", "IL6", "IL8", "NEU", "AST", "TNI", "IL17")

coef_list <- lapply(covar, function(i) {
  coef <- summary(models[[i]])$coefficients
  coef[grepl(":Sex", rownames(coef)), ]
})
str(df)

# 将列表中的所有元素合并为一个数据框
coef_df <- do.call(rbind, coef_list)

coef_df
write.csv(coef_df,file = "coef_df.csv", row.names = TRUE)
#---------------------提取所有模型交互项OR和CI值-----
setwd("E:/MG/results/0314")
new_df <- read.csv("coef_df_1.csv")
new_df
# 计算OR值
new_df$OR <- exp(new_df$Estimate)

# 计算95%置信区间
se <- new_df$Std..Error
new_df$CI_lower <- exp(new_df$Estimate - 1.96 * se)
new_df$CI_upper <- exp(new_df$Estimate + 1.96 * se)
new_df$name <- c("Diabetes", "Malignancy", "Lung disease", "Dry throat", "RR", "Age:50 to 59", "Age:0 to 49",
          "Neutrophil ratio", "Lymphocyte count", "Lymphocyte ratio", "Monocyte count", "Monocyte ratio", "Haemoglobin", 
          "Platelet count", "PH", "Oxygen saturation", "Oxygen partial pressure", "D-dimer", "Prothrombin time", "Thrombin time",
          "Interleukin10", "Interleukin2", "Interleukin4", "Interleukin5", "Interleukin6", "Interleukin8", "Neutrophil count", "Aspartate aminotransferase",
          "TNI", "Interleukin17")
           
new_df
#----------绘制森林图
library(grid)
#install.packages("forestploter")
library(forestploter)
dt<-new_df


tm <- forest_theme(
  base_size = 10,
  #文本的大小
  ci_pch = 15,
  #可信区间点的形状
  ci_col = "black",
  #CI的颜色
  ci_fill = "black",
  #ci颜色填充
  ci_alpha = 0.5,
  #ci透明度
  ci_lty = 1,
  #CI的线型
  ci_lwd = 1,
  #CI的线宽
  ci_Theight = 0.2,
  refline_lwd = 1,
  #中间的竖的虚线
  refline_lty =
    "dashed",
  refline_col =
    "grey20",
  # Vertical line width/type/color  垂直线宽/类型/颜色   可以添加一条额外的垂直线，如果没有就不显示
  vertline_lwd = 1,
  #可以添加一条额外的垂直线，如果没有就不显示
  vertline_lty =
    "dashed",
  vertline_col =
    "grey20",
  
  summary_fill =
    "yellow",
  #汇总部分大菱形的颜色
  summary_col =
    "#4575b4",
  
  footnote_cex = 0.6,
  footnote_fontface =
    "italic",
  footnote_col =
    "red"
)
dt$` `<- paste(rep(" ", 35), collapse = " ")
dt
pt <-
  forest(
    dt[, c(9,10,5)],
    est = dt$OR,
    #效应值
    lower = dt$CI_lower,
    #可信区间下限
    upper = dt$CI_upper,
    #可信区间上限
    sizes = 1,
    #黑框的大小
    ci_column = 2,
    #在那一列画森林图，要选空的那一列
    ref_line = 1,
    arrow_lab = c("Better", "Worse"),
    xlim = c(0.0, 4.0),
    ticks_at = c(0.0, 1.0, 2.0,4.0),
    theme = tm
  )

pt
write.csv(dt,"dt.csv",row.names = TRUE)
