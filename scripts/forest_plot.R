rm(list = ls())
library(dplyr)
setwd("E:/MG/317/317")
male<-read.csv("male.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
female<-read.csv("female.csv",header=T,encoding = "UTF-8", stringsAsFactors = FALSE)
sex_cor <- c( "BMI", "LYM1",
             "PCO2",
             "LDH")


m<-as.data.frame(subset(male, X %in% sex_cor))
f<-as.data.frame(subset(female, X %in% sex_cor))


library(dplyr)

# Add a 'group' column to the 'm' data frame
m <- m %>% mutate(group = "m")

# Add a 'group' column to the 'f' data frame
f <- f %>% mutate(group = "f")

# Merge the 'm' and 'f' data frames
merged_data <- bind_rows(m, f)
merged_data
str(merged_data)
library(tidyr)
# 分离OR_with CI列
merged_data <- merged_data %>%
  separate(OR_with_CI, into = c("OR", "orl", "orh"), sep = "\\(|~|\\)")
merged_data
dt<-merged_data
dt$OR<-as.numeric(dt$OR)
dt$orh<-as.numeric(dt$orh)
dt$orl<-as.numeric(dt$orl)
dt$Vars<-c("BMI (Body Mass Index)","Lymphocyte ratio","Carbon dioxide partial pressure","Lactate dehydrogenase",
           " "," "," "," ")
dt$group1<-c("m1","m2","m3","m4","f1","f2","f3","f4")
dt
# 定义一个指定顺序的向量
specified_order <- c("m1","f1","m2","f2","m3","f3","m4","f4")

# 使用order函数按照指定顺序排列数据框
dt <- dt[order(match(dt$group1, specified_order)), ]
dt
dt$se<- (log(dt$orh) - log(dt$OR))/1.96
dt$` `<- paste(rep(" ", 10), collapse = " ")
dt$`HR(95% CI)` <- ifelse(is.na(dt$se), "",
                          sprintf("%.2f(%.2f to %.2f)",dt$OR,dt$orl, dt$orh))#sprintF返回字符和可变量组合
#----
library(grid)
#install.packages("forestploter")
library(forestploter)
dt


tm <- forest_theme(
  base_size = 12,
  #文本的大小
  ci_pch = 15,
  #可信区间点的形状
  ci_col = "black",
  #CI的颜色
  ci_fill = "lightblue",
  #ci颜色填充
  ci_alpha = 0.6,
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
pt <-
  forest(
    dt[, c(11, 14:15)],
    est = dt$OR,
    #效应值
    lower = dt$orl,
    #可信区间下限
    upper = dt$orh,
    #可信区间上限
    sizes = 1,
    #黑框的大小
    ci_column = 2,
    #在那一列画森林图，要选空的那一列
    ref_line = 1,
    arrow_lab = c("Better", "Worse"),
    xlim = c(0.9, 1.1),
    ticks_at = c(0.9, 1, 1.1),
    theme = tm
  )

pt

g2 <- edit_plot(pt,
                row = c(1:8),
                which = "background",
                gp = gpar(fill =
                            "white"))
g2
g <- edit_plot(
  g2,
  row = c(2, 4, 6, 8),
  col = 2,
  which = "ci",
  gp = gpar(fill =
              "pink")
)
g
