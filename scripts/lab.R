#install.packages("tableone")
rm(list=ls())
library(tableone)
setwd("E:/MG/raw")
data<-read.csv("lab.csv",header=T)
head(data)
str(data)


mytable <- CreateTableOne(vars = c("BMI", "WBC", "NEU", "NEU.1", "LYM", "LYM.1", "NEU.LYM", "MON", 
                                   "MON.1", "HGB", "PLT", "CRP", "ESR", "PCT", "PH", "SO2", "PCO2","PO2","DD","PT",
                                   "TT","LDH","AST","ALT","TBIL","CREA","UREA","CK","TNI","CKMB","MYO",
                                   "BNP", "NK", "CD19", "CD3", "CD3CD4CD8", "CD3CD1656", "CD3CD19", "CD3_A", "CD4",
                                   "CD8", "CD3CD4CD8_A", "CD4.CD8", "IFNa", "IFNy", "IL10", "IL12", "IL17", "IL1b", "IL2", "IL4","IL5","IL6",
                                   "IL8", "TNFa", "ASTALT", "UREACREA"), 
                          data = data, testNonNormal = wilcox.test,
                          strata = "SEX",addOverall = TRUE)


tab <- print(mytable, quote = FALSE, noSpaces = TRUE)



wilcox.test(data$IL6 ~ data$SEX, paired = FALSE)

# 假设你的数据框叫做data
# 用一个循环来进行U检验
for (i in 3:59) {
  # 提取第i列的变量名
  varname <- names(data)[i]
  # 进行U检验，并输出结果
  print(paste("U检验结果 for", varname))
  print(wilcox.test(data[,i] ~ data$SEX))
}

# 或者用apply函数来进行U检验
# 定义一个函数来进行U检验，并返回p值
wilcox_pvalue <- function(x) {
  return(wilcox.test(x ~ data$SEX)$p.value)
}
#对指定列应用该函数，并输出结果
p<-print(apply(data[,c(3:59)], 2, wilcox_pvalue))
result <- cbind(tab, p)
print(result)


write.csv(result,file = "lab_table2_new.csv")
