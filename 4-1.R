# 第四章第一题
#引入dstatR
source("dstatR2func.R")
data <- read.csv("ch2-3.csv", encoding = "UTF-8")
print(data)
#(1)
t1 = table(data$性别)
t1
png(file = "gender-ratio.png")
barplot(t1, col = c("#FF0000", "#00FF00"), names.arg = c("女", "男"))

#(2)
print("对于性别的频数表")
a <- Ftab(data$性别)
png(file = "gender-ratio2.png")
barplot(a)