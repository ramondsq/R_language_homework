# 第四章第三题
source("dstatR2func.R")
data <- c(2050,2100,2200,2300,2350,2450,2500,2700,2900,2850,3500,3800,2600,3000,3300,3200,4000,3100,4200,3500)
#(1)
print("均值")
mean(data)
print("中位数")
median(data)
print("方差")
var(data)
print("标准差")
sd(data)

#(2)
print("散点图")
x <- (1:20)
png(file = "plot4-3.png")
plot(x, data, "p")
print("直方图")
png(file = "hist4-3.png")
hist(data)

#(3)
print("频数表")
png(file = "freq4-3.png")
Freq(data)