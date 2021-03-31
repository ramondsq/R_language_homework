# 第四章第四题
source("dstatR2func.R")
data <- c(11,19,14,22,14,28,13,81,12,43,11,16,31,16,23,42,22,26,17,22,13,27,108,16,43,82,14,11,51,76,28,66,29,14,14,65,37,16,37,35,39,27,14,17,13,38,28,40,85,32,25,26,16,120,54,50,18,27,16,14,33,29,77,50,19,34)

print("(1) 绘图分析")

print("(2) 分析数据的集中趋势和离散程度")
print("均值")
mean(data)
print("中位数")
median(data)
print("方差")
var(data)
print("标准差")
sd(data)

print("(3) 均值和中位数相差大说明财富分配不均，方差和标准差是看数据稳定程度的")

print("(4) 绘制散点图和直方图")
print("散点图")
png(file = "plot4-4.png")
plot(data)
print("直方图")
png(file = "hist4-4.png")
hist(data)

print("(5) 频数表和频数图")
png(file = "freq4-4.png")
Freq(data)