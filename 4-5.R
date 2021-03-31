# 第四章第五题
source("dstatR2func.R")
data <- read.csv("ch3-3.csv", encoding = "UTF-8")
head(data)

print("(1) 生成地区、性别、受教育程度、观点的频数表和图")

location <- Ftab(data$地区)
png(file = "freq-loc-4-5.png")
barplot(location)

gender <- Ftab(data$性别)
png(file = "freq-gen-4-5.png")
barplot(gender)

edu <- Ftab(data$受教育程度)
png(file = "freq-edu-4-5.png")
barplot(edu)

viewpoint <- Ftab(data$观点)
png(file = "freq-view-4-5.png")
barplot(viewpoint)

print("(2) 计算年龄、月收入、月支出基本统计量")
Stats(data$年龄)
Stats(data$月收入)
Stats(data$月支出)

print("(3) 计算年龄、月收入、月支出的探索性统计图")
png(file = "exp-age-4-5.png")
EDA(data$年龄)
png(file = "exp-income-4-5.png")
EDA(data$月收入)
png(file = "exp-outcome-4-5.png")
EDA(data$月支出)

print("(4) 计算年龄、月收入、月支出的频数表和频数图")
png(file = "freq-age-4-5.png")
Freq(data$年龄)
png(file = "freq-income-4-5.png")
Freq(data$月收入)
png(file = "freq-outcome-4-5.png")
Freq(data$月支出)

print("(5) 多变量联合分析")
tsr = table(data$性别, data$地区)
tsr