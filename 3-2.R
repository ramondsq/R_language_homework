# 第四章第2题
data <- read.csv("ch2-4.csv", encoding = "UTF-8")
print(data)

names(data)[1] <- "t"
names(data)

#(1)
print("提取2000年数据")
retval <- subset(data, t == 2000)
print(retval)
print("提取税收x2数据")
retval <- data$x2
print(retval)

#(2)
print("提取2001至2008年x4数据")
retval <- subset(data, t >= 2001 & t <= 2008)
retval2 <- retval$x4
print(retval2)