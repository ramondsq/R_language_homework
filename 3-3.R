# 第三章第3题
print("(1) 读入数据")
data <- read.csv("ch3-3.csv", encoding = "UTF-8")
head(data)

print("(2) 对年龄、月收入、月支出排序")
print("对年龄排序")
temp <- data[ order(data$年龄), ]
head(temp)
print("对月收入排序")
temp <- data[ order(data$月收入), ]
head(temp)
print("对月支出排序")
temp <- data[ order(data$月支出), ]
head(temp)

print("(3) 筛选不同性别或不同受教育程度的人的观点")
print("筛选不同性别的人的观点")
retval <- subset(data, 性别 == "女")
head(retval)
retval <- subset(data, 性别 == "男")
head(retval)
print("筛选不同受教育程度的观点")
retval <- subset(data, 受教育程度 == "低")
head(retval)
retval <- subset(data, 受教育程度 == "中")
head(retval)
retval <- subset(data, 受教育程度 == "高")
head(retval)

print("(4) 筛选 C地区、女性、受教育程度为中、观点为不支持的人")
retval <- subset(data, 地区 == "C" & 性别 == "女" & 受教育程度 == "中" & 观点 == "不支持")
head(retval)

print("(5) 对年龄、月收入、月支出进行分组")
print("对年龄分组")
data$age = cut(data$年龄, breaks = c(min(data$年龄), 40, max(data$年龄)))
head(data)
print("对月收入分组")
data$income = cut(data$月收入, breaks = c(0, 1000, 2000, 5000))
head(data)
print("对月支出进行分组")
data$outcome = cut(data$月支出, breaks = c(0, 1500, 3000))
head(data)

print("(6) 删除观点中的缺失值")
data_na = na.omit(data)
head(data_na)