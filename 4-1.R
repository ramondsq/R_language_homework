
data <- read.csv("ch2-3.csv", encoding = "UTF-8")

print(data)
#(1)改名
names(data)[1] <- "number"
names(data)[2] <- "gender"
names(data)[3] <- "maths"
names(data)[4] <- "stat"

names(data)
#(2)排序
print("对性别排序")
data[order(data$gender),]
print("对数学排序")
data[order(data$maths),]
print("对统计学排序")
data[order(data$stat),]
#(3)筛选
print("显示男性成绩")
male <- subset(data, gender == "M")
print(male)
print("显示女性成绩")
female <- subset(data, gender == "F")
print(female)
#(4)筛选2
print("筛选数学80分以上，统计90以上的")
retval <- subset(data, maths > 80 & stat > 90)
print(retval)
print("筛选数学和统计都60以上的")
retval <- subset(data, maths > 60 & stat > 60)
print(retval)
#(5)锟斤拷锟斤拷
print("对数学成绩分组")
data$maths_cut = cut(data$maths, breaks = c(0,60,100))
head(data)
print("对统计学成绩分组")
data$stat_cut = cut(data$stat, breaks = c(0,60,100))
head(data)