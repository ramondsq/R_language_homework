
data <- read.csv("ch2-3.csv", encoding = "UTF-8")

print(data)
#(1)����
names(data)[1] <- "number"
names(data)[2] <- "gender"
names(data)[3] <- "maths"
names(data)[4] <- "stat"

names(data)
#(2)����
print("���Ա�����")
data[order(data$gender),]
print("����ѧ����")
data[order(data$maths),]
print("��ͳ��ѧ����")
data[order(data$stat),]
#(3)ɸѡ
print("��ʾ���Գɼ�")
male <- subset(data, gender == "M")
print(male)
print("��ʾŮ�Գɼ�")
female <- subset(data, gender == "F")
print(female)
#(4)ɸѡ2
print("ɸѡ��ѧ80�����ϣ�ͳ��90���ϵ�")
retval <- subset(data, maths > 80 & stat > 90)
print(retval)
print("ɸѡ��ѧ��ͳ�ƶ�60���ϵ�")
retval <- subset(data, maths > 60 & stat > 60)
print(retval)
#(5)����
print("����ѧ�ɼ�����")
data$maths_cut = cut(data$maths, breaks = c(0,60,100))
head(data)
print("��ͳ��ѧ�ɼ�����")
data$stat_cut = cut(data$stat, breaks = c(0,60,100))
head(data)