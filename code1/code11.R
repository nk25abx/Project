library(readxl) 
library (tidyr)
library(ggplot2)
lcd <- read_excel("D:/Team Research and Development/Project/LungCapData.xls")
View(lcd)
summary(lcd)
str(lcd)
View(lcd)
hist(lcd$LungCap)
LungCapacity<-(lcd$LungCap)
hist(LungCapacity)
h <- hist(LungCapacity,
          main="Lung Capacity",
          xlab="Lung Capacity (Liters) ",
          col="lightgreen",
          freq=FALSE
)

lines(density(LungCapacity), col = "purple")
ggplot(lcd, aes(x = Caesarean, y = `LungCap`)) + 
  geom_boxplot() + 
  labs(title = "Lung Capacity by Caesarean", x = "Caesarean birth", y = "Lung Capacity")

result <- t.test(LungCap ~ Caesarean, data = lcd)
result

