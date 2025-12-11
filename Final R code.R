library(readr)
library(readxl)

LungCapData <- read_csv("LungCapData.csv")
View(LungCapData)

#reading columns and rows
colnames(LungCapData)
nrow(LungCapData)

#Changing Caesarean from yes & no to C-Section & Normal Delivery
LungCapData$Caesarean[is.na(LungCapData$Caesarean)] <- "no"

LungCapData$Caesarean[LungCapData$Caesarean == "no"]  <- 0
LungCapData$Caesarean[LungCapData$Caesarean == "yes"] <- 1

LungCapData$Caesarean <- as.numeric(LungCapData$Caesarean)

LungCapData$Caesarean <- factor(
  LungCapData$Caesarean,
  levels = c(0, 1),
  labels = c("Normal Delivery", "C-Section")
)

#Reading updated rows
table(LungCapData$Caesarean)
table(LungCapData$Smoke)
table(LungCapData$Gender)

#reading first 5 rows
head(LungCapData, 5)

#Analyzing bit 
summary(LungCapData$LungCap)
summary(LungCapData$Caesarean)
sd(LungCapData$LungCap)
sd(LungCapData$Age)
mean(LungCapData$LungCap)
median(LungCapData$LungCap)

# Mean of Lung Capacity by delivery method
aggregate(LungCap ~ Caesarean, data = LungCapData, FUN = mean)

# Median of Lung Capacity by delivery method
aggregate(LungCap ~ Caesarean, data = LungCapData, FUN = median)

#Boxplot (comparison of means)
boxplot(LungCap ~ Caesarean,
        data = LungCapData,
        main = "Lung Capacity by Delivery Method",
        xlab = "Birth Delivery Method",
        ylab = "Lung Capacity (litres)",
        col = c("lightblue", "lightgreen"))

#Histogram (comparison of means)
hist(LungCapData$LungCap,
     prob = T,
     breaks=20,
     xlab = "Lung Capacity (litres)",
     ylab = "Relative Frequency",
     main = "Distribution of Lung Capacity",
     col = "lightblue",
     las = 1)
axis(side = 1
)
x <- seq(min(LungCapData$LungCap), max(LungCapData$LungCap), length = 40)
f <- dnorm(x, mean = mean(LungCapData$LungCap), sd = sd(LungCapData$LungCap))
lines(x, f, col = "navy",lwd=2)

#Independent samples t-test (parametric)
t_test_result <- t.test(LungCap ~ Caesarean,
                        data = LungCapData,
                        paired = FALSE)
t_test_result

#p value
t_test_result$p.value