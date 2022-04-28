###------------------
###Regression and Clustering
###------------------

###Students Name: Lahari Pampati
###GNumber: G01350730


rm(list=ls())

install.packages(ggplot2)

library(ggplot2)
library(cluster)
library(ClusterR)

data <- read.csv("C:\\Users\\Lahar\\Downloads\\EmployeeAttrition(1).csv")


# Your answers here...
View(data)

#1.a
with(cars, scatter.smooth(data$TotalWorkingYears, data$MonthlyIncome,col='blue',lpars =
                            list(col = "red", lwd = 3, lty = 3)))
# As TotalWorkingYears increase the MonthlyIncome also increased, thus both are positively correlated

#1.b
with(cars, scatter.smooth(data$Age, data$DistanceFromHome,col='blue', lpars =
                            list(col = "red", lwd = 3, lty = 3)))
# It is not possible to determine the relation between Age and DistanceFromHome

#1.c
a <- cor.test(data$TotalWorkingYears, data$MonthlyIncome, 
              method = "pearson")
print(a)
# The correlation value for TotalWorkingYears and MonthlyIncome is 0.7728932, therefore they are positively correlated
b <- cor.test(data$Age, data$DistanceFromHome, 
              method = "pearson")
print(b)
#The correlation value for Age and DistanceFromHome is -0.00168612, since the value is negative and is nearer to zero 
#we can say that they are either negatively correlated or they are not correlated


#1.d
model = lm(data$TotalWorkingYears ~ data$MonthlyIncome)
print(model)
summary(model)
ggplot(data, aes(x = TotalWorkingYears, y= MonthlyIncome), na.rm= TRUE) +
  geom_point() +
  labs(x = "TotalWorkingYears", 
       y = "MonthlyIncome", 
       title = "Relationship between TotalWorkingYears and MonthlyIncome using scatterplot")+
  theme(plot.title = element_text(hjust = 0.5))
# p-value is less than 2.2e-16, thus the relationship is significant and has strong evidence against null hypothesis

#2.a
df <- data['HourlyRate']+data['TotalWorkingYears']
print(df)

km1 <- kmeans(df,centers = 3)
ggplot(data,aes(HourlyRate,TotalWorkingYears, color = km1$cluster )) + 
  geom_point()+
  labs(x = "Hourly Rate", 
       y = "Total Working Years", 
       title = "K-means with 3 clusters")+
  theme(plot.title = element_text(hjust = 0.5))
# Ranges of Hourly rate in each cluster are,Cluster 1 : 1-65, Cluster 2 : 65-90, Cluster 3 : 90-100

#2.b
km2 <- kmeans(df,centers = 5)
ggplot(data,aes(HourlyRate,TotalWorkingYears, color = km2$cluster )) +
  geom_point()+
  labs(x = "Hourly Rate", 
       y = "Total Working Years", 
       title = "K-means with 5 clusters")+
  theme(plot.title = element_text(hjust = 0.5))
# Ranges of Hourly rate in each cluster are,Cluster 1 : 1-58, Cluster 2 : 58-70, Cluster 3 : 70-85, Cluster 4 : 85-100, Cluster 5: 83-100
