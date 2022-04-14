###------------------
###Visualization
###------------------

###Students Name: Lahari Pampati
###GNumber: G01350730

library(ggplot2)
rm(list=ls())

data <- read.csv("C:\\Users\\Lahar\\Downloads\\EmployeeAttrition(1).csv")
View(data)

ggplot(data)+
  geom_histogram(aes(x=Age),binwidth = 3,fill="red",color="black")+
  labs(title = "Histogram for Age",
       x= "Age",
       y="Count")

ggplot(data,aes(x=Age,y=MonthlyIncome))+
  geom_point()+
  labs(title = "Scatterplot for Age and Monthly Income",
       x= "Age",
       y="Monthly Income")

