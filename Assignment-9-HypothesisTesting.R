###------------------
###Hypothesis Testing
###------------------

###Students Name: Lahari Pampati
###GNumber: G01350730

setwd("~/git/AIT580/")

rm(list=ls())
df <- read.csv("C:\\Users\\Lahar\\Downloads\\EmployeeAttrition(1).csv",as.is=TRUE)

# Your hypothesis testings here...
#1: If the MonthlyIncome of Males is greater than Females
male= which(df$Gender=='Male')
female= which(df$Gender=='Female')
print(t.test(df$MonthlyIncome[male],df$MonthlyIncome[female],alternative="greater", var.equal=T))
# We reject the null hypothesis as the p-value (.8891) is greater than the significance level. 

#2: If the WorkLifeBalance of Males is less than Females
male= which(df$Gender== 'Male')
female= which(df$Gender== 'Female')
print(t.test(df$WorkLifeBalance[male],df$WorkLifeBalance[female],alternative="less",var.equal=T))
# We reject the null hypothesis as the p-value (0.458) is greater than the significance level. 

#3: If the YearsAtCompany of Single is less than Married
Single_df= which(df$MaritalStatus=='Single')
Married_df= which(df$MaritalStatus=='Married')
print(t.test(df$YearsAtCompany[Single_df],df$YearsAtCompany[Married_df],alternative="less"))
#We reject the null hypothesis and support the claim because the p-value (0.004973) is less than the significance level.

#4: If the EnvironmentalSatisfaction of Attrition=Yes is less than Attrition=No
a_yes= which(df$Attrition== 'Yes')
a_no= which(df$Attrition== 'No')
print(t.test(df$EnvironmentSatisfaction[a_yes],df$EnvironmentSatisfaction[a_no],alternative="less"))
#We reject the null hypothesis and support the claim because the p-value (0.0001046) is less than the significance level.

#5: If the MonthlyIncome of Manager is greater than Laboratory Tehcnician (Hint: Use JobRole to find Manager and Laboratory Technician)
manager= which(df$JobRole== 'Manager')
technician= which(df$JobRole== 'Laboratory Technician')
print(t.test(df$MonthlyIncome[manager],df$MonthlyIncome[technician],alternative="greater"))
#We can reject the null hypothesis and support the claim because the p-value (2.2e-16) is less than the significance level.

#6: If YearsAtCompany and DailyRate are correlated with each other
print(cor.test(df$YearsAtCompany,df$DailyRate))
# We reject the null hypothesis as the p-value (0.1919) is greater than the significance level. 
#Therefore,we can say that the YearsAtCompany and DailyRate are not correlated.

#7: If YearsAtCompany and MonthlyIncome are correlated with each other
print(cor.test(df$YearsAtCompany,df$MonthlyIncome))
#We reject the null hypothesis and support the claim as the p-value (2.2e-16) is less than the significance level.
#There is a positive correlation between YearsAtCompany and MonthlyIncome

#8: If YearsAtCompany varies depending on individual's MaritalStatus
summary(aov(df$YearsAtCompany ~ df$MaritalStatus))
#We can say that the YearsAtCompany varies depending on individual MaritalStatus

#9: If MonthlyIncome varies depending on individual's PerformanceRating
summary(aov(df$MonthlyIncome ~ df$PerformanceRating))
#We can say that the MonthlyIncome varies depending on PerformanceRating

#10: If MonthlyIncome varies depending on individual's WorkLifeBalance
summary(aov(df$MonthlyIncome ~ df$WorkLifeBalance))
#We can say that the claim that MonthlyIncome varies depending on varying WorkLifeBalance
