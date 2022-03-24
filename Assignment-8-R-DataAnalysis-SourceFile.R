###--------------------------------------
#Student Name: Lahari Pampati
#GNumber: G01350730
###--------------------------------------

rm(list=ls())

df <- read.csv("C:\\Users\\Lahar\\Downloads\\EmployeeAttrition(1).csv",as.is=TRUE)

# this is just for testing to use "print" statement.
print(df[1,])

# a. Find the number of rows and columns in the dataset (5 points)
print(ncol(df))
print(nrow(df))

# b. Find the maximum Age in the dataset (5 points)
maxAge <- max(df$Age)
print(maxAge)

# c. Find the minimum DailyRate in the dataset (5 points)
minRate <- min(df$DailyRate)
print(minRate)

# d. Find the average/mean MontlyIncome in the dataset (5 points)
avgIncome <- mean(df$MonthlyIncome)
print(avgIncome)

# e. How many employees rated WorkLifeBalance as 1 (5 points)
sum(df$WorkLifeBalance=='1')

# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)
(sum(df$TotalWorkingYears<='5')/nrow(df))*100
(sum(df$TotalWorkingYears>'5')/nrow(df))*100

# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)
x <- subset(df,select=c('EmployeeNumber','Department','MaritalStatus'),df$Attrition=='yes' && df$RelationshipSatisfaction=='1' && df$YearsSinceLastPromotion>3)
print(x)

# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
males <- subset(df,df$Gender=='Male')
females <- subset(df,df$Gender=='Female')
mean(males$EnvironmentSatisfaction)
mean(females$EnvironmentSatisfaction)

median(males$EnvironmentSatisfaction)
median(females$EnvironmentSatisfaction)

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}

male_sat <- subset(df,Gender=='Male',select="EnvironmentSatisfaction")
males_mode <-getmode(df$EnvironmentSatisfaction)
print(males_mode)
female_sat <- subset(df,Gender=='Female',select="EnvironmentSatisfaction")
female_mode <- getmode(df$EnvironmentSatisfaction)
print(female_mode)

sd(females$EnvironmentSatisfaction)
sd(males$EnvironmentSatisfaction)

table(females$EnvironmentSatisfaction)
table(males$EnvironmentSatisfaction) 

#part 2 
df1 = read.csv("C:\\Users\\Lahar\\Downloads\\Acme.csv",as.is=TRUE)
#2.1
str(df1)

#2.2
summary(df1)

#2.3
hist(df1$Years,xlab="Years",col="Blue")
hist(df1$StSalary,xlab="Starting salary",col="Red")

ggplot(df1,aes(x=Gender,fill=Gender))+
  geom_bar()+
  scale_x_discrete(labels = c("Female","Male"))

ggplot(df1,aes(x=Degree,fill=Degree))+
  geom_bar()

#2.4a
plot(df1$StSalary,df1$Years,pch=19,col='black',title(main = "Years of Experience and Starting Salary for all employees"))+
  abline(lm(df1$Years~df1$StSalary),col='red')

#2.4b
male_data <- subset(df1,Gender=='M')
plot(male_data$Years,male_data$StSalary,pch=19,col='black',xlab="Years",ylab = "Starting Salary",title(main = "Years of Experience and Starting Salary for all male employees"))+
  abline(lm(male_data$StSalary~male_data$Years),col='red')

female_data <- subset(df1,Gender=='F')
plot(female_data$Years,female_data$StSalary,pch=19,col='black',xlab="Years",ylab = "Starting Salary",title(main = "Years of Experience and Starting Salary for all female employees"))+
  abline(lm(female_data$StSalary~female_data$Years),col='red')

#2.4c
bs <- subset(df1,Degree=="BS")
plot(bs$Years,bs$StSalary,pch=19,col='black',xlab="Years",ylab = "Starting Salary",title(main = "Years of Experience and Starting Salary for BS degree"))+
  abline(lm(bs$StSalary~bs$Years),col='red')

ms <- subset(df1,Degree=="MS")
plot(ms$Years,ms$StSalary,pch=19,col='black',xlab="Years",ylab = "Starting Salary",title(main = "Years of Experience and Starting Salary for MS degree"))+
  abline(lm(ms$StSalary~ms$Years),col='red')

phd <- subset(df1,Degree=="PhD")
plot(phd$Years,phd$StSalary,pch=19,col='black',xlab="Years",ylab = "Starting Salary",title(main = "Years of Experience and Starting Salary for PhD degree"))+
  abline(lm(phd$StSalary~phd$Years),col='red')

#2.5a
#female employees
cor(female_data$Years,female_data$StSalary,method = "pearson")
cor.test(female_data$Years,female_data$StSalary,method = "pearson")
#male employees
cor(male_data$Years,male_data$StSalary,method = "pearson")
cor.test(male_data$Years,male_data$StSalary,method = "pearson")

#We can say that the number of years and starting salary are proportional strongly related in between both the genders
# The correlation coefficient for female employees and male employees is 0.75 and 0.67 respectfully
# The correlation coefficient is different for each gender.

#2.5b
#BS Degree
cor(bs$Years,bs$StSalary,method = "pearson")
cor.test(bs$Years,bs$StSalary,method = "pearson")

#MS Degree
cor(ms$Years,ms$StSalary,method = "pearson")
cor.test(ms$Years,ms$StSalary,method = "pearson")

#PhD Degree
cor(phd$Years,phd$StSalary,method = "pearson")
cor.test(phd$Years,phd$StSalary,method = "pearson")

#The correlation coefficient for BS degree is 0.35
#The correlation coefficient for MS degree is 0.45
#The correlation coefficient for PhD degree is 0.045
# We can say that the the relation between degree and number years is weak.
#The correlation coefficient is different for each degree.

#2.6
#Males and Females with degree MS and BS and 1-6 years of experience earn similiar starting salary.
#But there is only 1 female who is a PhD and is earning similar to that of with Male PhDs with similar years of experience.
#According to me, there are only minor differences in the starting salaries of male and female employees but cannot draw conclusion in the case of female PhDs as there is only 1 such case.