library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)
salary_Test <- read.csv(file.choose())  ###########SALARY_TEST.CSV
View(salary_Test)
str(salary_Test)



salary_Train <- read.csv(file.choose()) #SALARY_TRAIN.CSV
View(salary_Test)
str(salary_Test)

#VANILLADOT
salary_vanilladot <- ksvm(Salary~., data = salary_Train, kernel="vanilladot")
salary_vanilladot
salary_vanilladot_prediction <- predict(salary_vanilladot, salary_Test)
salary_vanilladot_prediction
mean(salary_vanilladot_prediction == salary_Test$Salary)  #####84.6%

####RBFDOT
salary_rdf <- ksvm(Salary~., data = salary_Train,
                   kernel = "rbfdot")
salary_rdf
salary_rdf_prediction <- predict(salary_rdf, salary_Test)
salary_rdf_prediction
mean(salary_rdf_prediction == salary_Test$Salary) #####85.4%

##########KERNEL = BESSELDOT

salary_besseldot <- ksvm(Salary~., data = salary_Train,
                         kernel = "besseldot")
salary_besseldot
salary_predictions_besseldot <- predict(salary_besseldot, salary_Test)
salary_predictions_besseldot
mean(salary_predictions_besseldot == salary_Test$Salary) #####77%

##########KERNEL = polydot

salary_polydot <-  ksvm(Salary~., data = salary_Train,
                        kernel = "polydot")
salary_polydot 
salary_predictions_polydot <- predict(salary_polydot,salary_Test)
salary_predictions_polydot
mean(salary_predictions_polydot == salary_Test$Salary) ####84.8


###converting 
salary_Train$educationno <- as.factor(salary_Train$educationno)
class(salary_Data)
###VISUALIZATION

plot(salary_Train$workclass,salary_Train$Salary)

plot(salary_Train$education,salary_Train$Salary)

plot(salary_Train$educationno,salary_Train$Salary)

plot(salary_Train$age,salary_Train$Salary)

plot(salary_Train$maritalstatus,salary_Train$Salary)

plot(salary_Train$occupation,salary_Train$Salary)

plot(salary_Train$relationship,salary_Train$Salary)

plot(salary_Train$race,salary_Train$Salary)

plot(salary_Train$sex,salary_Train$Salary)

plot(salary_Train$capitalgain,salary_Train$Salary)


###########CAPITALGAIN

ggplot(data = salary_Train, aes(x=salary_Train$Salary, y = salary_Train$capitalgain,fill = salary_Train$Salary))+
  geom_boxplot()+
  ggtitle("capitalgain")

##############capitalloss
ggplot(data = salary_Train, aes(x=salary_Train$Salary, y = salary_Train$capitalloss,fill = salary_Train$Salary))+
  geom_boxplot()+
  ggtitle("capitalloss")

##############HoursPerWeek
ggplot(data = salary_Train, aes(x=salary_Train$Salary, y = salary_Train$hoursperweek,fill = salary_Train$Salary))+
  geom_boxplot()+
  ggtitle("HoursPerWeek")

#################Salary-train
ggplot(data = salary_Train, aes(x=salary_Train$Salary, y = salary_Train$education,fill = salary_Train$Salary))+
  geom_boxplot()+
  ggtitle("Salary-train")

################AGE
ggplot(data = salary_Data, aes(x=salary_Train$Salary, y = salary_Train$age,fill = salary_Train$Salary))+
  geom_boxplot()+
  ggtitle("age")

ggtitle("Age - Density Plot")

################WORKCLASS
ggplot(data = salary_Data, aes(x=salary_Data$Salary, y = salary_Data$workclass,fill = salary_Data$Salary))+
  geom_boxplot()+
  ggtitle("WORKCLASS")

################EDUCTIONNO
ggplot(data = salary_Data, aes(x=salary_Data$Salary, y = salary_Data$educationno,fill = salary_Data$Salary))+
  geom_boxplot()+
  ggtitle("EDUCTIONNO")


ggplot(data=salary_Data,aes(x = salary_Data$educationno, fill = salary_Data$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


ggplot(data=salary_Data,aes(x = salary_Data$sex, fill = salary_Data$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

