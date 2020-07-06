library(kernlab)
library(caret)
library(ggplot2)
library(plyr)
?plyr

forest_fire <- read.csv(file.choose())
View(forest_fire)
summary(forest_fire)
class(forest_fire)
str(forest_fire)

hist(forest_fire$area)
rug(forest_fire$area)
?rug
          ###############TRANSFORM THE AREA VALUE TO Y ####
FF1 <- mutate(forest_fire, y =log(area +1))
?mutate
hist(FF1$y)
rug(FF1$y)
        ##################SUMMARY######################
summary(FF1)
        #####################PREDICTION OF FOREST FIRES REQUIRES ONLY PREDICTION FROM
# TEMP, RAIN, RH RELATIVE HUMANITY AND WIND 

##APPLY nORMALIZATION TECHNIQUR TO THE WHOLE DATASET

normalize <- function(x){
  forest_fire$temp = normalize(forest_fire$temp)
  forest_fire$rain = normalize(forest_fire$rain)
  forest_fire$wind = normalize(forest_fire$wind)
  forest_fire$RH = normalize(forest_fire$RH)
  
}
#####WE NEED TO TWEAK THIS AS A CLASSIFICATION PROBLEM LETS BASE OUT THE SIZE
#USING THE CRITICA

attach(forest_fire)
View(forest_fire)
      #DATA PARTITION
set.seed(123)
ind <- sample(2, nrow(forest_fire), replace = TRUE, prob = c(0.7,0.3))
FF_train <- forest_fire[ind==1,]
FF_test <- forest_fire[ind==2,]

library(e1071)
model1<-ksvm(size_category~temp+rain+wind+RH, 
               data= FF_train,kernel = "vanilladot")
model1
model1_prediction <- predict(model1,FF_test)
model1_prediction
mean(model1_prediction == FF_test$size_category) #####67.8


model2<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "rbfdot")
model2
model2_prediction <- predict(model2, FF_test)
model2_prediction
mean(model1_prediction == FF_test$size_category)#########68.41

model3<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "polydot")
model3
model3_prediction <- predict(model3, FF_test)
model3_prediction
mean(model3_prediction == FF_test$size_category)#########67.80

model4<-ksvm(size_category~temp+rain+wind+RH, 
             data= FF_train,kernel = "besseldot")
model4
model4_prediction <- predict(model4, FF_test)
model4_prediction
mean(model4_prediction == FF_test$size_category)   ##67.80
