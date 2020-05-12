
# Raj Muthiah 05/12/2020

# Analysis of Titanic passenger survival using R
# Train and Test datasets available at https://www.kaggle.com/c/titanic/data

#LOAD DATASETS AND PACKAGES
#load packages
#caTools is used for actual modelling of the data
#ggplot2 is used for data visualization
#Amelia is used for data visualization to understand missing data
library(caTools)
library(ggplot2)
library(Amelia)
library(sqldf)

#Import datasets and choose the file stored in your directory location
#Train and Test datasets have identical columns, 
#except that Test dataset doesn't have the 'Survived' column 
train <- read.csv(file.choose(), stringsAsFactors = T)
test <- read.csv(file.choose(), stringsAsFactors = T)
head(train)
head(test)

str(train)
str(test)


#DATA CLEANING
#Checking datasets for null values
#Both datasets have null values
any(is.na(train))
any(is.na(test))

#Visualizing missing data using Amelia package
#Train dataset has missing values for Age
#Test dataset has missing values for Age and Fare
missmap(train, 
        main='Titanic training dataset missing values',
        col=c('red', 'white'),
        legend=F)
train[which(is.na(train$Age)), ]

missmap(test, 
        main='Titanic test dataset missing values',
        col=c('red', 'white'),
        legend=F)
test[which(is.na(test$Age)), ]
test[which(is.na(test$Fare)), ]

#Impute missing data

#Training Dataset
#Checking the distribution of Age across Sex
#Age distribution between sexes is similar
ggplot(train, aes(Sex, Age))+
  geom_boxplot(aes(group=Sex, fill=factor(Sex)))

#Checking the distribution of Age across Passenger Class
#Age distribution across Pclass is different, Higher Pclass have older passengers
#Impute missing Age with average age of passengers in each class
ggplot(train, aes(Pclass, Age))+
  geom_boxplot(aes(group=Pclass, fill=factor(Pclass)))

trainage <- sqldf('select distinct Pclass, avg(Age) as MAge 
                 from train 
                 group by Pclass 
                 order by Pclass')

train <- merge(train, trainage, by='Pclass', all.x=T)
train$Age <- ifelse(is.na(train$Age), train$MAge, train$Age)
train$MAge<-NULL

train[which(is.na(train$Age)), ]


#Testing Dataset
#Checking the distribution of Age across Sex
#Age distribution between sexes is similar
ggplot(test, aes(Sex, Age))+
  geom_boxplot(aes(group=Sex, fill=factor(Sex)))

#Checking the distribution of Age across Passenger Class
#Age distribution across Pclass is different, Higher Pclass have older passengers
#Impute missing Age with average age of passengers in each class
ggplot(test, aes(Pclass, Age))+
  geom_boxplot(aes(group=Pclass, fill=factor(Pclass)))

testage <- sqldf('select distinct Pclass, avg(Age) as MAge 
                 from test 
                 group by Pclass 
                 order by Pclass')

test <- merge(test, testage, by='Pclass', all.x=T)
test$Age <- ifelse(is.na(test$Age), test$MAge, test$Age)
test$MAge<-NULL

test[which(is.na(test$Age)), ]

#Checking the distribution of Fare across Passenger Class
#Missing Fare value is for passenger in Pclass=3
#Impute missing Fare with average fare value in Pclass
ggplot(test, aes(Pclass, Fare))+
  geom_boxplot(aes(group=Pclass, fill=factor(Pclass)))

test[which(is.na(test$Fare)), ]

testfare <- sqldf('select distinct Pclass, avg(Fare) as MFare 
                 from test 
                 group by Pclass 
                 order by Pclass')

test <- merge(test, testfare, by='Pclass', all.x=T)
test$Fare <- ifelse(is.na(test$Fare), test$MFare, test$Fare)
test$MFare<-NULL

test[which(is.na(test$Fare)), ]


#EXPLORATORY ANALYSIS
#Check survival
ggplot(train, aes(Survived))+geom_bar()

ggplot(train, aes(Sex))+geom_bar()

ggplot(train, aes(Survived))+geom_bar(aes(fill=Sex))


