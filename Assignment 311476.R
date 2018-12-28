library(readxl)
library(dplyr)
library(tidyr)

titanic = read_xls('titanic3.xls',1, col_names = TRUE)
head(titanic, 2)
str(titanic)
titanic$name

#Family Names List
titanicNames = separate(titanic, name, c('familyName','givenName'), ',') %>% select(c('familyName','givenName')) 
#Titles List
titanicTitles = separate(titanicNames,givenName, c('title','lastname'), '\\.') %>% select(c('title'))
#New Titanic Dataset
titanic_v2 = cbind(titanicTitles, cbind(titanicNames, titanic))
#Summary of New Titanic by Title & by Family Name
titanic_v2 %>% group_by(familyName) %>% summarise(passengers=n()) %>% arrange(desc(passengers))
titanic_v2 %>% group_by(title) %>% summarise(passengers=n())%>% arrange(desc(passengers))

#remove trailing and leading spaces
titanic_v2$title=trimws(titanic_v2$title)
titanic_v2$familyName=trimws(titanic_v2$familyName)
#clean up Madame & Mademoiselle to English variants
titanic_v2[titanic_v2$title %in% c('Mlle'),'title'] = 'Miss'
titanic_v2[titanic_v2$title %in% c('Mme'),'title'] = 'Ms'

#Visualization Of Title
library(ggplot2)
ggplot(titanic_v2,aes(x= title)) + 
  geom_bar(stat = 'count')  +   labs(x = 'Title')  + labs(y ='Number of Passangers Holding Title')  

#Visualization of Family Size Impact on Survival  
titanic_v2$familySize = titanic_v2$sibsp + titanic_v2$parch + 1
ggplot(titanic_v2,aes(x= titanic_v2$familySize,  fill = factor(titanic_v2$survived))) + 
  geom_bar(position = 'fill')  +   labs(x = 'Family Size')  + labs(y ='Survival %') +ggtitle('Family Size Impact on Survival')+ labs(fill='Survived?') 

#Impute Age Using Mice
install.packages('mice')
library(mice)
set.seed(3)
titanic_v3 = titanic_v2[, names(titanic_v2) %in% c('age','sibsp','parch','fare','embarked')] 
imputedAge = complete(mice(titanic_v3, method = "rf", m=5))
par(mfrow=c(1,2))
hist(titanic_v3$age, main = "Before Imputation", col = "blue")
hist(imputedAge$age, main = "After Imputation", col = "red")
