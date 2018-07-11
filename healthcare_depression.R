
h_data=read.csv("/Users/yashasvikathotia/Desktop/BDVA/Project/Healthcare.csv")
View(h_data)

#removing unneceesary coloumns 
h_data = h_data[c(1,3,46,47,51,56,57,60,61)]
summary(h_data)

#renaming no, yes, maybe to 0,1,2

colnames(h_data)=c("self_employed","organization","family_history","past","diagnosis","age","gender","country","state")

h_data$organization[is.na(h_data$organization)] = 1

h_data$family_history=factor(h_data$family_history, 
                             levels=c("No","Yes","Maybe", "I don't know"), 
                             labels = c('0','1','2','3'))

h_data$past=factor(h_data$past,
                   levels=c("No","Yes","Maybe", "I don't know"), 
                   labels = c('0','1','2','3'))

h_data$diagnosis=factor(h_data$diagnosis, 
                levels=c("No","Yes"), 
                labels = c('0','1'))

#removing rows with NA's 

h_data = h_data[complete.cases(h_data),]

#replacing male to M and female to F and other to O

h_data$gender = ifelse(h_data$gender=='m' |  
                                       h_data$gender=='male' | 
                                       h_data$gender=='Male','M',
                                     ifelse(h_data$gender=='f' |
                                              h_data$gender=='female' |
                                              h_data$gender=='Female' |
                                              h_data$gender=='I identify as female.','F','0'))

#Country

h_data$country=as.character(h_data$country)
h_data$country[h_data$country=="Back-end Developer|DevOps/SysAdmin|Supervisor/Team Lead|Executive Leadership"]="United States of America"
country_data_check = as.data.frame(unique(h_data$country))
h_data$country=as.factor(h_data$country)

#state 

h_data$state=as.character(h_data$state)
state_data_check = as.data.frame(unique(h_data$state))
h_data$state=as.factor(h_data$state)

#converting to factors/numerics
h_data$diagnosis = as.factor(h_data$diagnosis)
h_data$age = as.numeric(h_data$age)
h_data$gender = as.factor(h_data$gender)
h_data$self_employed = as.factor(h_data$self_employed)
h_data$organization = as.factor(h_data$organization)

#Data exploration
library(ggplot2)
ggplot(h_data, aes(x = diagnosis, fill = diagnosis))+ geom_bar()
hist(h_data$age)

#splitting data
library(caTools)
set.seed(123)
data_split = sample.split(h_data$diagnosis, SplitRatio = 0.8)

hdata_train = subset(h_data, data_split==T)
hdata_test = subset(h_data, data_split==F)

#random forest 
#install.packages("randomForest")
library("randomForest")

#coverting to factors in training set 
hdata_train$diagnosis = as.factor(hdata_train$diagnosis)


#optimized value of mtry 
h_mtry = tuneRF(x = hdata_train[,c("self_employed","organization", "family_history", "past", "age", "gender", "country", "state")],y = hdata_train$diagnosis,
                stepFactor = 1.2, improve = 0.01, trace = T, plot = T)

health_forest = randomForest(diagnosis~., hdata_train, 
                             keep.forest = TRUE, mtry=2, ntree=500, importance=TRUE )
health_forest
summary(health_forest)

#Extract 1 tree
getTree(health_forest,1)

#Finding the important variable
importance(health_forest)
varImpPlot(health_forest)

pred=predict(health_forest, hdata_test, type = "class")
pred

t = table(predictions = pred, actual = hdata_test$diagnosis)
t

#Accuracy
sum(diag(t))/sum(t)

#Plot ROC curve and calculate AUC metric
#install.packages("pROC")
library(pROC)
pred = predict(health_forest, hdata_test, type = "prob")
auc = auc(hdata_test$diagnosis, pred[,2])
plot(roc(hdata_test$diagnosis, pred[,2]))

