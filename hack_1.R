#Reading the data file
df = read.csv('bank-full.csv',sep = ';')
head(df)
str(df)
any(is.na(df))
unique(df$education)  
unique(df$default)
unique(df$contact)
 

#Dplyr for data manipulation
library(dplyr)
x = df %>% group_by(month) %>% summarize(sum = sum(day))
x

#Just to get a sense of the total number of days
sum(df$day)
#Documentation for cut function - splitting into bins
?cut

max(df$age)
min(df$age)

age_col = cut(df$age,8,c('[15 - 25]','[26 - 35]','[36 - 45]','[46 - 55]','[56 - 65]','[66 - 75]','[76 - 85]','[86 - 95]'))
df = cbind(df,age_col)

library(ggplot2)

df$age_col1 = NULL
df$age1_col = NULL
ggplot(df,aes(age_col)) + geom_histogram(stat = 'count')

ggplot(df,aes(marital)) + geom_histogram(stat="count")

str(df)

df$pdays = as.factor(df$pdays)

a = filter(df,balance < 0,)
count(a)

?glm

col = subset(df,subset = (education == 'secondary' & balance < 0) | (education == 'tertiary' & balance < 0))
count(col)
col1 = 
count(filter(df))



#Education
col = subset(df,subset = (education == 'secondary' & balance < 0) | (education == 'tertiary' & balance < 0))
count(col)
#Count of above is 3031

col1 = subset(df,subset = (education == 'secondary' | education == 'tertiary'))
count(col1)
#Count of above is 36503

col2 = subset(df,subset = (education == 'secondary' & y == 'yes') | (education == 'tertiary' & y == 'yes'))
count(col2)
#Count of above is 4446

col3 = subset(df,subset = (education == 'secondary' & y == 'no') | (education == 'tertiary' & y == 'no'))
count(col3)
#Count of above is 32057

c = subset(df,subset = (balance < 0))
c1 = subset(df,subset = (balance < 0 & age_col == '[56 - 65]')|(balance < 0 & age_col == '[66 - 75]')|(balance < 0 & age_col == '[76 - 85]')|(balance < 0 & age_col == '[86 - 95]'))
count(c)
count(c1)
max(df$campaign)

c = subset(df,subset = (y == 'yes' & campaign == '1'))
count(c)

df$campaign = as.factor(df$campaign)
str(df)
unique(df$campaign)

df$previous = as.factor(df$previous)

c$balance = abs(c$balance)
head(c$balance)

#Model1 on dataset1 
data = read.csv('bank_full_iteration_1.csv')    
head(data)
data$duration = NULL
data$X = NULL
colnames(df)  
head(data)

library(e1071)  
#Setting the train test split as 75% and 25% respectively.
s = sample.split(df$y,SplitRatio = 0.75)
train = subset(data,subset = s == T)
test =  subset(data,subset = s == F)

#Using logistic regression

logit1 = glm(formula = y ~ ., family = binomial(link = "logit"), 
             data = train)
logit1
pred1 = predict(logit1,test,type = 'response')
pred1
pred1 = as.data.frame(pred1)
table('predicted' = pred1 > 0.5,'actual' = test$y)
anova(logit1, test="Chisq")

#Using random forest on model 1
library(randomForest)
str(data)
?randomForest
m = randomForest(y~.,data = train)
m  
summary(m)

y = predict(m,test)

y = as.data.frame(y)
y

table(y$y,test$y)
?rmse

install.packages('Metrics')
library(Metrics)


y = as.vector(y)
test = as.vector(test)


mod = glm(y~.,data = train,family = 'binomial')
summary(mod)

install.packages('pscl')
library(pscl)
pR2(mod1)  # look for 'McFadden'

mod1 = glm(y~job + education + balance + housing + loan + contact + pdays + quarter + age_bin_pd_cut + contacted_1_to_4 + duration_binary, 
           data = train,family = 'binomial')
summary(mod1)

head(data)

normalize = function(x){
  return((x - min(x))/(max(x) - min(x)))
}
?sapply

df$balance = sapply(df,normalize(df$balance))

m


# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")

train(y~., train, method = "rf", metric= "Accuracy", trControl = trainControl(), tuneGrid = NULL)


set.seed(1234)
# Run the model
rf_default <- train(y~.,
                    data = train,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)

?randomForest
adwait = randomForest(y~.,data = train)


file = write.csv(frame,'adwait.csv')

frame = read.csv('adwait.csv')
head(frame)

frame$pdays = NULL
s = sample.split(frame$y,SplitRatio = 0.80)
train = subset(frame,subset = s == T)
test =  subset(frame,subset = s == F)
rf = randomForest(y~.,data = train)
summary(rf)
rf

pred = predict(rf,test)

table(pred,test$y)
head(frame)

a = read.csv('bank_full_iteration_2.csv')
head(a)
a$X = NULL
a$age = NULL 
a$default = NULL
a$housing = NULL
a$contact = NULL
a$loan = NULL



mydata <- read.csv("bank-full.csv",sep = ';')

#Summary on dataset
summary(mydata)
#One hot encoding
for(level in unique(mydata$job)){
  mydata[paste("job", level, sep = "_")] <- ifelse(mydata$job == level, 1, 0)
}

for(level in unique(mydata$marital)){
  mydata[paste("marital", level, sep = "_")] <- ifelse(mydata$marital == level, 1, 0)
}

for(level in unique(mydata$education)){
  mydata[paste("education", level, sep = "_")] <- ifelse(mydata$education == level, 1, 0)
}

mydata$default_yes <- ifelse(mydata$default == "yes", 1, 0)

mydata$housing_yes <- ifelse(mydata$housing == "yes", 1, 0)

mydata$loan_yes <- ifelse(mydata$loan == "yes", 1, 0)

for(level in unique(mydata$contact)){
  mydata[paste("contact", level, sep = "_")] <- ifelse(mydata$contact == level, 1, 0)
}

for(level in unique(mydata$month)){
  mydata[paste("month", level, sep = "_")] <- ifelse(mydata$month == level, 1, 0)
}

for(level in unique(mydata$poutcome)){
  mydata[paste("poutcome", level, sep = "_")] <- ifelse(mydata$poutcome == level, 1, 0)
}

mydata$Class <- ifelse(mydata$y == "yes", "Yes", "No")

head(mydata)


mydata$job <- NULL
mydata$marital <- NULL
mydata$education <- NULL
mydata$default <- NULL
mydata$housing <- NULL
mydata$loan <- NULL
mydata$contact <- NULL
mydata$month <- NULL
mydata$poutcome <- NULL
mydata$y = NULL




head(mydata)
mydata$Class <- as.factor((mydata$Class))
colnames(mydata)[11] <- "job_blue_collar"
colnames(mydata)[14] <- "job_admin"
colnames(mydata)[16] <- "job_self_employeed"

head(mydata)

#Model2 with one hot encoding only.

s = sample.split(frame$y,SplitRatio = 0.75)
train = subset(mydata,subset = s == T)
test =  subset(mydata,subset = s == F)
rf = randomForest(Class~.,data = train)
summary(rf)
rf

adu = predict(rf,test)
adu = as.data.frame(adu)
table('predicted' = adu$adu,'actual' = test$Class)

head(mydata)

pred = predict(rf,test)

table(pred,test$Class)


#Model3 with feature engineering and one hot encoding
mohan = read.csv('bank_full_iteration_3.csv',sep = ',')

head(mohan)
mohan$X = NULL
head(mohan)
mohan$age = NULL
mohan$balance = NULL
head(mohan)

for(level in unique(mohan$job)){
  mohan[paste("job", level, sep = "_")] <- ifelse(mohan$job == level, 1, 0)
}

for(level in unique(mohan$marital)){
  mohan[paste("marital", level, sep = "_")] <- ifelse(mohan$marital == level, 1, 0)
}

for(level in unique(mohan$education)){
  mohan[paste("education", level, sep = "_")] <- ifelse(mohan$education == level, 1, 0)
}

mohan$default_yes <- ifelse(mohan$default == "yes", 1, 0)

mohan$housing_yes <- ifelse(mohan$housing == "yes", 1, 0)

mohan$loan_yes <- ifelse(mohan$loan == "yes", 1, 0)

for(level in unique(mohan$contact)){
  mohan[paste("contact", level, sep = "_")] <- ifelse(mohan$contact == level, 1, 0)
}

for(level in unique(mohan$month)){
  mohan[paste("month", level, sep = "_")] <- ifelse(mohan$month == level, 1, 0)
}



head(mohan)
colnames(mohan)[14] <- "job_blue_collar"
colnames(mohan)[17] <- "job_admin"
colnames(mohan)[19] <- "job_self_employeed"

mohan$job = NULL
mohan$marital = NULL
mohan$education = NULL
mohan$default = NULL
mohan$housing = NULL
mohan$contact = NULL
mohan$loan = NULL

head(mohan)

set.seed(1234)
s = sample.split(mohan$y,SplitRatio = 0.75)
train1 = subset(mohan,subset = s == T)
test1 =  subset(mohan,subset = s == F)
rf = randomForest(y~.,data = train1)
summary(rf)
rf

pred = predict(rf,test1)

pred = as.data.frame(pred)
pred

table('predicted'= pred$pred,'actual'=test1$y)

m1 = glm(formula = y ~ ., family = binomial(link = "logit"), 
    data = train1)
m1
summary(m1)
head(mohan)

d1 = mohan[,]

d1$job_student = NULL
d1$marital_single = NULL
d1$marital_divorced = NULL
d1$education_primary = NULL
d1$education_unknown = NULL
d1$default_yes = NULL
d1$contact_cellular = NULL
d1$contact_telephone = NULL
d1$contact_unknown = NULL
d1$balance_sign = NULL
d1$age_bin_pd_cut = NULL
d1

head(d1)

s1 = sample.split(d1$y,SplitRatio = 0.75)
train1 = subset(d1,subset = s1 == T)
test1 =  subset(d1,subset = s1 == F)
rf = randomForest(y~.,data = train1)
summary(rf)
rf

p1 = predict(rf,test1)
p1 = as.data.frame(p1)

table('predicted' = p1$p1,'actual' = test1$y)

head(d1)

#Random forest 4

rf_model<-randomForest(y ~.,data = train1, importance=TRUE, ntree=1000)
varImpPlot(rf)


p1 = predict(rf,test1)
error <- mean(p1 != test1$y)   
print(paste('Accuracy',1-error))

#test_result = p1
#new_test = test1


install.packages("ROCR")
library(ROCR)
pr <- prediction(as.numeric(p1), as.numeric(test1$y))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc





