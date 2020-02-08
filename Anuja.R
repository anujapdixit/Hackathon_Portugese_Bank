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
colnames(mohan)[15] <- "job_blue_collar"
colnames(mohan)[18] <- "job_admin"
colnames(mohan)[20] <- "job_self_employeed"

mohan$job = NULL
mohan$marital = NULL
mohan$education = NULL
mohan$default = NULL
mohan$housing = NULL
mohan$contact = NULL
mohan$loan = NULL



head(mohan)

mohan$job_student = NULL
mohan$marital_single = NULL
mohan$marital_divorced = NULL
mohan$education_primary = NULL
mohan$education_unknown = NULL
mohan$default_yes = NULL
mohan$contact_cellular = NULL
mohan$contact_telephone = NULL
mohan$contact_unknown = NULL
mohan$balance_sign = NULL

head(mohan)
set.seed(1234)
s = sample.split(mohan$y,SplitRatio = 0.75)
train1 = subset(mohan,subset = s == T)
test1 =  subset(mohan,subset = s == F)


rf = randomForest(y~.,data = train1,importance = TRUE,ntree = 1000)
varImpPlot(rf)


p1 = predict(rf,test1)
error <- mean(p1 != test1$y)   
print(paste('Accuracy',1-error))

#test_result = p1
#new_test = test1
library(ROCR)
pr <- prediction(as.numeric(p1), as.numeric(test1$y))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc






