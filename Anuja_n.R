
adwait = read.csv('bank-full.csv',sep = ';')
head(adwait)

for(level in unique(adwait$job)){
  adwait[paste("job", level, sep = "_")] <- ifelse(adwait$job == level, 1, 0)
}

for(level in unique(adwait$marital)){
  adwait[paste("marital", level, sep = "_")] <- ifelse(adwait$marital == level, 1, 0)
}

for(level in unique(adwait$education)){
  adwait[paste("education", level, sep = "_")] <- ifelse(adwait$education == level, 1, 0)
}


adwait$housing_yes <- ifelse(adwait$housing == "yes", 1, 0)
adwait$poutcome_success <- ifelse(adwait$poutcome == "success", 1, 0)
adwait$poutcome_failure <- ifelse(adwait$poutcome == "failure", 1, 0)
adwait$poutcome_unknown <- ifelse(adwait$poutcome == "unknown", 1, 0)

for(level in unique(adwait$month)){
  adwait[paste("month", level, sep = "_")] <- ifelse(adwait$month == level, 1, 0)
}

head(adwait)

colnames(adwait)[21] <- "job_blue_collar"
colnames(adwait)[24] <- "job_admin"
colnames(adwait)[26] <- "job_self_employeed"

adwait$education = NULL
adwait$job = NULL
adwait$marital = NULL
adwait$month = NULL
adwait$default = NULL
adwait$loan = NULL


head(adwait)
head(train1)


adwait$poutcome = NULL
adwait$poutcome_unknown = NULL

head(adwait)
set.seed(1234)
s = sample.split(adwait$y,SplitRatio = 0.85)
train1 = subset(adwait,subset = s == T)
test1 =  subset(adwait,subset = s == F)

rf = randomForest(y~.,data = train1)
varImpPlot(rf)
rf


p1 = predict(rf,test1)
error <- mean(p1 != test1$y)   
print(paste('Accuracy',1-error))

#test_result = p1
#new_test = test1
library(ROCR)
pr <- prediction(as.numeric(p1),as.numeric(test1$y))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

p1
table('predicted' = p1,'actual' = test1$y)

write.csv(adwait,'hack.csv')

