d = read.csv('bank_full_iteration_9.csv')
d$X = NULL
head(d)

for(level in unique(d$job)){
  d[paste("job", level, sep = "_")] <- ifelse(d$job == level, 1, 0)
}

for(level in unique(d$marital)){
  d[paste("marital", level, sep = "_")] <- ifelse(d$marital == level, 1, 0)
}

for(level in unique(d$education)){
  d[paste("education", level, sep = "_")] <- ifelse(d$education == level, 1, 0)
}

d$housing_yes <- ifelse(d$housing == "yes", 1, 0)
for(level in unique(d$month)){
  d[paste("month", level, sep = "_")] <- ifelse(d$month == level, 1, 0)
}

head(d)
