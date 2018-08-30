# Credit Card data
rm(list = ls())
library(ggplot2)
library(ISLR)
library(leaps) #for finding the best fit with different set of features
library(dplyr)


path = "C:\\Users\\arvra\\Documents\\UVa files\\Classes\\Fall_18\\Data Mining\\Kaggle competitions\\Titanic\\"
setwd(path)


#Reading the train and test data
train_data <- read.csv(dir()[grep("train",dir())])
test_data <- read.csv(dir()[grep("test",dir())])

summary(train_data)


initial_fn <- function(train_data){
  
  train_data$Last_name <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[1]]))
  
  name_2 <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[2]]))
  
  # strsplit for '.' (dot) can be used in two ways. 1) [.] 2) //.
  train_data$Initial <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[1]]))
  
  train_data$First_name <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[2]]))
  return(train_data)
  library(dplyr)
  train_data1 %>% group_by(Initial) %>% summarize(min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE), mean_age = mean(Age, na.rm = TRUE)
                                                  ,no_of_values = length(Age)	)
}


data_pre_processing <- function(data_input)
{
  train_data_1 <- data_input
  train_data_1 <- initial_fn(train_data_1)
  
  # Analysing the data set
  
  train_data_1 %>% group_by(Initial, Sex) %>% summarize(med_age = median(Age, na.rm = TRUE),mean_age = mean(Age, na.rm = TRUE), t_val = length(Age)) 
  # Creating extra features based on df.d vector
  
  
  
  # Changing the initial values to 4 consistent values 
  Mr.val <- c("Capt", "Col", "Don", "Jonkheer","Major","Rev", "Sir")
  Mrs.val <- c("Lady", "Ms", "the Countess", "Mme", "Mlle","Dona")
  
  train_data_1$title_form <- ""
  train_data_1$title_form <- ifelse(train_data_1$Initial %in% Mr.val, "Mr", 
                                    ifelse(train_data_1$Initial %in% Mrs.val, "Mrs",
                                           ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "male", "Mr",
                                                  ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "female", "Mrs",train_data_1$Initial)))) 
  
  # Create a model for finding the unknown age values
  
  if(nrow(train_data_1[is.na(train_data_1$Fare),])>0)
  {
    train_data_1[is.na(train_data_1$Fare),]$Fare <- 13
  }
  
  
  age.fit <- glm(Age ~ poly(SibSp,2)+poly(Parch,2)+poly(Fare,2)+title_form+poly(Pclass,2)+Sex, data= train_data_1[!is.na(train_data_1$Age),])
  
  
  
  library(boot)
  age.cv.fit <- cv.glm(train_data_1[!is.na(train_data_1$Age),], age.fit, K = 20)	
  
  
  
  x =  model.matrix(Age ~ poly(SibSp,2)+poly(Parch,2)+poly(Fare,2)+title_form+poly(Pclass,2)+Sex, train_data_1[!is.na(train_data_1$Age),]) [,-1]
  y = train_data_1[!is.na(train_data_1$Age),]$Age
  
  library(glmnet)
  ridge.fit =  glmnet(x, y, alpha = 0)
  
  cv.ridge.fit = cv.glmnet(x,y, alpha = 0)
  
  df.ridge.fit = data.frame(cbind(predict(cv.ridge.fit, x),y,predict(cv.ridge.fit, x)-y))
  df.age.fit = data.frame(cbind(predict(age.fit), y,predict(age.fit)-y))
  
  # The sum of third column for ridge is zero. Somehow it seems that age.fit to be a better model
  
  train_data_1[is.na(train_data_1$Age),]$Age <- round(predict(age.fit, train_data_1[is.na(train_data_1$Age),]))
  
  train_data_1$Age <- ifelse(train_data_1$Age < 0 , 0 , train_data_1$Age)	
  
  train_data_1 %>% group_by(title_form, Sex) %>% summarize(med_age = median(Age, na.rm = TRUE),mean_age = mean(Age, na.rm = TRUE),
                                                           min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE),t_val = length(Age))
  
  # A small thing in the data
  # There is a data entry with sex as male and age as 11. Focus on that later
  
  
  # Single or not
  
  train_data_1$single_or_not <- as.factor(ifelse(train_data_1$SibSp == 0 & train_data_1$Parch == 0, 1, 0) )
  train_data_1$single_lady <- as.factor( ifelse(as.numeric(train_data_1$single_or_not)  & train_data_1$Sex == "female" , 1, 0) )
  
  train_data_1$family_big <- as.factor( ifelse(train_data_1$SibSp + train_data_1$Parch + 1 > 3, 1, 0) )
  
  
  train_data_1$family_size <- as.numeric( train_data_1$SibSp + train_data_1$Parch + 1  )
  
  train_data_1$family_ID <- as.factor(paste0(train_data_1$Last_name,train_data_1$family_size))
  
  
  train_data_1$Mother <- as.factor(ifelse(train_data_1$Age > 18 & train_data_1$Parch == 1 & train_data_1$title == "Mrs", 1, 0))
  train_data_1$Minor <- as.factor(ifelse(train_data_1$Age <= 18 & train_data_1$Parch == 1, 1, 0))
  
  train_data_1$Name <- NULL
  
  
  
  train_data_1$Age_2 <-  (train_data_1$Age)^2 
  train_data_1$SibSp_2 <-  (train_data_1$SibSp)^2
  train_data_1$Parch_2 <-  (train_data_1$Parch)^2
  train_data_1$Fare_2 <-  (train_data_1$Fare)^2 
  
  
  train_data_1$Name <-  NULL 
  train_data_1$Ticket <-  NULL 
  train_data_1$Last_name <-  NULL 
  train_data_1$Cabin <-  NULL 
  train_data_1$Initial <-  NULL 
  
  
  train_data_1$First_name <-  NULL 
  
  head(train_data_1)
  
  
  
  train_data_1$Pclass <- as.factor( train_data_1$Pclass )
  
  return(train_data_1)
  
}

train_data_1 <- data_pre_processing(train_data)
train_data_1$Survived <- as.factor( train_data_1$Survived )

test_data_1 <- data_pre_processing(test_data)


test.fit.glm <- glm(Survived ~ . -PassengerId, data = train_data_1[-15], family = binomial)
summary(test.fit.glm)

library(boot)
#Cv.glm is not used for fitting a model. It is just testing the error of the data set over cross validation error
#You cannot make predictions over it

test.fit.cv  <- cv.glm(data = train_data_1[-15], test.fit.glm, K = 20)


summary(test.fit.glm)
predict(test.fit.glm, test_data_1, type = "response")


acc_fn <- function(train_data1,fit){
  
  pred = round(predict(fit, train_data1, type = "response"),0)
  
  t_val = train_data1$Survived
  true_val = as.numeric(summary(t_val == pred)[["TRUE"]])
  
  #print(true_val)
  #print(length(t_val))
  
  acc = 100*(true_val / length(t_val))
  return(acc)
  
  
  
}

acc_fn(train_data_1,test.fit.glm)



# Major inference : When you are training the dataset, make sure that you use only non NA values in it
# You can either remove the NA values. That has a disadvantage. If you do that then will be missing out on training records
# Else try to fill up that NA by some way or the other

# Compare test.fit2 and test.fit.3 There are difference in the summary of them which shows different features to be important
# But the accuracy by both of them is the same


# Now using the feature selection method

library(glmnet)

test_data_1$Survived <- 0
test_data_1$EmbarkedC <- ifelse(test_data_1$Embarked == "C",1,0)


train_data_2 <- train_data_1
test_data_2 <- test_data_1

train_data_2$S <- as.factor(ifelse(train_data_2$Embarked == "S",1,0))
train_data_2$Q <- as.factor(ifelse(train_data_2$Embarked == "Q",1,0))
train_data_2$C <- as.factor(ifelse(train_data_2$Embarked == "C",1,0))


test_data_2$S <- as.factor(ifelse(test_data_2$Embarked == "S",1,0))
test_data_2$Q <- as.factor(ifelse(test_data_2$Embarked == "Q",1,0))
test_data_2$C <- as.factor(ifelse(test_data_2$Embarked == "C",1,0))



scale_fn <- function(data_frame, columns)
{
  
  for (i in columns){
    print(i)
    a <- diff(range(data_frame[[i]]))
    b <- mean(data_frame[[i]])
    
    value <- (data_frame[[i]] - b )/a
    data_frame[[i]] <- value
    
  }
  
  return(data_frame)
}

train_data_2 <- scale_fn(train_data_2,c("Age","Age_2","Fare","Fare_2"))

test_data_2 <- scale_fn(test_data_2,c("Age","Age_2","Fare","Fare_2"))

train_data_2$Embarked <- NULL
test_data_2$Embarked <- NULL
test_data_2$EmbarkedC <- NULL


combined_data_2 <- rbind(train_data_2,test_data_2)
combined_data_matrix =  model.matrix(Survived ~ . - PassengerId, combined_data_2 ) [,-1]


x =  combined_data_matrix[1:nrow(train_data_2),]
x1 = combined_data_matrix[(nrow(train_data_2)+1):nrow(combined_data_matrix),]

y = as.numeric(train_data_1$Survived)

ridge.fit =  glmnet(x, y, alpha = 1)

#plot(ridge.fit)
#plot(ridge.fit, xvar = "lambda", lable = TRUE)

cv.ridge.fit2 = cv.glmnet(x,y, alpha = 1)

#plot(cv.ridge.fit2)
coef(cv.ridge.fit2)

# Here you can use the CV set for prediction as it gives the best model with best value of 'lambda'

true_value <- table(round(predict(cv.ridge.fit2, x)) == y)[["TRUE"]]

acc =  100*(true_value / length(y))

print(acc)



table(round(predict(cv.ridge.fit2, x1, type = "response")))



accuracy <- numeric(0)

for (i in 1:75){
  
  t_val <- table(round(predict(ridge.fit, x)[,i]) == y)[["TRUE"]]
  
  accuracy[i]<- 100 * (t_val/ length(y))
  
  
  
}

plot(accuracy)





y <- test_data$Pass
head(test_data)

submission <- data.frame(PassengerId = test_data$PassengerId,Survived = round(predict(cv.ridge.fit2, x1, type = "response")))

submission$X1 <- ifelse(submission$X1 == 2,1,0)
names(submission)[names(submission)=="X1"] <- "Survived"

write.csv(submission, "Titanic_sub_29Aug.csv",row.names = FALSE)




############## Trying random forest 



library(randomForest)
library(MASS)

set.seed(108)
train_no = 600
train = sample(1:nrow(train_data_1),train_no)

train_data_2$Survived <- as.factor(train_data_2$Survived)
train_data_2$title_form <- as.factor(train_data_2$title_form)

test_data_2$Survived <- NULL
test_data_2$title_form <- as.factor(test_data_2$title_form)

#Got error when the column "title_form" was in character. got rectified as soon as it was changed to factor

rf.titanic = randomForest(Survived ~. - PassengerId, data = train_data_2, subset = train, ntree = 500,importance = T, mtry = 2)

print("Accuracy: ")
print(table(predict(rf.titanic) == train_data_2$Survived[train])[["TRUE"]]/train_no)


table(predict(rf.titanic) == train_data_2$Survived[train])

oob.err = double(13)
test.err = double(13)



for (mtry in 1:13){
  
  fit = randomForest(Survived ~. - PassengerId, data = train_data_2, ntree = 500,importance = T)
  
  pred = predict(fit, train_data_2[-train,])
  
  test.err[mtry] = table(train_data_2$Survived[-train]==pred)[["TRUE"]]/length(train_data_2$Survived[-train])
  
  cat(mtry, " ")
  
} 

matplot(1:mtry, cbind(test.err), pch = 19, col = c("red", "blue"), type = "b", ylab = "Accuracy")

#print(table(predict(rf.titanic, test_data_2) == train_data_2$Survived[train])[["TRUE"]]/train_no)


table(predict(rf.titanic, test_data_2))

submission <- data.frame(PassengerId = test_data$PassengerId,Survived = predict(rf.titanic, test_data_2))


names(submission)[names(submission)=="X1"] <- "Survived"
submission$Survived <- ifelse(submission$Survived == 2,1,0)

write.csv(submission, "Titanic_sub10.csv",row.names = FALSE)





### Implement cforest function in r, which is present in "party" library:

library(party)



set.seed(101)
train_no = 600
train = sample(1:nrow(train_data_1),train_no)

train_data_2$Survived <- as.factor(train_data_2$Survived)
train_data_2$title_form <- as.factor(train_data_2$title_form)

test_data_2$Survived <- NULL
test_data_2$title_form <- as.factor(test_data_2$title_form)

#Got error when the column "title_form" was in character. got rectified as soon as it was changed to factor
# cforest is computationally little expensive



cf.titanic = cforest(Survived ~. - PassengerId, 
                     data = train_data_2,
                     subset = train,
                     controls = cforest_unbiased(mtry = 4,ntree = 2000))



tabl <- table(train_data_2$Survived[-train],predict(cf.titanic,train_data_2[-train,], OOB = TRUE, type = "response"))

print("Accuracy: ")
print(table(predict(cf.titanic,train_data_2[-train,], OOB = TRUE, type = "response") == train_data_2$Survived[-train])[["TRUE"]]/(891-train_no))
precision = tabl[1,1]/sum(tabl[1,]) 
recall = tabl[1,1]/sum(tabl[,1])

print(precision)
print(recall)


# Predicting test_data
table(predict(cf.titanic, test_data_2, OOB = TRUE, type = "response"))



test.err = double(13)

recall = double(13)
precision = double(13)

for (mtry in 1:10){
  
  fit = cforest(Survived ~. - PassengerId, 
                data = train_data_2,
                subset = train,
                controls = cforest_unbiased(mtry = mtry,ntree = 1000))
  
  pred = predict(fit, train_data_2[-train,],OOB = TRUE, type = "response")
  
  tabl <- table(train_data_2$Survived[-train],predict(fit,train_data_2[-train,], OOB = TRUE, type = "response"))
  
  print("Accuracy: ")
  #print(table(predict(cf.titanic,train_data_2[-train,], OOB = TRUE, type = "response") == train_data_2$Survived[-train])[["TRUE"]]/(891-train_no))
  precision[mtry] = tabl[1,1]/sum(tabl[1,]) 
  recall[mtry] = tabl[1,1]/sum(tabl[,1])
  
  
  test.err[mtry] = table(train_data_2$Survived[-train]==pred)[["TRUE"]]/length(train_data_2$Survived[-train])
  
  cat(mtry, " ")
  
} 



matplot(1:13, cbind(test.err,precision,recall), pch = 19, col = c("red", "blue"), type = "b", ylab = "Accuracy")

#print(table(predict(rf.titanic, test_data_2) == train_data_2$Survived[train])[["TRUE"]]/train_no)



# print( table(predict(cf.titanic, test_data_2, OOB = TRUE, type = "response")))





############ Some feature Engineering, Again !! ####################### 






y <- test_data$Pass
head(test_data)

submission <- data.frame(PassengerId = test_data$PassengerId,Survived = predict(cf.titanic, test_data_2, OOB = TRUE, type = "response"))

submission$X1 <- ifelse(submission$X1 == 2,1,0)
names(submission)[names(submission)=="X1"] <- "Survived"



write.csv(submission, "Titanic_sub16.csv",row.names = FALSE)















############################################### Other test codes ######################################################################

table(train_data$Survived, train_data$Sex)

#train_data <- na.omit(train_data)
## Going through the data

ggplot(train_data, aes(x=Age,y=PassengerId, color = as.factor(Survived))) +geom_point() + 
  facet_grid(Sex ~.) +
  ggtitle("Survival vs Passenger's Age")+
  xlab("Age") + 
  theme(legend.position = "none")+
  scale_colour_manual(values = c("#FF0000","#0000FF"))


ggplot(subset(train_data, Embarked != ""), aes(x = Embarked, y = PassengerId, color = as.factor(Survived))) +
  geom_tile() + 
  facet_grid(.~Sex) +
  ggtitle("Survival vs Passenger's Sex and Port of Embarkment")+	
  theme(legend.position = "none")+
  scale_colour_manual(values = c("#FF0000","#0000FF"))



ggplot(subset(train_data, Embarked != ""), aes(x = Embarked, y = PassengerId, color = as.factor(Survived))) +
  geom_tile() + 
  facet_grid(.~Pclass) +
  ggtitle("Survival vs Passenger's Class and Port of Embarkment")+	
  theme(legend.position = "none")+
  scale_colour_manual(values = c("#FF0000","#0000FF"))



ggplot(subset(train_data, Embarked != ""), aes(x = Embarked, y = PassengerId, color = as.factor(Survived))) +
  geom_tile() + 
  facet_grid(Sex~Pclass) +
  ggtitle("Survival vs Passenger's Class, Sex and Port of Embarkment")+	
  theme(legend.position = "none")+
  scale_colour_manual(values = c("#FF0000","#0000FF"))



######################################3
# Data manipulationsss

all <- train_data

as.data.frame(
  cbind("Title" = unique(all$Title), 
        "No_of_passengers" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x,])),
        "Age_missing" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x & is.na(all$Age),])),
        "Minimum_Age" = sapply(unique(all$Title), function(x) min(all[all$Title == x,'Age'], na.rm = TRUE)),
        "Maximum_Age" = sapply(unique(all$Title), function(x) max(all[all$Title == x,'Age'], na.rm = TRUE))), row.names = F)


#####################################################333


train_data$Name <- NULL
train_data$Ticket <- NULL
train_data$PassengerId <- NULL
train_data$Cabin <- NULL


set.seed(1)
cv_set <- sample(1:nrow(train_data), size = nrow(train_data)/4)

train.model <- glm(Survived~., family = "binomial", data= train_data[-cv_set,])

#regfit.full = regsubsets(Survived ~., train_data[-cv_set])
#reg.summary <- summary(regfit.full)


#plot(reg.summary$rsq, xlab = "No of predictors", type = "b")


# Accuracy of the model
n_value <- length(train_data[-cv_set,]$Survived)

true_value <- as.numeric(summary( round(predict(train.model, train_data[-cv_set,],type = "response"),0) == (train_data[-cv_set,]$Survived) )[["TRUE"]])
table_value <- table( round(predict(train.model, train_data[-cv_set,],type = "response"),0) ,(train_data[-cv_set,]$Survived) )

accuracy <- (true_value / n_value)*100
precision <- 
  recall <- 
  
  
  ############ Playing with the titanic data set ###################

head(train_data)

#Split the names

train_data$Last_name <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[1]]))

name_2 <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[2]]))

# strsplit for '.' (dot) can be used in two ways. 1) [.] 2) //.
train_data$Initial <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[1]]))

train_data$First_name <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[2]]))

library(dplyr)
train_data %>% group_by(Initial) %>% summarize(min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE), mean_age = mean(Age, na.rm = TRUE)
                                               ,no_of_values = length(Age)	)



o_mr <- order(train_data[train_data$Initial == "Mr",]$Age)
a_mr <- train_data[train_data$Initial == "Mr",]
head(a_mr[o_mr,])


o_ma <- order(train_data[train_data$Initial == "Master",]$Age)
a_ma <- train_data[train_data$Initial == "Master",]
tail(a_ma[o_ma,])

train_data1 <- na.omit(train_data) 
test.fit <- glm(Survived~poly(Age,5), data= train_data1, family = binomial)
summary(test.fit)



plot(train_data$Age, train_data$Survived, pch = "|")
lines(predict(test.fit, train_data, type = "response"))



################# Current state of the model 
# what to write the codes for
# Accuracy of the model
# Precision and Recall. Need to find trade off between them.

################ Improving the model
# Ridge regression/Lasso (You can try with PCA a little time later)
# Polynomial regression / Test the model again

## Cross validataion/ Validation with different seed values. Check the difference. Open the codes written after seeing the video!


#Feature Engineering -- Do that again !!



########################################################################################################################################
########################################################################################################################################


############ Playing with the titanic data set ###################

head(train_data)

#Split the names

initial_fn <- function(train_data){
  train_data$Last_name <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[1]]))
  
  name_2 <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[2]]))
  
  # strsplit for '.' (dot) can be used in two ways. 1) [.] 2) //.
  train_data$Initial <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[1]]))
  
  train_data$First_name <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[2]]))
  return(train_data)
  library(dplyr)
  train_data1 %>% group_by(Initial) %>% summarize(min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE), mean_age = mean(Age, na.rm = TRUE)
                                                  ,no_of_values = length(Age)	)
}


o_mr <- order(train_data[train_data$Initial == "Mr",]$Age)
a_mr <- train_data[train_data$Initial == "Mr",]
head(a_mr[o_mr,])


o_ma <- order(train_data[train_data$Initial == "Master",]$Age)
a_ma <- train_data[train_data$Initial == "Master",]
tail(a_ma[o_ma,])


# Analysing each predictor



acc_fn <- function(train_data1,fit){
  
  pred = round(predict(fit, train_data1, type = "response"),0)
  
  t_val = train_data1$Survived
  true_val = as.numeric(summary(t_val == pred)[["TRUE"]])
  
  #print(true_val)
  #print(length(t_val))
  
  acc = 100*(true_val / length(t_val))
  return(acc)
  
  
  
}

#Fitting different degree



train_data1 <- na.omit(train_data)
test.fit1 <- substitute(glm(Survived~poly(Age,4), data= train_data1, family = binomial))
plot(1:80,predict(test.fit1, data.frame(Age = c(1:80)), type = "response"))

a <- "Parch"
test.fit2 <- glm(Survived~poly(a,4), data= train_data1, family = binomial)
summary(test.fit2)
plot(1:6,predict(test.fit2, data.frame(Parch = c(1:6)), type = "response"), type = "b")
acc_fn(test.fit2)







degree_chk <- function(feature)
{
  acc <- numeric(0)
  max_val <- max(train_data1[[feature]], na.rm = TRUE)
  
  if(max_val > 6)
  {
    max_val <- 7
  }
  
  for( d_val in 1:(max_val-1)){
    
    formula.d <- as.formula(paste("Survived ~ poly(", feature,",",d_val,")"))
    
    fit.d <- glm(formula = formula.d, data= train_data1, family = binomial)
    
    
    acc[d_val] <- acc_fn(fit.d)	
  }
  plot(1:(max_val-1), acc, type = "b", xlab = "Degree", ylab = feature)
  return(acc)
}



## Check the degree suitable for each numeric column 

numeric_name <- names(train_data)[sapply(train_data, is.numeric)]
numeric_name <- numeric_name[numeric_name != "Survived"]

par(mfrow=c(round(length(numeric_name)/2),2))

accu <- list(0)
for (i in 1:length(numeric_name))
{
  accu[[i]] <- degree_chk(numeric_name[i])
  
  
}

#Choosing degree with threshold as 0.5 % accuracy 


threshold = 0.5
choose_best <- function(x){
  
  max_x <- max(x)
  diff_x <- max_x - x
  return(min(which(as.numeric(diff_x < threshold)==1)))
  
}

# Create data frame

df.d <- data.frame(numeric_name, degree = as.numeric(lapply(accu, choose_best)))



# Now create a model based on these

train_data1<- initial_fn(train_data1)

test.fit2 <- glm(Survived~poly(Age,df.d[match("Age",df.d$numeric_name),]$degree)+
                   poly(Pclass,df.d[match("Pclass",df.d$numeric_name),]$degree)+
                   poly(SibSp,df.d[match("SibSp",df.d$numeric_name),]$degree)+
                   poly(Parch,df.d[match("Parch",df.d$numeric_name),]$degree)+
                   poly(Fare,df.d[match("Fare",df.d$numeric_name),]$degree)+
                   Embarked+Sex+Initial,
                 data= train_data1, family = binomial)
summary(test.fit2)

acc_fn(train_data1,test.fit2)



# second submission got 78% in the leaderboard and ranks 2029



###################################### Trying another type of model - try 3 ########## Target 80%



#train_data_1 <- na.omit(train_data)
train_data_1 <- test_data
train_data_1 <- initial_fn(train_data_1)

# Analysing the data set

library(dplyr)

train_data_1 %>% group_by(Initial, Sex) %>% summarize(med_age = median(Age, na.rm = TRUE),mean_age = mean(Age, na.rm = TRUE), t_val = length(Age)) 
# Creating extra features based on df.d vector

# Changing the initial values to 4 consistent values 

Mr.val <- c("Capt", "Col", "Don", "Jonkheer","Major","Rev", "Sir")
Mrs.val <- c("Lady", "Ms", "the Countess", "Mme", "Mlle","Dona")

train_data_1$title_form <- ""
train_data_1$title_form <- ifelse(train_data_1$Initial %in% Mr.val, "Mr", 
                                  ifelse(train_data_1$Initial %in% Mrs.val, "Mrs",
                                         ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "male", "Mr",
                                                ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "female", "Mrs",train_data_1$Initial)))) 

# Create a model for finding the unknown age values

train_data_1[is.na(train_data_1$Fare),]$Fare <- 13



age.fit <- glm(Age ~ poly(SibSp,2)+poly(Parch,2)+poly(Fare,2)+title_form+poly(Pclass,2)+Sex, data= train_data_1[!is.na(train_data_1$Age),])



library(boot)
age.cv.fit <- cv.glm(train_data_1[!is.na(train_data_1$Age),], age.fit, K = 20)	



x =  model.matrix(Age ~ poly(SibSp,2)+poly(Parch,2)+poly(Fare,2)+title_form+poly(Pclass,2)+Sex, train_data_1[!is.na(train_data_1$Age),]) [,-1]
y = train_data_1[!is.na(train_data_1$Age),]$Age

library(glmnet)
ridge.fit =  glmnet(x, y, alpha = 0)

cv.ridge.fit = cv.glmnet(x,y, alpha = 0)

df.ridge.fit = data.frame(cbind(predict(cv.ridge.fit, x),y,predict(cv.ridge.fit, x)-y))
df.age.fit = data.frame(cbind(predict(age.fit), y,predict(age.fit)-y))

# The sum of third column for ridge is zero. Somehow it seems that age.fit to be a better model

train_data_1[is.na(train_data_1$Age),]$Age <- round(predict(age.fit, train_data_1[is.na(train_data_1$Age),]))

train_data_1$Age <- ifelse(train_data_1$Age < 0 , 0 , train_data_1$Age)	

train_data_1 %>% group_by(title_form, Sex) %>% summarize(med_age = median(Age, na.rm = TRUE),mean_age = mean(Age, na.rm = TRUE),
                                                         min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE),t_val = length(Age))

# A small thing in the data
# There is a data entry with sex as male and age as 11. Focus on that later


# Single or not

train_data_1$single_or_not <- ifelse(train_data_1$SibSp == 0 & train_data_1$Parch == 0, 1, 0) 
train_data_1$single_lady <- ifelse(train_data_1$single_or_not  & train_data_1$Sex == "female" , 1, 0) 

train_data_1$family_big <- ifelse(train_data_1$SibSp + train_data_1$Parch + 1 > 3, 1, 0) 


train_data_1$Mother <- ifelse(train_data_1$Age > 18 & train_data_1$Parch == 1 & train_data_1$title == "Mrs", 1, 0)
train_data_1$Minor <- ifelse(train_data_1$Age <= 18 & train_data_1$Parch == 1, 1, 0)

train_data_1$Name <- NULL



train_data_1$Age_2 <-  (train_data_1$Age)^2 
train_data_1$SibSp_2 <-  (train_data_1$SibSp)^2
train_data_1$Parch_2 <-  (train_data_1$Parch)^2
train_data_1$Fare_2 <-  (train_data_1$Fare)^2 


train_data_1$Name <-  NULL 
train_data_1$PassengerId <-  NULL 
train_data_1$Ticket <-  NULL 
train_data_1$Last_name <-  NULL 
train_data_1$Cabin <-  NULL 
train_data_1$Initial <-  NULL 


train_data_1$First_name <-  NULL 

head(train_data_1)

test.fit.glm <- glm(Survived ~ . , data = train_data_1, family = binomial)
test.fit.cv  <- cv.glm(train_data_1, test.fit.glm, K = 20)



summary(test.fit.3)

predict(test.fit.cv)
acc_fn(train_data_1,test.fit.cv)



# Major inference : When you are training the dataset, make sure that you use only non NA values in it

# Compare test.fit2 and test.fit.3 There are difference in the summary of them which shows different features to be important
# But the accuracy by both of them is the same


# Now using the feature selection method

library(glmnet)

x =  model.matrix(~ ., train_data_1 ) [,-1]

y = train_data_1$Survived

ridge.fit =  glmnet(x, y, alpha = 1)

plot(ridge.fit)
plot(ridge.fit, xvar = "lambda", lable = TRUE)

cv.ridge.fit2 = cv.glmnet(x,y, alpha = 1)

plot(cv.ridge.fit2)
coef(cv.ridge.fit2)

true_value <- table(round(predict(cv.ridge.fit, x)) == y)[["TRUE"]]

acc =  100*(true_value / length(y))

print(acc)


accuracy <- numeric(0)
for (i in 1:78){
  
  t_val <- table(round(predict(ridge.fit, x)[,i]) == y)[["TRUE"]]
  
  accuracy[i]<- 100 * (t_val/ length(y))
  
  
  
}

plot(accuracy)


y <- test_data$Pass
head(test_data)

submission <- data.frame(PassengerId = test_data$PassengerId,Survived = round(predict(cv.ridge.fit2, x, type = "response")))
write.csv(submission, "Titanic_sub3.csv")


# Work on the missing age part
# Work on building extra features
# Work on addressing the initial part


## Prepare test dataset 


test_data_1 <- initial_fn(test_data)
x =  model.matrix(Survived ~ ., train_data_1 ) [,-1]


test_data1<-initial_fn(test_data)

test_data1 %>% group_by(Initial) %>% summarize(min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE), mean_age = mean(Age, na.rm = TRUE)
                                               ,no_of_values = length(Age)	)

train_data1 %>% group_by(Initial) %>% summarize(min_age = min(Age, na.rm = TRUE), max_age = max(Age, na.rm = TRUE), mean_age = mean(Age, na.rm = TRUE)
                                                ,no_of_values = length(Age)	)


#Corrections in test data												
test_data1[test_data1$Initial == "Dona",]$Initial <- "Mrs"
test_data1[is.na(test_data1$Age),]$Age <- 50
test_data1[is.na(test_data1$Fare),]$Fare <- 32


round(predict(test.fit2, train_data1, type = "response"))

submission <- data.frame(PassengerId = test_data1$PassengerId,Survived =round(predict(test.fit2, test_data1, type = "response")))
write.csv(submission, "Titanic_sub2.csv")


########################################################################################################################################
########################################################################################################################################




# Try ridge and lasso 
# Try different Degree of data with cross validatiaon set 
# Find variance and bias
# code to use various predictors








#Try cross validataion 





#Test

comp_df <- data.frame(prediction = round(predict(train.model,type = "response"),0), data_val = train_data[-cv_set,]$Result)
table(comp_df)


### Try the other logistic method suggested in the book

# Calculate the error. See the curves for number of predictors to be followed.

# See if there is a high variance or high bias

