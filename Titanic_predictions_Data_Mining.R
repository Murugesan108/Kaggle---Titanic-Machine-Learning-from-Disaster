######## TITANIC PREDICTION - KAGGLE ###########
rm(list = ls())

#Calling the required libraries for the operations
library(dplyr)
library(glmnet) # For cross validation and regularization (ridge and lasso) using glm models

## Setting the wroking directors
path = "C:\\Users\\arvra\\Documents\\UVa files\\Classes\\Fall_18\\Data Mining\\Kaggle competitions\\Titanic\\"
setwd(path)


#Reading the train and test data
train_data <- read.csv(dir()[grep("train",dir())])
test_data <- read.csv(dir()[grep("test",dir())])

############################# FEATURE ENGINEERING  ########################################################3

#### Function to extract the first and last name with the intials
initial_fn <- function(train_data){
  
  train_data$Last_name <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[1]]))
  
  name_2 <- as.character(lapply(strsplit(as.character(train_data$Name), ", "),function(x)x[[2]]))
  
  # strsplit for '.' (dot) can be used in two ways. 1) [.] 2) //.
  train_data$Initial <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[1]]))
  
  train_data$First_name <- as.character(lapply(strsplit(name_2, "[.] "), function(x)x[[2]]))
  return(train_data)
}

## Function to pre-preprocess - add Age and features like name title and others
data_pre_processing <- function(data_input)
{
  
  train_data_1 <- data_input
  
  #Creating features for the names
  train_data_1 <- initial_fn(train_data_1)
  
  
  # Creating extra features based on the title (initials)
  # Changing the initial values to 4 consistent values 
  Mr.val <- c("Capt", "Col", "Don", "Jonkheer","Major","Rev", "Sir")
  Mrs.val <- c("Lady", "Ms", "the Countess", "Mme", "Mlle","Dona")
  
  train_data_1$title_form <- ""
  train_data_1$title_form <- ifelse(train_data_1$Initial %in% Mr.val, "Mr", 
                                    ifelse(train_data_1$Initial %in% Mrs.val, "Mrs",
                                           ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "male", "Mr",
                                                  ifelse(train_data_1$Initial %in% "Dr" &  train_data_1$Sex == "female", "Mrs",train_data_1$Initial)))) 
  

  # Imputing the Fare column based on the analysis
  # The value has been manually written as 13 since it is the average Fare for 3rd class passengers which is the only missing value
  
  if(nrow(train_data_1[is.na(train_data_1$Fare),])>0)
  {
    train_data_1[is.na(train_data_1$Fare),]$Fare <- 13
  }

  
  ####### Creating a sub-model - to predict the missing values of age instead of imputing the age values with the mean value
  age.fit <- glm(Age ~ poly(SibSp,2)+poly(Parch,2)+poly(Fare,2)+title_form+poly(Pclass,2)+Sex,
                 data= train_data_1[!is.na(train_data_1$Age),])
  
  ###Imputing the age column with the model created for age
  train_data_1[is.na(train_data_1$Age),]$Age <- round(predict(age.fit, train_data_1[is.na(train_data_1$Age),]))
  
  
  #Setting the minimum age as 1
  train_data_1$Age <- ifelse(train_data_1$Age < 0 , 1 , train_data_1$Age)	
  
  # CREATING ADDITIONAL FEATURES ########
  
  #Feature to check if the individual is Single or Married
  train_data_1$single_or_not <- as.factor(ifelse(train_data_1$SibSp == 0 & train_data_1$Parch == 0, 1, 0) )
  
  #Feature to check if the individual is a Single Woman
  train_data_1$single_lady <- as.factor( ifelse(as.numeric(train_data_1$single_or_not)  & train_data_1$Sex == "female" , 1, 0) )
  
  #Feature to check if the family size is greater than 3
  train_data_1$family_big <- as.factor( ifelse(train_data_1$SibSp + train_data_1$Parch + 1 > 3, 1, 0) )
  
  #Feature to get the size of the family
  train_data_1$family_size <- as.numeric( train_data_1$SibSp + train_data_1$Parch + 1  )
  
  #Feature to check if the individual is a mother
  train_data_1$Mother <- as.factor(ifelse(train_data_1$Age > 18 & train_data_1$Parch == 1 & train_data_1$title == "Mrs", 1, 0))
  
  #Feature to check if the individual is a minor
  train_data_1$Minor <- as.factor(ifelse(train_data_1$Age <= 18 & train_data_1$Parch == 1, 1, 0))
  
  #Removing the Name variable
  train_data_1$Name <- NULL
  
  #Creating squres of the following variables
  train_data_1$Age_2 <-  (train_data_1$Age)^2 
  train_data_1$SibSp_2 <-  (train_data_1$SibSp)^2
  train_data_1$Parch_2 <-  (train_data_1$Parch)^2
  train_data_1$Fare_2 <-  (train_data_1$Fare)^2 
  
  
  #Removing unnecessary variabes
  train_data_1$Name <-  NULL 
  train_data_1$Ticket <-  NULL 
  train_data_1$Last_name <-  NULL 
  train_data_1$Cabin <-  NULL 
  train_data_1$Initial <-  NULL 
  train_data_1$First_name <-  NULL 
  
  
  train_data_1$Pclass <- as.factor( train_data_1$Pclass )
  
  return(train_data_1)
  
}


##Calling the pre-precessing function to impute Age and add various new features
train_data_1 <- data_pre_processing(train_data)
train_data_1$Survived <- as.factor( train_data_1$Survived ) #Converting Survived to factor data type

##Calling the pre-precessing function to impute Age and add various new features for TEST dataset
test_data_1 <- data_pre_processing(test_data)

test.fit.glm <- glm(Survived ~ . -PassengerId, data = train_data_1, family = binomial)

###Function to calculate the accuracy
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
#[1] 83.95062

test_data_1$Survived <- 0
test_data_1$EmbarkedC <- ifelse(test_data_1$Embarked == "C",1,0)


train_data_2 <- train_data_1
test_data_2 <- test_data_1

##Creating one-hot vectors for the embarked locations S,Q and C
train_data_2$S <- as.factor(ifelse(train_data_2$Embarked == "S",1,0))
train_data_2$Q <- as.factor(ifelse(train_data_2$Embarked == "Q",1,0))
train_data_2$C <- as.factor(ifelse(train_data_2$Embarked == "C",1,0))


test_data_2$S <- as.factor(ifelse(test_data_2$Embarked == "S",1,0))
test_data_2$Q <- as.factor(ifelse(test_data_2$Embarked == "Q",1,0))
test_data_2$C <- as.factor(ifelse(test_data_2$Embarked == "C",1,0))


#### Function to normalize the values
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

### Calling the scaling function for training and test dataset
train_data_2 <- scale_fn(train_data_2,c("Age","Age_2","Fare","Fare_2"))
test_data_2 <- scale_fn(test_data_2,c("Age","Age_2","Fare","Fare_2"))

train_data_2$Embarked <- NULL
test_data_2$Embarked <- NULL
test_data_2$EmbarkedC <- NULL


combined_data_2 <- rbind(train_data_2,test_data_2)
combined_data_matrix =  model.matrix(Survived ~ . - PassengerId, combined_data_2 ) [,-1]


#Creating the matrix for train and testdataset 
x =  combined_data_matrix[1:nrow(train_data_2),]
x1 = combined_data_matrix[(nrow(train_data_2)+1):nrow(combined_data_matrix),]
y = as.numeric(train_data_1$Survived)

### Running glm with cross validation
cv.ridge.fit2 = cv.glmnet(x,y, alpha = 1)

# Here you can use the CV set for prediction as it gives the best model with best value of 'lambda'
true_value <- table(round(predict(cv.ridge.fit2, x)) == y)[["TRUE"]]

acc =  100*(true_value / length(y))

print(acc)


y <- test_data$PassengerId
head(test_data)

submission <- data.frame(PassengerId = test_data$PassengerId,Survived = round(predict(cv.ridge.fit2, x1, type = "response")))

submission$X1 <- ifelse(submission$X1 == 2,1,0)
names(submission)[names(submission)=="X1"] <- "Survived"

write.csv(submission, "Titanic_sub_Data_Mining.csv",row.names = FALSE)

#########################################################################################