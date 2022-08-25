#packages
if(!(require(car))) install.packages("car")
if(!(require(ROCR))) install.packages("ROCR")
if(!(require(randomForest))) install.packages("randomForest") 
if(!(require(dplyr))) install.packages("dplyr") 
if(!(require(corrplot))) install.packages("corrplot") 
if(!(require(ggplot2))) install.packages("ggplot2") 
if(!(require(neuralnet))) install.packages("neuralnet")
if(!(require(Hmisc))) install.packages("Hmisc") 
if(!(require(plotrix))) install.packages("plotrix") 
library(neuralnet)

#set working directory for local
setwd("~/BA II/data")
source("http://bigblue.depaul.edu/jlee141/econdata/R/func_lib.R")
#read file for local
data <- read.csv("Breast_Cancer_dataset.csv",header = TRUE)

#check dimension of the data
dim(data)

#structure of the data
str(data)

#column names of data
colnames(data)

#head of data
head(data)

#tail of data
tail(data)

#check for NULL values
sum(is.na(data)) #no NULL values

#check for duplicate records if any
sum(duplicated(data)) #no duplicate records

#summary of data
summary(select_if(data,is.numeric))

#check of no of patients
table(data$Classification)

#convert classification data to 0,1 
data$Classification <- data$Classification-1
table(data$Classification)

#check outliers
par(mfrow = c(2,5))
name = colnames(data)
invisible(lapply(1:ncol(data), function(i) boxplot(data[, i],xlab=name[i])))
par(mfrow = c(1,1))
#Although we had outliers for some variables but those values were significant and hence same are not removed.

#correlation
cor_matrix <- round(cor(select_if(data,is.numeric)),digits = 2)
View(cor_matrix)

#plot correlation matrix
corrplot(cor(select_if(data,is.numeric)),method = "number",type = "upper",tl.cex = 0.6,number.cex = 0.5)

#test for correlation
res <- rcorr(as.matrix(select_if(data,is.numeric))) # rcorr() accepts matrices only
pvalue_cor <- round(res$P, 3) #Only correlations with p-values smaller than the significance level (usually α=0.05) should be interpreted.
View(pvalue_cor)

#check for multicolinearlity 
vif1 <-lm(Classification~. , data = data)
vif(vif1)
#We see multicolinearity b/w Insulin(12.451423) and HOMA(18.203099)

#distribution of patients
mat_class <- data %>% 
  group_by(Classification) %>% 
  summarise(Total = n())
View(mat_class)

mat_class$class_name <- if_else(mat_class$Classification==1,"Patient","Healthy controls")
mat_class$class_name <- as.factor(mat_class$class_name)

dev.off()
#barchart
ggplot(mat_class, aes(class_name, Total)) + 
  geom_bar(stat="identity",fill = c("pink","grey")) +
  ggtitle("Frequency distribution of Individual") +
  xlab("Category") +
  ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#descriptive statistics based on classification
descp_class <- data %>% group_by(Classification) %>%
  summarise(
    Avg_age = mean(Age),
    Avg_BMI = mean(BMI),
    Avg_Glucose=mean(Glucose),
    Avg_Insulin=mean(Insulin),
    Avg_HOMA=mean(HOMA),
    Avg_Leptin=mean(Leptin),
    Avg_Adiponectin=mean(Adiponectin),
    Avg_Resistin=mean(Resistin),
    Avg_MCP.1=mean(MCP.1),
  )
View(descp_class)

#categorizing the age of people
#Age             Category
#Below 14        Child
#15-24           Youth
#25-64           Adult
#65 or higher    Senior
# source: https://www.statcan.gc.ca/en/concepts/definitions/age2
data$age_cat <- "Child"
data$age_cat[data$Age>=15 & data$Age<=24] <- "Youth"
data$age_cat[data$Age>=25 & data$Age<=64] <- "Adult"
data$age_cat[data$Age>=65] <- "Senior"

mat_cat <- data %>%
  group_by(Classification,age_cat) %>%
  summarise(Total = n())

mat_cat$class_name <- if_else(mat_cat$Classification==1,"Patient","Healthy controls")
mat_cat$class_name <- as.factor(mat_cat$class_name)

dev.off()
#barchart
ggplot(mat_cat, aes(age_cat, Total, fill = class_name)) + 
  geom_bar( stat = "identity",position = "dodge") +
  ggtitle("Frequency distribution based on Age Category") +
  scale_x_discrete(limits=c("Youth","Adult","Senior"))+
  scale_fill_manual(values = c("pink", "grey"))+
  xlab("Age Category") +
  ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5)) 

#BMI ranges for women - considering women is going through no body transitions
#BMI              Weight classification
#Below 18.5       Underweight
#18.5-24.9        Normal
#25.0-29.9        Overweight
#30.0 or higher   Obese
# source: https://www.google.com/amp/s/health.clevelandclinic.org/bmi-for-women/amp/
data$BMI_cat <- "underweight"
data$BMI_cat[data$BMI>=18.5 & data$BMI<=24.9] <- "normal"
data$BMI_cat[data$BMI>=25.0 & data$BMI<=29.9] <- "overweight"
data$BMI_cat[data$BMI>=30.0] <- "obese"

mat_cat1 <- data %>%
  group_by(Classification,BMI_cat) %>%
  summarise(Total = n())

mat_cat1$class_name <- if_else(mat_cat1$Classification==1,"Patient","Healthy controls")
mat_cat1$class_name <- as.factor(mat_cat1$class_name)

dev.off()
#barchart
ggplot(mat_cat1, aes(BMI_cat, Total, fill = class_name)) + 
  geom_bar( stat = "identity",position = "dodge") +
  ggtitle("Frequency distribution based on BMI Category") +
  scale_fill_manual(values = c("pink", "grey"))+
  scale_x_discrete(limits=c("underweight","normal","overweight","obese"))+
  xlab("BMI Category") +
  ylab("Frequency")+
  theme(plot.title = element_text(hjust = 0.5)) 

#categorizing based on classification, age and BMI
m1 <- data %>% 
  group_by(Classification,age_cat,BMI_cat) %>% 
  summarise(Total=n())
View(m1)

#logit model to check significant variables
data <- data[,1:10]
data$Classification <- as.factor(data$Classification)
log <- glm(formula = Classification~., data = data, family = binomial(link = logit))
summary(log)

data$Classification <- as.numeric(data$Classification)
data$Classification <- data$Classification-1
table(data$Classification)

# Normalize the data
indata <- data
colnames(indata)
zindata <- min_max_nor(indata)

# set seed and split the data to train and test
set.seed(1234)
train_idx <- sample(nrow(zindata),round(.7*nrow(zindata)))
train <- indata[train_idx,]                    
test <- indata[-train_idx,]                    
ztrain <- zindata[train_idx,]                    
ztest <- zindata[-train_idx,]
testy <- ztest$Classification


# Linear Probability Model
#Model 1
lm1 <- lm(Classification~BMI+Glucose +Resistin, data=train)
summary(lm1)
yhat1 <- predict(lm1,newdata = test)
conf_table(yhat1,testy,"LPM")
auc_plot(yhat1,testy,"LPM")

#Model 2
lm2 <- lm(Classification~., data=train)
summary(lm2)
yhat2 <- predict(lm2,newdata = test)
conf_table(yhat2,testy,"LPM")
auc_plot(yhat2,testy,"LPM")

#Model3
lm3 <- step(lm(Classification~., data=train),direction = "both")
summary(lm3)
yhat3 <- predict(lm3,newdata = test)
conf_table(yhat3,testy,"LPM")
auc_plot(yhat3,testy,"LPM")

#combine the linear probability model
par(mfrow=c(2,2))
auc_plot(yhat1,testy,"LPM1")
auc_plot(yhat2,testy,"LPM2")
auc_plot(yhat3,testy,"LPM3")
par(mfrow=c(1,1))

# Logistic Regression Model
#Model 1
logit1 <- glm(formula = Classification~BMI+Glucose +Resistin, data = train, family = binomial(link = logit))
summary(logit1)
loghat1 <- predict(logit1,newdata = test,type = "response")
conf_table(loghat1,testy,"LOGIT")
auc_plot(loghat1,testy,"LOGIT")

#Model 2
logit2 <- glm(formula = Classification~ ., data = train, family = binomial(link = logit))
summary(logit2)
loghat2 <- predict(logit2,newdata = test,type = "response")
conf_table(loghat2,testy,"LOGIT")
auc_plot(loghat2,testy,"LOGIT")

#Model 3
logit3 <- step(glm(formula = Classification~ ., data = train, 
                   family = binomial(link = logit)),direction = "both")
summary(logit3)
loghat3 <- predict(logit3,newdata = test,type = "response")
conf_table(loghat3,testy,"LOGIT")
auc_plot(loghat3,testy,"LOGIT")

#combine the Logistic model
par(mfrow=c(2,2))
auc_plot(loghat1,testy,"LOGIT1")
auc_plot(loghat2,testy,"LOGIT2")
auc_plot(loghat3,testy,"LOGIT3")
par(mfrow=c(1,1))

train$Classification <- as.factor(train$Classification)
test$Classification <-  as.factor(test$Classification)

# Random Forest Model
#Model 1
rf1 <- randomForest(formula= Classification~. , data=train,mtry=5,ntree=500)
summary(rf1)
rfhat1 <- predict(rf1, newdata = test, type = "prob")
rfhat1 <- rfhat1[,2]
conf_table(rfhat1, testy,"RANDFOREST")
auc_plot(rfhat1,testy,"RANDOMFOREST")

# Find the best mtry
oob.values <- vector(length=12)

for(i in 1:12) {
  temp.model <- randomForest(formula= Classification~. , data=train,mtry=i,ntree=500)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}
cbind(1:12,oob.values)

# I find mtry =11 works best

# Find the best ntree
rf_tree <- randomForest(formula= Classification~. , data=train,mtry=i,ntree=1000)
Trees <- rep(1:nrow(rf_tree$err.rate))
Error.rate <- rf_tree$err.rate[,"OOB"]
plot(Trees,Error.rate,col="red")
# ntree  100, 300 or 600

rf2 <- randomForest(formula= Classification~. , data=train,mtry=11,ntree=100)
summary(rf2)
rfhat2 <- predict(rf2, newdata = test, type = "prob")
rfhat2 <- rfhat2[,2]
conf_table(rfhat2, testy,"RANDFOREST")
auc_plot(rfhat2,testy,"RANDOMFOREST")

rf3 <- randomForest(formula= Classification~. , data=train,mtry=11,ntree=300)
summary(rf3)
rfhat3 <- predict(rf3, newdata = test, type = "prob")
rfhat3 <- rfhat3[,2]
conf_table(rfhat3, testy,"RANDFOREST")
auc_plot(rfhat3,testy,"RANDOMFOREST")

rf4 <- randomForest(formula= Classification~. , data=train,mtry=11,ntree=600)
summary(rf4)
rfhat4 <- predict(rf4, newdata = test, type = "prob")
rfhat4 <- rfhat4[,2]
conf_table(rfhat4, testy,"RANDFOREST")
auc_plot(rfhat4,testy,"RANDOMFOREST")

rf5 <- randomForest(formula= Classification~ Age+BMI + Glucose+Insulin +Resistin, data=train,mtry=7,ntree=100)
summary(rf5)
rfhat5 <- predict(rf5, newdata = test, type = "prob")
rfhat5 <- rfhat5[,2]
conf_table(rfhat5, testy,"RANDFOREST")
auc_plot(rfhat5,testy,"RANDOMFOREST")

#combine the Random Forest Model
par(mfrow=c(2,3))
auc_plot(rfhat1,testy,"RANDOMFOREST1,mtry=5,ntree=500")
auc_plot(rfhat2,testy,"RANDOMFOREST2,mtry=11,ntree=100")
auc_plot(rfhat3,testy,"RANDOMFOREST3,mtry=11,ntree=300")
auc_plot(rfhat4,testy,"RANDOMFOREST4,mtry=11,ntree=600")
auc_plot(rfhat5,testy,"RANDOMFOREST,mtry=7,ntree=100")

ztrain$Classification <- as.factor(ztrain$Classification)
nnet1 <- neuralnet(Classification~., data = ztrain, hidden=5, linear.output = FALSE)
plot(nnet1)
nnetp1 <- predict(nnet1, newdata = ztest)
nnetp1 <- nnetp1[,2]
auc_plot(nnetp1 , testy, "Neural Network Model")

nnet2 <- neuralnet(Classification~., data = ztrain, hidden=c(5,3), stepmax = 1e+06)
plot(nnet2)
nnetp2 <- predict(nnet2, newdata = ztest)
nnetp2 <- nnetp2[,2]
auc_plot(nnetp2 , testy, "Neural Network Model2")

nnet3 <- neuralnet(Classification~., data = ztrain, hidden=c(7,5,3), stepmax = 1e+06)
plot(nnet3)
nnetp3 <- predict(nnet3, newdata = ztest)
nnetp3 <- nnetp3[,2]
auc_plot(nnetp3 , testy, "Neural Network Model3")

nnet4 <- neuralnet(Classification~., data = ztrain, hidden=c(9,7,5,3), stepmax = 1e+06)
plot(nnet4)
nnetp4 <- predict(nnet4, newdata = ztest)
nnetp4 <- nnetp4[,2]
auc_plot(nnetp4 , testy, "Neural Network Model4")

#combine the neural network model
par(mfrow=c(2,2))
auc_plot(nnetp1,testy,"NEURALNETWORK1,hidden=5")
auc_plot(nnetp2,testy,"NEURALNETWORK2,hidden=c(5,3)")
auc_plot(nnetp3,testy,"NEURALNETWORK3,hidden=c(7,5,3)")
auc_plot(nnetp4,testy,"NEURALNETWORK4,hidden=c(9,7,5,3)")

# Combine Graphs
par(mfrow = c(2,2))
auc_plot(yhat3,testy,"LPM Mode3")
auc_plot(loghat2,testy,"LOGIT Model2")
auc_plot(rfhat2,testy,"Random Forest Model2")
auc_plot(nnetp4, testy, "Neural Network Model4")

par(mfrow=c(1,1))
conf_table(yhat3,testy,"LPM")
conf_table(loghat2,testy,"LOGIT")
conf_table(rfhat2,testy,"RandomForest")
conf_table(nnetp4,testy,"NeuralNetwork")
