

#SETTING THE DIRECTORY

setwd('Z:/DBS/SEM-1/Statistics B9DA101')
getwd()
Data <-  read.csv("insurance.csv", header=TRUE, sep=",")
str(Data)
head(Data)

#Taking variables from the dataset

x1=Data$bmi
x2=Data$charges
y=Data$sex

# Removing missing values so that we have clean data

Newdata <- na.omit(data.frame(x1,x2,y))

vect = 0
mc = 1000
for (i in 1:mc) {
  n=nrow(Newdata)
  split_Data = sample(n,n*(80/100))
  trainset = Newdata[split_Data,]
  testset = Newdata[-split_Data,]
  trainset.glm = glm(y ~ ., data = trainset, family = 'binomial')
  summary(trainset.glm)
  
  phat_i=predict(trainset.glm,testset, type="response")  # prediction
  phat_i
  L=length(phat_i)
  predictedvalues=rep(0,L)
  
  predictedvalues[phat_i>0.5]=1  
  predictedvalues
  confusion_matrix=table( predictedvalues, testset[,3])
  confusion_matrix
  accuracy = mean(predictedvalues == testset[,3]) # correctness of prediction
  length(testset[,3])
  vect = vect + (1/mc)*accuracy
  vect
}
vect

#Example 3
Yes_Opinion <- c(200,250)
No_Opinion <- c(150,300)
Cant_say_Opinion <- c(50,50)

result <- data.frame(Yes_Opinion,No_Opinion,Cant_say_Opinion,row.names = c("Male", "Female"))
result

chisq.test(result)
#Reject Null hypothesis

#Example 2