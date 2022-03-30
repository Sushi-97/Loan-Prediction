library(rpart) #Decison Tree 
library(e1071) #Decison Tree 

library(rpart.plot) #Decison Tree Visualization
library(rattle) #Decison Tree Visualization
library(RColorBrewer) #Decison Tree Visualization

library(caTools) #Splitting the dataset
library(caret)

loan <- read.csv("loan_dt.csv", stringsAsFactors = TRUE)
View(loan)
summary(loan)

loan<-loan[,-1]

set.seed(10)
split <- sample.split(loan$Loan_Status,SplitRatio=0.7)
loanTR <- subset(loan,split=="TRUE")
loanTS <- subset(loan,split=="FALSE")

loanDT <- rpart(Loan_Status~Gender+Dependents+Education+
                  Self_Employed+ApplicantIncome+LoanAmount+Loan_Amount_Term+
                  Credit_History,data=loanTR,method="class",parms=list(split="information"))

fancyRpartPlot(loanDT)

Pred <- predict(loanDT, newdata = loanTS, type="class")

confusionMatrix(data=Pred, reference=loanTS$Loan_Status, positive = "Y")

