#Load dataset; decision tree
library(datasets)
data("iris")
summary(iris)
#Sepal.Length    Sepal.Width     Petal.Length    Petal.Width   
#Min.   :4.300   Min.   :2.000   Min.   :1.000   Min.   :0.100  
#1st Qu.:5.100   1st Qu.:2.800   1st Qu.:1.600   1st Qu.:0.300  
#Median :5.800   Median :3.000   Median :4.350   Median :1.300  
#Mean   :5.843   Mean   :3.057   Mean   :3.758   Mean   :1.199  
#3rd Qu.:6.400   3rd Qu.:3.300   3rd Qu.:5.100   3rd Qu.:1.800  
#Max.   :7.900   Max.   :4.400   Max.   :6.900   Max.   :2.500  
#Species  
#setosa    :50  
#versicolor:50  
#virginica :50  


#Split data to train and test
set.seed(123)
n<-nrow(iris)
train_ind<-sample(seq_len(n), size = 0.8*n)

train<-iris[train_ind,]
test<-iris[-train_ind,]

#Run the model
library(rpart)
DT <- rpart(Species ~ ., train)
summary(DT)
#Call:
#  rpart(formula = Species ~ ., data = train)
#n= 120 

#CP nsplit  rel error    xerror       xstd
#1 0.5333333      0 1.00000000 1.1333333 0.06638831
#2 0.4000000      1 0.46666667 0.4666667 0.06638831
#3 0.0100000      2 0.06666667 0.1600000 0.04381780

#Variable importance
#Petal.Length  Petal.Width Sepal.Length  Sepal.Width 
#35           31           22           12 

#Node number 1: 120 observations,    complexity param=0.5333333
#predicted class=virginica   expected loss=0.625  P(node) =1
#class counts:    40    35    45
#probabilities: 0.333 0.292 0.375 
#left son=2 (40 obs) right son=3 (80 obs)
#Primary splits:
#  Petal.Length < 2.45 to the left,  improve=40.20833, (0 missing)
#Petal.Width  < 0.8  to the left,  improve=40.20833, (0 missing)
#Sepal.Length < 5.45 to the left,  improve=28.48114, (0 missing)
#Sepal.Width  < 3.25 to the right, improve=14.41695, (0 missing)
#Surrogate splits:
#  Petal.Width  < 0.8  to the left,  agree=1.000, adj=1.000, (0 split)
#Sepal.Length < 5.45 to the left,  agree=0.925, adj=0.775, (0 split)
#Sepal.Width  < 3.25 to the right, agree=0.825, adj=0.475, (0 split)

#Node number 2: 40 observations
#predicted class=setosa      expected loss=0  P(node) =0.3333333
#class counts:    40     0     0
#probabilities: 1.000 0.000 0.000 

#Node number 3: 80 observations,    complexity param=0.4
#predicted class=virginica   expected loss=0.4375  P(node) =0.6666667
#class counts:     0    35    45
#probabilities: 0.000 0.438 0.562 
#left son=6 (32 obs) right son=7 (48 obs)
#Primary splits:
#  Petal.Length < 4.75 to the left,  improve=30.104170, (0 missing)
#Petal.Width  < 1.75 to the left,  improve=28.705830, (0 missing)
#Sepal.Length < 6.15 to the left,  improve= 8.642677, (0 missing)
#Sepal.Width  < 2.45 to the left,  improve= 3.402778, (0 missing)
#Surrogate splits:
#  Petal.Width  < 1.55 to the left,  agree=0.912, adj=0.781, (0 split)
#Sepal.Length < 5.75 to the left,  agree=0.775, adj=0.438, (0 split)
#Sepal.Width  < 2.45 to the left,  agree=0.675, adj=0.188, (0 split)

#Node number 6: 32 observations
#predicted class=versicolor  expected loss=0.03125  P(node) =0.2666667
#class counts:     0    31     1
#probabilities: 0.000 0.969 0.031 

#Node number 7: 48 observations
#predicted class=virginica   expected loss=0.08333333  P(node) =0.4
#class counts:     0     4    44
#probabilities: 0.000 0.083 0.917 

#Plot the result
library(rpart.plot)
rpart.plot(DT, type=1)

#Confusion Matrix
pred<-predict(DT, test, type="class")
tbl<-table(as.numeric(pred), test$Species)
tbl
#setosa versicolor virginica
#1     10          0         0
#2      0         13         0
#3      0          2         5

#Test the accuracy
sum(diag(tbl))/sum(tbl)*100
#[1] 93.33333