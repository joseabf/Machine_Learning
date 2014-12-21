# Machine Learning Course - Week 3 - Project
========================================================

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to 
collect a large amount of data about personal activity relatively inexpensively. 
These type of devices are part of the quantified self movement – a group of enthusiasts 
who take measurements about themselves regularly to improve their health, to find patterns 
in their behavior, or because they are tech geeks. One thing that people regularly 
do is quantify how much of a particular activity they do, but they rarely quantify 
how well they do it. In this project, your goal will be to use data from accelerometers 
on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform 
barbell lifts correctly and incorrectly in 5 different ways. More information is 
available from the website here: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset). 


## Data 

The training data for this project are available here: 

[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)

The test data are available here: 

[https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har). If you use the document you create for this 
class for any purpose please cite them as they have been very generous in allowing 
their data to be used for this kind of assignment. 

## Load Data and Clean them

First load the data from both the training and the testing file.

```r
data<-read.csv("pml-training.csv",stringsAsFactors=FALSE)
testing<-read.csv("pml-testing.csv",stringsAsFactors=FALSE)
```

How many columns are filled mostly NAs or white space, we will delete leaving only the numeric values, then we add the classe and name.

We have not filled in the NA's with average or similar values, because most of the lines were 
NA's and any estimate would be very unreliable.


```r
control=function(x){sum(is.na(x))<5000}
training<-data
data2<-testing
uno<-sapply(training,control)
training<-training[,uno]
testing<-testing[,uno]
# eliminate columns character
control2=function(x){!is.character(x)}
dos<-sapply(training,control2)
training<-training[,dos]
testing<-testing[,dos]
```
We normalize the values, there are several ways but the best I've considered it subtracting 
the minimum value and dividing by the difference in minimum and maximum (the example 
of the notes subtracting the mean and dividing by sd generated worse results). 
It is difficult to test because the analyzes are very slow and the computer becomes eternal.


```r
#normalize data
normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
#normalize<-function(x){return((x-mean(x)/sd(x))}
training<-as.data.frame(lapply(training[2:56],normalize))
testing<-as.data.frame(lapply(testing[2:56],normalize))
# add columns with classe and name
training$user_name<-data$user_name
training$classe<-data$classe
testing$user_name<-data2$user_name
```

Finally, I have added classe and user_name.

## First Model "rpart"

We first tested a decision tree, as we shall see, the results are bad. In the tree graph 
we can see that ignores the option D.

And if we look at the precision and accuracy kappa see just above 50% and 
the kappa barely 0.4.

The only advantage is that the computer does not become eternal, it is 
a quick process.


```r
# try model rpart
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
m<-train(as.factor(classe)~.,training,method="rpart")
```

```
## Loading required package: rpart
```

```r
#ptraining<-preProcess(data2,method=c("center","scale"))
plot(m$finalModel,uniform=T,main="Classification Tree")
text(m$finalModel,use.n=T,all=TRUE,cex=.8)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
m
```

```
## CART 
## 
## 19622 samples
##    56 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## 
## Summary of sample sizes: 19622, 19622, 19622, 19622, 19622, 19622, ... 
## 
## Resampling results across tuning parameters:
## 
##   cp       Accuracy  Kappa    Accuracy SD  Kappa SD
##   0.03892  0.5548    0.43232  0.03293      0.04645 
##   0.05999  0.4220    0.21910  0.06441      0.10767 
##   0.11515  0.3197    0.05457  0.04239      0.06286 
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was cp = 0.03892.
```

```r
predict(m,newdata=testing)
```

```
##  [1] E B C E A C C A A E C C C A C A E A E B
## Levels: A B C D E
```


## Second Model "rf"

As a second model will use random forest, use the caret as the first model package.

Here obtain greater precision, above 98-99%. A much better results. In return, 
the process is much slower, it is difficult to test and play with different 
parameters because it takes more than half an hour.

Anyway with the model presented obtain an acceptable accuracy.


```r
#random forest method
ctrl <- trainControl(method = "cv", number = 3, allowParallel = TRUE)
m2 <- train(as.factor(classe)~., data = training, method="rf", trControl = ctrl, prox=FALSE)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
m2
```

```
## Random Forest 
## 
## 19622 samples
##    56 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (3 fold) 
## 
## Summary of sample sizes: 13082, 13081, 13081 
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa   Accuracy SD  Kappa SD 
##    2    0.9965    0.9956  0.0009303    0.0011769
##   31    0.9993    0.9991  0.0005789    0.0007323
##   60    0.9982    0.9977  0.0007644    0.0009668
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 31.
```

```r
predict(m2,newdata=testing)
```

```
##  [1] E A B A A E D B B E E E A A E D E E E A
## Levels: A B C D E
```

