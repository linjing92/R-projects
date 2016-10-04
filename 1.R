censusData <- read.csv("~/Downloads/censusData.csv", na.strings="?")
library(ggplot2)

#Q1.
#Refer pdf

#Q2.a
sapply(censusData, function(x) sum(is.na(x))*100/32561)

#Q2.b
test<-apply(censusData, 1, function(x) sum(is.na(x)))
p<-as.data.frame(test)
ggplot(data=p,aes(x=test))+geom_histogram(binwidth=1)

#Q3.a
ggplot(data=censusData, aes(x=age)) + geom_histogram(binwidth=4)
ggplot(data=censusData, aes(x=hrs_per_week)) + geom_histogram(binwidth=4)

#Q3.b
ggplot(data=censusData, aes(x=age)) + geom_histogram(binwidth=5) + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=hrs_per_week)) + geom_histogram(binwidth=5) + facet_grid(income ~ .)

#Q3.c
ggplot(data=censusData, aes(x=income,y=age,fill=income)) + geom_boxplot() + guides(fill=FALSE)
ggplot(data=censusData, aes(x=income,y=hrs_per_week,fill=income), fill=income) + geom_boxplot() + guides(fill=FALSE)

#Q4.a
ggplot(data=censusData, aes(x=factor(work))) + geom_bar()
ggplot(data=censusData, aes(x=factor(edu))) + geom_bar()
ggplot(data=censusData, aes(x=factor(marital))) + geom_bar()
ggplot(data=censusData, aes(x=factor(occupation))) + geom_bar()
ggplot(data=censusData, aes(x=factor(race))) + geom_bar()
ggplot(data=censusData, aes(x=factor(sex))) + geom_bar()
ggplot(data=censusData, aes(x=factor(income))) + geom_bar()

#calculate unique values, ignore NA as unique value, if present
loop_v<- c(2,3,4,5,6,7,9)
j=1
label_attr <- c("work","edu","marital", "occupation","race","sex", "income")
s = c()
for(i in loop_v){
  if(any(is.na(unique(censusData[,i])))){
    s[j]<-length(unique(censusData[,i])) - 1
  } else {
    s[j]<-length(unique(censusData[,i]))
  }
  j=j+1
}
for(i in 1:7){
  cat("the number of unique values in",label_attr[i], ' :', s[i],'\n')
}

#Q4.b
ggplot(data=censusData, aes(x=factor(work))) + geom_bar(binwidth=1) + facet_grid(income ~ .) 
ggplot(data=censusData, aes(x=factor(edu))) + geom_bar() + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=factor(marital))) + geom_bar() + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=factor(occupation))) + geom_bar() + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=factor(race))) + geom_bar() + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=factor(sex))) + geom_bar() + facet_grid(income ~ .)
ggplot(data=censusData, aes(x=factor(income))) + geom_bar() + facet_grid(income ~ .)

#Q5.a - two solutions to handle overplotting
library(hexbin)
ggplot(data=censusData, aes(x=age, y=hrs_per_week)) +geom_point(shape=1)