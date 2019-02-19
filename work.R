ls()
data()
?iris

a <-3*5
a
a1<-c(a,a*4,-7*a)
a1

A<-c(1,2,3,NA)
A

string <-c("3",2,0,NA)
           
class(string)

num <- c("a","b","c","d","e")
num

#assigning names
names(num)<-c("x1","x2","x3","x4","x5")
names(num)[4]
num["x4"]
num[c("x1","x2")]
num["x1","x2"]

length(num)

product=c("Bag","shoes","belt","belt1")
total_price=c(500,1000,150,10000)
color=c("blue","red","black","blue")
quantity=c(5,2,3,4)

product_details<-data.frame(product,total_price,color,quantity)
product_details

product_details[,"total_price"]
product_details[2,]
product_details[,2]

head(product_details,2)
tail(product_details,1)

set.seed(124)
schtyp <- sample(0:1, 20, replace = TRUE)
schtyp
is.factor(schtyp)
schtyp.f <- factor(schtyp, labels = c("private", "public"))
schtyp.f


gender<-c(1,2,1,2,1,2,1,2,1)
gender<-factor(gender, levels=c(0,1,2), labels=c("M","F","T"))
table(gender)

ses <- c("low", "middle", "low", "low", "low", "low", "middle", "low", "middle",
         "middle", "middle", "middle", "middle", "high", "high", "low", "middle",
         "middle", "low", "high")

ses.order <- ordered(ses, levels = c("low", "middle", "high"))
ses
is.factor(ses)
is.character(ses)
ses.order

my.data <-BJsales::R 

x<-c("yes","no","no","yes","no")
y<-as.factor(x)
class(y)
table(y)

##### importing tabular data ####


import1<-read.table("D:\\Jigsaw\\3.R\\customer.csv", sep=",", header=TRUE)
drop<-drop.

ls()
rm("imoprt1")
summary(import1)
str(import1)

data(BJsales)
view(BJsales)

data(iris)
view(iris)

ses.f <- factor(ses, levels = c("low", "middle", "high"))
ses.f[21] <- "very.high"
## Warning: invalid factor level, NA generated
ses.f
ses.f<-factor(ses.f,levels = c(levels(ses.f),"very high"))
ses.f
ses.f[21]<-"very high"

getOption("repos")
?iris
data(CO2)
view(CO2)
view(iris)
install.packages("openintro")

install.packages("tsibble")
library(jsonlite)

options(
  "repos",
  "download.file.method",
  "download.file.extra"
)

install.packages("jsonlite")
install.packages("jsonlite", repos="https://cran.rstudio.com/")
install.packages("devtools", repos="https://cran.rstudio.com/")
options(repos="https://CRAN.R-project.org")

web2<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", header=FALSE)



install.packages("ggplot2",repos="http://cran.rstudio.com/", type="source")
install.packages("http://cran.r-project.org/bin/windows/contib/3.5/ggplot2_3.0.0.zip", repos=NULL, type="source")


library(ggplot2)

###########15Sep2018####################

setwd("D:\\Jigsaw\\3.R")
getwd()
oj<-read.csv("oj (1).csv")
oj
view(oj)
str(oj)

#dataframes[rows.columns]

oj[3,3]

oj[c(1,2,8,456),c(1,3,6)]


oj[c(1:5),"brand"]

#logical subsetting

#selecting only those rows where brand bought is tropicana
dat<-oj[oj$brand== 'tropicana',]

dat1<-oj[oj$brand=='tropicana'|oj$brand=='dominicks',]

head(dat1)

dat2<-oj[oj$brand=='tropicana' & oj$feat==0,]
head(dat2)

#subsetting using which() operator

ind<-which(oj$brand=="dominicks")
ind
class(ind)
head(ind)
dat3<-oj[ind,]
head(dat3)

#selecting columns
dat4<-oj[,c("week","brand")]
head(dat4)

#selceting+subsetin

dat5<-oj[oj$brand=='tropicana' & oj$feat==0,c("week","store")]
view(dat5)

dat7<-oj[oj$brand=='tropicana' | oj$feat==0 | oj$price==3.29,c("week","store")]

#Adding new columns
oj$logInc<-log(oj$INCOME)

dim(oj)
view(oj)

oj1<-oj[,-18]


#Revenue Column

head(oj$logmove)
head(exp(oj$logmove))
oj$revenue<-exp(oj$logmove)*oj$price


oj$revenue
View(oj)


#sorting data

numbers<-c(10,100,5,8)
order(numbers)
order(-numbers)

dat8<-oj[order(oj$week),]
head(dat8)
min(oj$week)

dat9<-oj[order(-oj$week),]
head(dat9)
max(oj$week)


#Group by 





#syntax aggregate (variables to be summarize) by=list(variable by which grouping is to be done), function


#select brand, average(price) from oj group by 1,2 - SQL 

aggregate(oj$price, by=list(oj$brand),mean)
class(aggregate(oj$price,by=list(oj$brand,oj$feat),mean))

tapply(oj$price,oj$brand,sd)
class(tapply(oj$price,oj$brand,mean))

#Mean Income of people bby brand
#summarize - Income
#Summarize by Brand
#summarize how mean
aggregate(oj$INCOME,by=list(oj$brand),mean)
class(aggregate(oj$INCOME,by=list(oj$brand),mean))

tapply(oj$INCOME[oj$INCOME<=10.5&oj$brand!='dominicks'],
       oj$brand[oj$INCOME<=10.5&oj$brand!='dominicks'],mean)

class(tapply(oj$INCOME,oj$brand,mean))

sapply(oj$INCOME[oj$INCOME<=10.5&oj$brand!='dominicks'],mean)
dat13<-c(sapply(oj$INCOME[oj$INCOME<=10.5&oj$brand!='dominicks'],mean))
data(dat13)
lapply(oj$INCOME[oj$INCOME<=10.5&oj$brand!='dominicks'],mean)

dat10<-oj[oj$INCOME<=10.5&oj$brand!='dominicks',c("INCOME","brand")]

dat11<-oj[oj$week==160&oj$price<=9.5,c("brand","week","price")]

install.packages("dplyr")
library(dplyr)
dat8=filter()
dat8=filter(oj,brand=='tropicana')
dim(filter(oj,brand=="tropicana"))
dat9=filter(oj,brand=='tropicana'|brand=='dominicks')
dim(filter(oj,brand=='tropicana'|brand=='dominicks'))

#selecting columns
dat10<-select(oj,brand,INCOME,feat)
dat10


dat11<-select(oj,-brand,-INCOME,-feat)


#creating a new column
dat12<-mutate(oj,logIncome=log(INCOME),sqrtInc=sqrt(INCOME))
view(dat12)

#Arranging data
dat13<-arrange(oj,INCOME)
dat13
dat13<-arrange(oj,-INCOME) 
          

dat14<-arrange(oj,desc(INCOME))

#Group wise summaries
gr_brand<-group_by(oj,brand)

summarize(gr_brand,mean(INCOME),sd(INCOME))

class(gr_brand)
group<-as.data.frame(gr_brand)
class(group)

print(group)



mean(oj[oj$INCOME>=10.5,"price"])
#dplyr code
summarize(filter(oj,INCOME>=10.5),mean(price))
summarize(filter(oj,INCOME>=10.5,mean(price)))

oj%>%filter(price>=2.5)%>%mutate(logIncome=log(INCOME))%>%summarize(mean(logIncome),median(logIncome),sd(logIncome))


library(lubridate)

#date
fd<-read.csv("Fd (1).csv")
str(fd)
dim(fd)
class(fd)

library(lubridate)
fd$FlightDate<-dmy(fd$FlightDate)

head(months(fd$FlightDate))
unique(months(fd$FlightDate))
head(weekdays(fd$FlightDate))
unique(weekdays(fd$FlightDate))


#Finding time interval
fd$FlightDate[60]-fd$FlightDate[900]
difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "weeks")
difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "days")
difftime(fd$FlightDate[3000],fd$FlightDate[90],units = "hours")

#setting data based on time information
library(dplyr)

#subsetting the data for day=sunday

dim(fd)
fd_s<-fd%>%filter(weekdays(FlightDate)=="Sunday")
dim(fd_s)


#Find the number of flights on sundays for destination Atlanta
fd%>%filter(weekdays(FlightDate)=="Sunday",DestCityName=="Atlanta, GA")%>%nrow()

fd%>%filter(weekdays(FlightDate)=="Sunday")%>%group_by(DestCityName)%>%summarize(n())


fd%>%filter(weekdays(FlightDate)=="Sunday")%>%group_by(DestCityName)%>%summarize(n())
#merging data
fd1<-as.data.frame(fd)
unique(fd$DestCityName)


df1=data.frame(CustomerID=c(1:6),product=c(rep("Toaster",3),rep("Radio",3)))
    
df2=data.frame(CustomerID=c(2,4,6),State=c(rep("Alabama",2),rep("Ohio",1)))


merge (x=df1,y=df2,by="CustomerID",all=TRUE) #Outer Join
merge (x=df1,y=df2,by="CustomerID",all.x=TRUE) #Left Join
merge (x=df1,y=df2,by="CustomerID",all.y=TRUE) #Right Join

merge(x=df1,y=df2,by="CustomerID")

#Different var name
authors <- data.frame(
  surname = c("Tukey", "Venables", "Tierney", "Ripley", "McNeil"),
  nationality = c("US", "Australia", "US", "UK", "Australia"),
  retired = c("yes", rep("no", 4)))
books <- data.frame(
  name = c("Tukey", "Venables", "Tierney", "Ripley", "Ripley", "McNeil"),
  title = c("Exploratory Data Analysis",
            "Modern Applied Statistics ...",
            "LISP-STAT",
            "Spatial Statistics", "Stochastic Simulation",
            "Interactive Data Analysis"),
  other.author = c(NA, "Ripley", NA, NA, NA, NA))

authors
books
merge(x=authors,y=books,by.x = "surname",by.y = "name",all=TRUE)
merge(x=authors,y=books,by.x = "surname",by.y = "name",all.x=TRUE)
merge(x=authors,y=books,by.x = "surname",by.y = "name",all.y=TRUE)

full_join(x=df1,y=df2,by=NULL,all=TRUE)
inner_join(x=df1,y=df2,by=NULL,all.x=TRUE)
right_join(x=df1,y=df2,by="CustomerID",all.y=TRUE)
left_join(x=df1,y=df2,by="CustomerID",all.x=TRUE)


#Missing values
a<-c(1,2,3,4,5,6,NA,NA,NA,7,8,9)
is.na(a)
sum(is.na(a))
mean(a,na.rm=TRUE)

a1<-c(1,2,3,4,5,6,NULL,NULL,NA,7,8,9)
is.na(a1)
is.null(a1)
a1

air<-airquality
head(air)

sum(is.na(air$Ozone))

sum(is.na(air$Solar.R))

summary(air)

#Imputing Missing Values

air$Ozone[is.na(air$Ozone)]<-45
air$Solar.R[is.na(air$Solar.R)]<-mean(air$Solar.R,na.rm=TRUE)

summary(air)

#string manipulation

a="Batman"

substr(a,start=2,stop=6)

nchar(a)

tolower(a)

toupper(a)

b<-"Bat-Man"

strsplit(b,split="-")

c<-"Bat/Man"

strsplit(c,split="/")

paste(b,split=c)

grep("-",b)

grepl("/",c)

sub("-","/",b)


gsub("-","/",b)

strings3<-read.csv("Strings.csv")

str(strings)
mean(strings$Income_M)

#need to clean

strings$Income_M<-gsub("Rs","",strings$Income_M)
head(strings)

strings$Income_M<-gsub("/-","",strings$Income_M)
mean(strings$Income_M)

strings$Income_M<-as.numeric(strings$Income_M)
mean(strings$Income_M)


strings3$Income_M<-gsub("Rs","",strings3$Income_M)
strings3


#sometimes  u might need to use regexes to work character data u 

#select distinct DestCityName, count(*) from fd where FlightDate = "Sunday"
#select * from fd

library(sqldf)
fd1<-sqldf("select distinct DestCityName,  count(DestCityName) from fd where Weekday(FlightDate) = 6")
fd1<-sqldf("select weekday(FlightDate) from fd")


library(arules)
install.packages("arules")

data("AdultUCI")

names(AdultUCI)


AdultUCI%>%select(capital-gain)%>%dim()#Why Errors

AdultUCI%>%select(`capital-gain`)%>%dim()


#Window functions in dplyr()

?min_rank

mtcars
data("mtcars")
view(mtcars)
mtc<-mtcars

##############9/29/2018########
data()
data("iris")
view(iris)
ir<-iris
str(ir)


range(ir$Petal.Length)
range(ir$Petal.Width)


#Syntax
#plot(x=variable to be displayed on x axis, y = )

plot(x=ir$Petal.Width,y=ir$Petal.Length)


#Adding xlabels

plot(x=ir$Petal.Width,y=ir$Petal.Length,
     main=c("Petal Width Vs Petal Length"),
     xlab=c("Petal Width"),ylab=c("Petal lenth"),
    col=ir$Species,pch=as.numeric(ir$Species))
unique(ir$species)
legend(0.2,7,c("Setosa","Versicolor","Verginica"))

#Box plots

boxplot(ir$Petal.Length)

summary(ir$Petal.Length)
sd(ir$Petal.Length)

boxplot(ir$Sepal.Width)
summary(ir$Sepal.Width)
summary(ir)


#improving the asethetics
boxplot(ir$Petal.Length,col="red",main="Distribution of petal length")

hist(ir$Sepal.Width,col="orange")

hist(ir$Sepal.Width,col="orange",labels=TRUE)

hist(ir$Sepal.Width,col="orange",freq=FALSE)

hist(ir$Sepal.Width, col="orange",labels=T, freq= F)
lines(density(ir$Sepal.Width))


##
library(ggplot2)
p<-ggplot(ir,aes(x=ir$Petal.Length))
p+geom_density(aes(fill=ir$Species, colour=ir$Species),aplha=0.4)

p<-ggplot(ir,aes(x=ir$Sepal.Width))
p+geom_density(aes(fill=ir$Species, colour=ir$Species),aplha=0.1)
ggplot


url <- "http://rstatistics.net/wp-content/uploads/2015/09/ozone.csv"
ozone <- read.csv(url)

#### 13/10-- Logistic Regression
install.packages("gains")
library(gains)
library(dplyr)
install.packages("irr")
library(irr)
install.packages("caret")
library(caret)
install.package ("e1071")
library(e1071)


setwd("D:\\Jigsaw\\1.Data")
getwd()
dm<-read.csv("DirectMarketing.csv")

summary(dm$AmountSpent)

dm%>%mutate(target=ifelse(AmountSpent>mean(AmountSpent),1,0))->dm
dm$target
names(dm)
dm<-dm[,-10] #removing amount spent


summary(dm)

table(dm$target)


# Minimal Data prep

dim(dm)

dm$History1<-ifelse(is.na(dm$History),"Missing",as.character(dm$History))
dm$History1<-as.factor(dm$History1)

summary(dm$History1)
class(dm$Catalogs)

dm$Children<-as.factor(dm$Children)
class(dm$Children)

dm$Catalogs<-as.factor(dm$Catalogs)

head(dm)
dm<-dm[,-8] #Removing history
head(dm)


#spliting into test and training samples

set.seed(200) #to select same row and code those row as 200
index<-sample(nrow(dm),0.70*nrow(dm),replace = F)
train<-dm[index,]
test<-dm[-index,]

nrow(dm)

head(train)
head(test)


#Building the first model using all variables

names(train)
mod<-glm(train$target~.,data = train[,-9],family="binomial")
summary(mod)
summary(train$age)
dim(train)
?glm
table(dm$Age)
names(dm)
table(dm$Children)

#Creating Dummies

train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)

train$Hist.Mid_d<-ifelse(train$History1=="Medium",1,0)

train$Children2_d<-ifelse(train$Children=="2",1,0)

train$Children3_d<-ifelse(train$Children=="3",1,0)

test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)

test$Hist.Mid_d<-ifelse(test$History1=="Medium",1,0)

test$Children2_d<-ifelse(test$Children=="2",1,0)

test$Children3_d<-ifelse(test$Children=="3",1,0)

mod2<-glm(target~AgeYoung_d+Location+Salary+Children3_d+Children2_d+Catalogs+Hist.Mid_d,data=train,family="binomial")
names(train)

summary(mod2)

pred<-predict(mod2,type="response",newdata=test)
#default is log(odds). "Response" gives predicted prob for nrows(test)

head(pred)
nrow(test)
nrow(train)
table(test)
summary(test)

table(tittrain$Survived)/nrow(tittrain)
pred_binary1<-ifelse(pred>=0.3838,1,0)




confusionMatrix(pred_binary1,test$Survived,positive = "1")
class(pred_binary)
class(test$Survived)
length(pred_binary)
length(test$target)
table(pred_binary)
table(test$target)
as.factor(pred_binary)->pred_binary1
as.factor(test$Survived)->test$Survived

test$target<-as.numeric(test$target)
gains(test$target, predict(mod2,type="response",newdata = test),groups=10)

Concordance(test$target, predictedScores)
?Concordance
install.packages("concordance")
library(concordance)


#titanic data set

tittrain<-read.csv("tittrain.csv")
tittest<-read.csv("tittest.csv")
dim(tittrain)
dim(tittest)
summary(tittrain)
summary(tittest)
table(tittest)


colSums(is.na(tittrain)) # sum of all missing valuesmm
colSums(tittrain=='') # sum blank values

colSums(is.na(tittest)) # sum of all missing valuesmm
colSums(tittest=='') # sum blank values

summary(tittrain$age)
summary(tittest$age)
table(tittrain$Age)
table(tittest$Age)

## Missing values imputation
tittrain$Embarked[tittrain$Embarked==""] <- "S"
tittrain$Age[is.na(tittrain$Age)] <- median(tittrain$Age,na.rm=T)
tittest$Age[is.na(tittest$Age)]<- median(tittest$Age,na.rm=T)

## Removing Cabin as it has very high missing values, passengerId, Ticket and Name are not required
library(dplyr)
tittest <- tittest %>% select(-c(Cabin, PassengerId, Ticket, Name, Embarked))

## Check number of uniques values for each of the column to find out columns which we can convert to factors
sapply(tittrain, function(x) length(unique(x)))

for (i in c("Survived","Pclass","Sex","Embarked"))
  {
  tittrain[,i]=as.factor(tittrain[,i])
}

## Create dummy variables for categorical variables
library(dummies)
install.packages("dummies")
tittrain <- dummy.data.frame(tittrain, names=c("Pclass","Sex","Embarked"), sep="_")


## Splitting training and test data
train <- tittrain[1:667,]
test <- tittrain[668:891,]
dim(tittrain)
## Model Creation
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

## Model Summary
summary(model)

model2<-glm(Survived~Pclass_1+Pclass_2+Sex_female+Age+SibSp,data=test,family="binomial")
model3<-glm(Survived~Pclass+Sex+Age+SibSp,data=tittest,family="binomial")
summary(model2)
pred<-predict(model2,type="response",newdata=tittest)

head(pred)
nrow(test)
nrow(train)
table(test)
summary(test)
?predictionFunction
summary(model1)
confusionMatrix(pred_binary,test$target,positive = "1")

test$Survived<-as.numeric(test$Survived)
gains(test$Survived, predict(model2,type="response",newdata = test),groups=10)
##############################################################
# Inject outliers into data.
cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers.

# Plot of data with outliers.
Par(mfrow=c(1,2))
plot(Cars)


#############27/10/2018###########Time Series###########
search()
data('AirPassengers')
AP<-AirPassengers
class(AP)
AP
start(AP)
end(AP)
frequency(AP)
?ts
print(ts(100:200,frequency = 10,start = c(12,10)), calendar = TRUE)
print(ts(1:10,frequency = 8,start = c(1,4)),calendar = TRUE)

AP<-window(AP,start = c(1953,1),end = c(1960,12))
AP
summary(AP)
frequency(AP)
cycle(AP)
plot(AP)


aggregate(AP)


unemployment <- read.csv("http://rci.rutgers.edu/~rwomack/UNRATE.csv", row.names =1)
unemployment
?read.csv
class(unemployment)


Urate <-ts(unemployment$VALUE,start = c(1948,1),frequency = 12)
Urate
plot(Urate)


Inflation <- read.csv("http://rci.rutgers.edu/~rwomack/UNRATE.csv", row.names =1)
Inflation


presidents
window(presidents,1960,c(1969,4))

pres <- window(presidents,1945,c(1949,4))
pres
window(pres,1945.0,1945.25)<-c(60,70)
pres
decurate <- decompose(Urate)
names(decurate)
decurate
plot(decurate)
acf(Urate)
acf(AP)

###Simple Forecasting methods###
library(fpp)
install.packages(fpp)
library("ggplot2")
data(ausbeer,package= "fpp")
library(ggplot2)
beer
beer2<-window(ausbeer,start=1992,end=c(2007,4))
beer2

beerfit1<-meanf(beer2,h=11)
beerfit2<-naive(beer2,h=11)#naive
beerfit3<-snaive(beer2,h=11)

plot(beerfit1)
plot(beerfit2)
plot(beerfit3)


data(cafe)
cafe
class(cafe)
es<-ses(cafe,h=10)
plot(es)
summary(es)


checkresiduals(es)

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip = 1)
setwd("D:\\Jigsaw\\3.R")
getwd()
rain<-read.csv("precip1.csv")
rain

rainseries<-ts(rain,start = c(1813))
plot.ts(rainseries)
rainseries_forecasts<-HoltWinters(rainseries,gamma=F)
rainseries_forecasts
plot(rainseries_forecasts)
rainseries_forecasts2<-forecast(rainseries_forecasts,h=8)
plot(rainseries_forecasts2)
rainseries_forecasts2
checkresiduals(rainseries_forecasts2)
Box.test(rainseries_forecasts2$residuals,lag=20,type='Ljung-Box')

rainseries_forecasts2<-HoltWinters(rainseries,gamma=TRUE)


souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
setwd("D:\\Jigsaw\\3.R")
getwd()
souvenir<-read.csv("fancy.csv")
souvenir

logsouvenir<-log(souvenir)
souvenirseries<-ts(souvenir,start = c(1866),frequency = 12)
plot.ts(souvenirseries)

#The series is multiplicative in nature
#Convert to additive series and fit the additive holtwinter model

logsouvenirseries<-ts(logsouvenir, start=c(1866),frequency = 12)
logsouvenirseries
plot.ts(logsouvenirseries)
souvenirforecasts<-HoltWinters(logsouvenirseries)
plot(souvenirforecasts)
souvenirforecasts2<-forecast(souvenirforecasts,h=48)
plot(souvenirforecasts2)
Box.test(souvenirforecasts2$residuals,lag=20,type="Ljung-Box")
checkresiduals(souvenirforecasts2)


ts.plot(cafe)
fit<-ets(cafe)
fcast<-forecast(fit,h=12)
fit
?ets
fcast
accuracy(fcast)

#####ARIMA###############

data("WWWusage")
intusage<-WWWusage
class(intusage)
intusage
plot.ts(intusage)

plot(intusage)
intusageauto<-auto.arima(intusage)
summary(intusageauto)
intusageautoforecasts<-forecast(intusageauto,h=10)
plot(intusageautoforecasts)
checkresiduals(intusageautoforecasts)

#making the timeseries stationary

intusagediff1<-diff(intusage,differences = 1)
intusagediff1
plot.ts(intusagediff1)
intusagediff2<-diff(intusage,differences = 2)
plot.ts(intusagediff2)

par(mfrow=c(1,5)) # creating multiple graphs in one window
dev.off()

adf.test(intusage)
adf.test(intusagediff1)
adf.test(intusagediff2)


acf(diff(intusagediff2,lag.max=20))
pacf(diff(intusagediff2,lag.max=20))

par(mfrow=c(1,2))

