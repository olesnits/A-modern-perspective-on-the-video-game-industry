#Load libraries ----
library("ggplot2")
library("dplyr")
library("gridExtra")
library("ggcorrplot")
library("reshape2")
library("lubridate")
library("GGally")
library("leaps")
library("glmnet")
library("MASS")
library("caTools")
library("klaR")
library("caret")
library("class")
library("xts")
library("tseries")
library("strucchange")
library("tree")
library("randomForest")


#CLEANING DATASET ----
#Import data ----
mydata = read.csv("C:/Users/dinar/Dropbox/Statistical Data Analysis/Final Project/videogamedata.csv")

#Remove duplicates ----
mydata = mydata[!duplicated(mydata$title_x),]

#Drop columns ----
mydata = mydata[,-c(2,3,6,7,11,17,20,28)]

#Clean names ----
names(mydata)[c(1,2,11,14:20,24)] = c("title","releaseDate","priceInEur",
"genre","publisher","naSales","euSales","jpSales","otherSales","globalSales","averageTime")
rownames(mydata) = c(1:678)

#Clean date ----
clean_date = ymd(mydata$releaseDate)
mydata$releaseDate = clean_date

#Add like/dislike ratio ----
mydata$likeRatio = (mydata$positiveReviews/(mydata$positiveReviews+mydata$negativeReviews))*100
mydata$dislikeRatio = (mydata$negativeReviews/(mydata$positiveReviews+mydata$negativeReviews))*100
names(mydata)[c(25,26)] = c("likeRatio(%)","dislikeRatio(%)")

#Reorder columns ----
mydata = mydata[,c(1,2,14,15,10,11,3,12,13,25,26,4:9,23,21,22,24,16:20)]
                   
#Clean nOfPlayers ----
unique(mydata$nOfPlayers)

mydata$nOfPlayers[mydata$nOfPlayers == "1"] = "SINGLE-PLAYER"

mydata$nOfPlayers[mydata$nOfPlayers == "2  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "ONLINE MULTIPLAYER"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers =="4  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "MASSIVELY MULTIPLAYER"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "2"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "8  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "16  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "6  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "64+"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "32  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "64+  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "24  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "12  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "10  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "64  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "5  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "1-MORE THAN 64"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "44  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "3  ONLINE"] = "MULTI-PLAYER"
mydata$nOfPlayers[mydata$nOfPlayers == "14  ONLINE"] = "MULTI-PLAYER"



mydata$nOfPlayers[mydata$nOfPlayers == "1-32"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-18"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-4" ] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-8"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-40"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-12"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-6"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-16"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-64"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-24"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-10"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-2"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-20"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-22"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-15"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-5"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-3"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-33"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-9"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-30"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-36"] = "BOTH"
mydata$nOfPlayers[mydata$nOfPlayers == "1-60"] = "BOTH"

#Clean price ----
free_games = mydata[mydata$isFree == "True",c(1,2,5,6)]
x = as.numeric(rownames(free_games))
mydata$priceInEur[x] = 0

NAs = mydata[is.na(mydata$priceInEur),c(1,2,5,6)]
x = as.numeric(rownames(NAs))
mydata=mydata[-x,]


#Clean critic rating ----
mydata$criticMeanValue = mydata$criticMeanValue/10



#EXPLORATORY DATA ANALYSIS ----
#Examine Dataset ----
summary(mydata[,c(6,8:26)])

#Boxplots ----
ggplot(mydata, aes(x=nOfPlayers,y=globalSales, fill=nOfPlayers))+ 
  geom_boxplot()+xlab("No. of Players")+ylab("Global Sales (in millions")+
  theme(legend.position="none")+ylim(c(0,5))


df =  mydata %>% group_by(isFree) %>% 
  summarize(User = userMeanValue, Critic = criticMeanValue)
df.m <- melt(df, id.var = "isFree")
names(df.m)[2]="Legend"
ggplot(df.m, aes(x=isFree,y=value, color=Legend))+ 
  geom_boxplot()+xlab("Free")+ylab("Rating")


df =  mydata %>% group_by(nOfPlayers) %>% 
  summarize(`North America` = naSales, Europe = euSales, Japan = jpSales, Other= otherSales)
df.m <- melt(df, id.var = "nOfPlayers")
names(df.m)[2]="Legend"
ggplot(df.m, aes(x=nOfPlayers,y=value, color=Legend))+ 
  geom_boxplot()+xlab("No. of Players")+ylab("Sales (in millions)")+
  ylim(c(0,1))


ggplot(mydata, aes(x=isFree,y=globalSales, fill=isFree))+ 
  geom_boxplot()+xlab("Free")+ylab("Average Time")+
  theme(legend.position="none")+ylim(c(0,2))


df =  mydata %>% group_by(nOfPlayers) %>% 
  summarize(User = userMeanValue, Critic = criticMeanValue)
df.m <- melt(df, id.var = "nOfPlayers")
names(df.m)[2]="Legend"
ggplot(df.m, aes(x=nOfPlayers,y=value, color=Legend))+ 
  geom_boxplot()+xlab("No. of Players")+ylab("Rating")


ggplot(mydata, aes(x=nOfPlayers,y=averageTime, fill=nOfPlayers))+ 
  geom_boxplot()+xlab("No. of Players")+ylab("AverageTime (in hours)")+
  theme(legend.position="none")+ylim(c(0,50))


df =  mydata %>% group_by(isFree) %>% 
  summarize(Positive = positiveReviews, Negative = negativeReviews)
df.m <- melt(df, id.var = "isFree")
names(df.m)[2]="Legend"
ggplot(df.m, aes(x=isFree,y=value, color=Legend))+ 
  geom_boxplot()+xlab("Free")+ylab("")+ylim(c(0,1000))


ggplot(mydata, aes(x=genre,y=averageTime, fill=genre))+ 
  geom_boxplot()+xlab("Genre")+ylab("Average Time")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+ylim(c(0,60))+
  theme(legend.position="none")


fig1 = ggplot()+geom_histogram(data=mydata, aes(x=userMeanValue),bins=12,
col="darkblue",fill="lightblue")+xlab("User Rating")

fig2 = ggplot()+geom_qq(data = mydata,aes(sample = userMeanValue))

fig3 = ggplot()+geom_histogram(data=mydata, aes(x=criticMeanValue),bins=12,
col="darkblue",fill="lightblue")+xlab("Critic Rating")

fig4 = ggplot()+geom_qq(data = mydata,aes(sample = criticMeanValue))

grid.arrange(fig1,fig2,fig3,fig4,ncol=2,nrow=2)


fig1 = ggplot()+geom_histogram(data=mydata, aes(x=priceInEur),bins=12,
  col="darkblue",fill="lightblue")+xlab("Price (EUR)")

fig2 = ggplot()+geom_qq(data = mydata,aes(sample = priceInEur))

fig3 = ggplot()+geom_histogram(data=mydata, aes(x=globalSales),bins=12,
col="darkblue",fill="lightblue")+xlab("Global Sales")

fig4 = ggplot()+geom_qq(data = mydata,aes(sample = globalSales))

grid.arrange(fig1,fig2,fig3,fig4,ncol=2,nrow=2)


#Year vs Sales ----
df = mydata
df$releaseDate = year(df$releaseDate)

df_sales_year = df %>% group_by(releaseDate) %>% 
  summarize(tot_globalsales = sum(globalSales))
ggplot(df_sales_year, aes(releaseDate,tot_globalsales))+
  geom_point(color = "red")+xlab("Year")+
  ylab("Global Sales (in millions)")+ggtitle("Worldwide Sales")+
  geom_line(mapping = aes(color="red")) +theme(legend.position="none")










#STATISTICAL DATA ANALYSIS ----
#Correlation ----
new = mydata[,c(6,8:10,12,13,15,16,18,19,26)]
rownames(new) = c(1:660)

cor_pearson=cor(new,use="complete.obs",method = "pearson")
ggcorrplot(cor_pearson, lab = TRUE, outline.color = "white", type = "lower")

cor_s = cor(new,use="complete.obs",method = "spearman")
ggcorrplot(cor_s, lab = TRUE, outline.color = "white", type = "lower")

#Best Subset Selection ----
regfit.full=regsubsets(meanHours~.,data=new, nvmax=10)
reg.summary = summary(regfit.full)
reg.summary

#Plot RSS and R^2
par(mfrow=c(1,2))
plot(reg.summary$rss,xlab="Number of variables",ylab="RSS")
plot(reg.summary$rsq,xlab="Number of variables",ylab=expression(R^2))

#Plot Cp
par(mfrow=c(1,1))
plot(reg.summary$cp,xlab="Number of variables",ylab="Cp")
which.min(reg.summary$cp)
points(4,reg.summary$cp[4],col="red",pch=20)

#Get variables
coefficients(regfit.full, id = 4)

#Multiple Linear Regression ----
lm.fit=lm(meanHours~positiveReviews+negativeReviews+
            criticNumberReviews+globalSales,data=new)
summary(lm.fit)

#Diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

#Zoom-in on QQ Plot
par(mfrow=c(1,1))
lm.stdres = rstandard(lm.fit)
qqnorm(lm.stdres, ylab="Standardized Residuals", 
       xlab="Theoretical Quantiles",
       xlim=c(-2.1,1.5), ylim=c(-1,0.6))
qqline(lm.stdres,lty=3)

#Plot them side by side
par(mfrow=c(1,2))
plot(lm.fit, which=2, title(" "))
qqnorm(lm.stdres, ylab="Standardized residuals", 
       xlab="Theoretical Quantiles", main=" ",
       xlim=c(-2.1,1.5), ylim=c(-1,0.6))
qqline(lm.stdres,lty=3)

#Remove high leverage points and re-fit the model
new2 = new[-c(168,574,590),]
lm.fit2=lm(meanHours~positiveReviews+negativeReviews+
             criticNumberReviews+globalSales,data=new2)
summary(lm.fit2)

#Identify high-leverage points
highlev = new[c(168,574,590),c(2,3,7,11,10)]
 
#Compute 10-fold Cross Validation Test MSE
set.seed(123)
k = 10
MSE_CV = numeric(k)

folds = sample(1:k, dim(new)[1], replace = TRUE)

for(i in 1:k) {
  lm.fit=lm(meanHours~positiveReviews+negativeReviews+criticNumberReviews,data=new[folds!=i,])
  MSE_CV[i]=mean((new$meanHours[folds==i]-predict(lm.fit,new[folds==i,]))^2)}

MSE_CV
mean(MSE_CV)

#Linear Discriminant Analysis ----
#Create new binary classifier
median_hours = median(new$meanHours)
hours01 = rep(x = "low", times = length(new$meanHours))
hours01[new$meanHours > median_hours] = "high"
new = data.frame(new,hours01)

#Split into test and training data
set.seed(123)
sample = sample.split(new, SplitRatio = 0.75)
train = subset(new, sample == TRUE)
test = subset(new, sample == FALSE)

#Fit LDA model
set.seed(123)
lda.fit = lda(hours01~positiveReviews+negativeReviews+criticNumberReviews, data = train)
lda.fit
plot(lda.fit)

#Compute error rate
lda.pred = predict(lda.fit, test)
table(lda.pred$class, test$hours01)
mean(lda.pred$class != test$hours01)


#Create partition plots
par(mfrow=c(1,3))
train$hours01 = as.factor(train$hours01)
partimat(hours01~positiveReviews+negativeReviews+criticNumberReviews, data=train, method="lda",nplots.hor = 3)


#Quadratic Discriminant Analysis ----
#Fit LDA model
set.seed(123)
qda.fit = qda(hours01~positiveReviews+negativeReviews+criticNumberReviews, data = train)
qda.fit

#Compute error rate
qda.pred = predict(qda.fit, test)
table(qda.pred$class, test$hours01)
mean(qda.pred$class != test$hours01)



#K-Nearest Neighbors ----
#Use 10-fold cross-validation to obtain best K
set.seed(321)
trControl = trainControl(method  = "cv", number  = 10)
fit = train(hours01 ~ positiveReviews+negativeReviews+criticNumberReviews,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = new)
plot(fit)
fit

#In order to use the knn function, we first create two matrices for the training and test set
test.X = cbind(test$positiveReviews, test$negativeReviews, test$criticNumberReviews)
train.X = cbind(train$positiveReviews, train$negativeReviews, train$criticNumberReviews)

#Apply K-Nearest Neighbors using k=7
set.seed(321)
knn.pred = knn(train.X, test.X, train$hours01, k=7)
table(knn.pred, test$hours01)
mean(knn.pred != test$hours01)

#Regression Trees ----
set.seed(8)


#Make the subsets that we need for the test
mydata = mydata[,-c(25,24,23,22,21,20,17,14,11, 7,5,4,3,2,1)]
names(mydata)[c(4)] = c("likeRatio")
mydata=na.omit(mydata)
train = sample(dim(mydata)[1], dim(mydata)[1]/2)
mydatatrain = mydata[train, ]
mydatatest = mydata[-train, ]
Xtrain = mydata[train, -10]
Xtest = mydata[-train, -10]
Ytrain = mydata[train, 10]
Ytest = mydata[-train, 10]


#Plot the Full grown Tree on the train data
mydatatree = tree(meanHours ~. , data = mydatatrain, mindev = 0, minsize = 2 )
summary(mydatatree)
par(mfrow=c(1,1))
plot(mydatatree, uniform=TRUE)
text(mydatatree, pretty=0)

#Test MSE Full Grown Tree
pred_mydatatree = predict(mydatatree, mydatatest)
mean((mydatatest$meanHours - pred_mydatatree)^2)

#Let's see if pruning the tree would work
#CV
cv.mydata = cv.tree(mydatatree, FUN = prune.tree)
plot(cv.mydata$size, cv.mydata$dev, type = "b", xlab = "Tree Size", 
     ylab = "Error")

#The best is 7, it's useless to go further, no improvement
pruned.mydata = prune.tree(mydatatree, best = 7)
plot(pruned.mydata)
text(pruned.mydata, pretty = 0)

#Test MSE for Pruned Tree
pred.pruned = predict(pruned.mydata, mydatatest)
mean((mydatatest$meanHours - pred.pruned)^2)

#RANDOM FOREST
#Check on which p we have to use
p = dim(mydata)[2] - 1
p2 = p/2
psq = sqrt(p)

mydatap = randomForest(Xtrain, Ytrain, xtest = Xtest, ytest = Ytest, mtry = p, ntree = 500)
mydatap2 = randomForest(Xtrain, Ytrain, xtest = Xtest, ytest = Ytest, mtry = p2, ntree = 500)
mydatapsq = randomForest(Xtrain, Ytrain, xtest = Xtest, ytest = Ytest, mtry = psq, ntree = 500)


plot(1:500, mydatap$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(20000, 50000))
lines(1:500, mydatap2$test$mse, col = "red", type = "l")
lines(1:500, mydatapsq$test$mse, col = "blue", type = "l")
legend("top", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("green", "red", "blue"), 
       cex = 1.2, lty = 1, box.lty=5)

#Random forest function
bag.mydata = randomForest(meanHours ~. , data = mydatatrain, mtry = psq, ntree = 500, 
                          importance = T)
bag.pred = predict(bag.mydata, mydatatest)
mean((mydatatest$meanHours - bag.pred)^2)
importance(bag.mydata)
varImpPlot(bag.mydata)



#TIME SERIES ANALYSIS ----
#Plot time-series ----

#Convert the dataset into a time-series object first
new = mydata[,c(2,26)]
sales = new %>% group_by(releaseDate) %>% 
  summarize(tot_globalsales = sum(globalSales))
sales = sales[-1,]  #Remove observation 1992, because it is the only observation for that year
sales_ts = xts(sales$tot_globalsales,sales$releaseDate)  

#Then plot it
plot(sales_ts,xlab="Date",ylab="Global Sales (in millions)",main="")

#Plot total sales over the years ----

#First create year variable and then add it to the dataset
year = as.numeric(format(mydata$releaseDate,"%Y"))
new = cbind(new, year)

#Then create the plot
df_sales_year = new %>% group_by(year) %>% 
  summarize(tot_globalsales = sum(globalSales))
plot(df_sales_year$year,df_sales_year$tot_globalsales,type="l",xlab="Year",ylab= "Global Sales (in millions)")
abline(v=c(2009,2014), lty=2, col="lightcoral")


#Split our data into 3 sets, [1995,2009],[2010,2014],[2015,2020] ----
set1 = sales[1:230,]
set1_ts = xts(set1$tot_globalsales,set1$releaseDate)

set2 = sales[231:433,]
set2_ts = xts(set2$tot_globalsales,set2$releaseDate)

set3 = sales[434:576,]
set3_ts = xts(set3$tot_globalsales,set3$releaseDate)




#Dickey-Fuller test for stationarity ----
adf.test(sales_ts, alternative = "stationary")

adf.test(set1_ts, alternative = "stationary")
adf.test(set2_ts, alternative = "stationary")
adf.test(set3_ts, alternative = "stationary")

#Seasonality ----
par(mfrow=c(2,2))

plot = monthplot(sales_ts,xaxt="none",xlab="Month",ylab="Gloabal Sales (in millions)",main="1995 - 2020")+
axis(side=1,at = 1:12,labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                "Aug","Sep","Oct","Nov","Dec"))


plot = monthplot(set1_ts,xaxt="none",xlab="Month",ylab="Gloabal Sales (in millions)",main="1995 - 2009")+
  axis(side=1,at = 1:12,labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                   "Aug","Sep","Oct","Nov","Dec"))

plot = monthplot(set2_ts,xaxt="none",xlab="Month",ylab="Gloabal Sales (in millions)",main="2010 - 2014")+
  axis(side=1,at = 1:12,labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                   "Aug","Sep","Oct","Nov","Dec"))

plot = monthplot(set3_ts,xaxt="none",xlab="Month",ylab="Gloabal Sales (in millions)",main="2015 - 2020")+
  axis(side=1,at = 1:12,labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                                   "Aug","Sep","Oct","Nov","Dec"))



#Auto Correlation Function ----
par(mfrow=c(2,2))

acf(sales_ts,main="1995 - 2020")
acf(set1_ts,main="1995 - 2009")
acf(set2_ts,main="2010 - 2014")
acf(set3_ts,main="2015 - 2020")





#Chow Test ----
#Check assumptions of Chow Test
lm.fit=lm(globalSales~releaseDate, data=new)
lm.fit
plot(lm.fit,2)
plot(lm.fit,3)


#Run Chow Tests
set1_chow = sales[1:433,]
sctest(set1_chow$tot_globalsales ~ set1_chow$releaseDate , type = "Chow", point = 230)

set2_chow = sales[231:576,]
sctest(set2_chow$tot_globalsales ~ set2_chow$releaseDate , type = "Chow", point = 203)






