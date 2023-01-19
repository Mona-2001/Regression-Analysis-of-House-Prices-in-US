#Clean Code
library(olsrr); library(MASS); library(stats); library(psych)
#Reading the original Data & Getting the statistics
df_original=read.csv("Data.csv")
View(df_original)
summary(df_original)
head(df_original)
nrow(df_original)
#Removing the  city,street,zip, and country
df_numeric=df_original[,c(1:14)]
View(df_numeric)
head(df_numeric)
nrow(df_numeric)
#Getting the minimum of the response variable which is price
min(df_original$price) #zero. We understand that this is definitely a typo.
minimum <- subset(df_numeric, price == min(df_original$price)) 
nrow(minimum) # The number of properties (49) that have price zero.
df_modified = subset(df_numeric, price != min(df_original$price)) # removing price = 0
# df_modified now has no 0 price, no city column,no street,no country,no zip code

#Change the format of the date and only keep the month since it all belongs to same year and for the same time it is on the first day in the month
df_modified$date
nrow(df_modified)
any(is.na(df_modified))
View(df_modified)
dates <- df_modified$date
dates <- gsub(x=dates,pattern=" 0:00",replacement="",fixed=T)
any(is.na(dates) )
dates <- as.Date(dates,format = "%m/%d/%Y")
any(is.na(dates))
dates <- format(as.POSIXct(dates,format='%Y-%m-%d'),format='%m')
x<-as.data.frame(dates)
View(x)
df_modified$date = dates
df_modified$date = as.integer(df_modified$date)#changing the dates to integers to graph the data
df_modified=df_modified[,c(2,1,3:14)]
View(df_modified)
nrow(df_modified)
head(df_modified)
# Graphs before fitting the model
dfStand=scale(df_modified)
dfStand=as.data.frame(dfStand)
windows()
Boxplot(~price, data = dfStand, id.n = Inf,pch = 19,main = 'Box Plot of Standardized Response Variable')
# Histogram for each variable
windows()
for(j in 1:ncol(df_modified)){  
  windows(); hist(df_modified[,j],xlab= names(df_modified)[j],main=paste("Histogram of ",names(df_modified)[j]))
}
windows()
# Plot matrix
pairs(df_modified,pch=19)   

# Setting the categorical variables using as.factor
View(df_modified)
df_modified$waterfront = as.factor(df_modified$waterfront)
df_modified$view = as.factor(df_modified$view)
df_modified$condition = as.factor(df_modified$condition)
df_modified$date = as.factor(df_modified$date)
# Bathrooms and floors have float numbers for maid
# They will be rounded up for simpler categorizing
df_modified$bathrooms = ceiling(df_modified$bathrooms)
df_modified$floors = ceiling(df_modified$floors)
View(df_modified)
#Fitting the model using the data frame df_modified
ols=ols_regress(price ~ ., data = df_modified) # Gives complete pretty output
cat("\nRegression Output:\n")
print(ols)
reg=lm(price ~ ., data = df_modified)
#Graphs After Fitting
windows()
IStudRes=rstandard(reg) 
windows()
op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(IStudRes,pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
plot(reg$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)
# Before plotting the rest of the graphs, we will deal with the NA first
# Outcome of iteration 1: perfect collinearity exists

# Start of iteration 2: fixing the collinearity
#As the coefficent of sqft_basement is NA, upon research we find that NA as a coefficient in a regression indicates that the variable in question is linearly related to the other variables.
#Upon strong reason we find that the sum of sqft_basement and sqft_above is sqft_living.
#This is a case of collinearity so we have two options. 
#Either drop sqft_basement and sqft_above or sqft_living only.
#Thus we fit two models and we compare their p-value.
df_basement_above_no_living= subset(df_modified, select = -sqft_living)
reg_basement_above = lm(price~.,data = df_basement_above_no_living)
summary(reg_basement_above)

df_living_no_basement_above= subset(df_modified, select = -c(sqft_basement,sqft_above))
reg_living_no_basement_above = lm(price~.,data =df_living_no_basement_above)
summary(reg_living_no_basement_above)
#Since the two models have the same p value. 
#We will work with the following data frame: df_living_no_basement_above.
df_modified = df_living_no_basement_above
reg = lm(df_modified$price~.,data = df_modified)
summary(reg)
#Graphs After Fitting
windows()
IStudRes=rstandard(reg) 
windows()
op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(IStudRes,pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals
plot(reg$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)
# End of iteration 2: Perfect collinearity solved

# Iteration 3: Dealing iwth the extreme 2 outliers shown in the boxplot of price
dfNoOutlier=df_living_no_basement_above[-c(4351,4347),]
reg_no_outlier=lm(dfNoOutlier$price~.,data=dfNoOutlier)
summary(reg_no_outlier)
# Fitting the graphs of new model
IStudRes=rstandard(reg_no_outlier) 
windows()
op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(IStudRes,pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals     # Index plot of internally studentized Residuals
plot(reg_no_outlier$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)
# Now we will plot the residual graphs to verify the assumptions
# After the model changes
View(dfNoOutlier)
p=reg_no_outlier$rank-1      # Number of predictors
# each predictor vs. residual
for(j in 1:(p-1)){  
  windows()
  plot(dfNoOutlier[,j+1],IStudRes,pch=19,xlab = names(dfNoOutlier)[j+1],main=paste("Plot of IStudRes v.",names(dfNoOutlier)[j+1]),ylab = "IStudRes")
}
# Iteration 3 done: 2 extreme outliers are removed, but there are still other outliers
# Also fitted values vs. residuals is not random so we will consider transforming Y

#Iteration 4: Transforming Y
# We will choose the best lambda for transformation
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  #print(i)
  df1=dfNoOutlier
  print(min(df1$price))
  df1$price = (df1$price )^lambda[i]
  #head(df1)
  if(lambda[i]==0) {df1$price=log(dfNoOutlier$price)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# From the graphs above, there are 3 possible lambda (-0.5,0,0.5)
# However, 0.5 produced a higher R2 while the others decrease it
#so we can infer that transforming Y to Y^0.5 is best transformation

# This part should be run once
dfNoOutlier$price = dfNoOutlier$price^0.5
reg=lm(dfNoOutlier$price ~.,data=dfNoOutlier)
summary(reg)
View(dfNoOutlier)
# Checking verifications after regression
IStudRes=rstandard(reg) 
windows()
op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(IStudRes,pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals     # Index plot of internally studentized Residuals
plot(reg$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)
# each predictor vs. residual
p=reg$rank-1      # Number of predictors
for(j in 2:p){  
  windows()
  plot(dfNoOutlier[,j],IStudRes,pch=19,xlab = names(dfNoOutlier)[j],main=paste("Plot of IStudRes v.",names(dfNoOutlier)[j]),ylab = "IStudRes")
}
# Iteration 4 is done and Y is transformed


# Iteration 5: Examining whether transformation is needed 
# in an attempt to fix the graphs above
# This will occur by examining each predictor
View(dfNoOutlier)
# Predictor 1: sqft_lot
windows()
plot(dfNoOutlier$sqft_lot,IStudRes,pch=19,xlab = names(dfNoOutlier$sqft_lot),main=paste("Plot of IStudRes v. sqft_lot"),ylab = "IStudRes")
identify(dfNoOutlier$sqft_lot,IStudRes)
# Examining Transforming sqft_lot
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$sqft_lot = (df1$sqft_lot )^lambda[i]
  if(lambda[i]==0) {df1$sqft_lot=log(dfNoOutlier$sqft_lot)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor



# Predictor 2: sqft_living
#windows()
#plot(dfNoOutlier$sqft_living,IStudRes,pch=19,xlab = names(dfNoOutlier$sqft_living),main=paste("Plot of IStudRes v. sqft_living"),ylab = "IStudRes")
#identify(dfNoOutlier$sqft_living,IStudRes)
# Examining Transforming sqft_lot
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$sqft_living = (df1$sqft_living)^lambda[i]
  if(lambda[i]==0) {df1$sqft_living=log(dfNoOutlier$sqft_living)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# lambda 2 is the best among the plots because points are closer to the line, 
# transformation of sqft_living is based on lambda = 2
df_Trial = dfNoOutlier
regOrg = lm(df_Trial$price~.,data = df_Trial)
summary(regOrg)
df_Trial$sqft_living = df_Trial$sqft_living ^2
regN = lm(df_Trial$price~.,data = df_Trial)
summary(regN)
# After transformation, since the model decreased to 0.5373 then we will not apply the transformation


# Predictor 3: bedrooms
#windows()
#plot(dfNoOutlier$bedrooms,IStudRes,pch=19,xlab = names(dfNoOutlier$bedrooms),main=paste("Plot of IStudRes v. bedrooms"),ylab = "IStudRes")
#identify(dfNoOutlier$bedrooms,IStudRes)

# Examining Transforming bedrooms
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$bedrooms = (df1$bedrooms + 1)^lambda[i]
  if(lambda[i]==0) {df1$bedrooms=log(dfNoOutlier$bedrooms + 1)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor


# Predictor 4: bathrooms
#windows()
#plot(dfNoOutlier$bathrooms,IStudRes,pch=19,xlab = names(dfNoOutlier$bathrooms),main=paste("Plot of IStudRes v. bathrooms"),ylab = "IStudRes")
#identify(dfNoOutlier$bathrooms,IStudRes)

# Examining Transforming bathrooms
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,2,4)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$bathrooms = (df1$bathrooms + 1)^lambda[i]
  if(lambda[i]==0) {df1$bathrooms=log(dfNoOutlier$bathrooms + 1)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# lambda 2 is the best among the plots because points are closer to the line, 
# transformation of bathrooms is based on lambda = 2
df_Trial = dfNoOutlier
regOrg = lm(df_Trial$price~.,data = df_Trial)
summary(regOrg)
df_Trial$bathrooms = df_Trial$bathrooms ^2
regN = lm(df_Trial$price~.,data = df_Trial)
summary(regN)
# Since the transformation did not decrease R2 adjusted, then we can keep the transformation of lambda = 2
dfNoOutlier$bathrooms = dfNoOutlier$bathrooms ^2



# Predictor 5: floors
#windows()
#plot(dfNoOutlier$floors,IStudRes,pch=19,xlab = names(dfNoOutlier$floors),main=paste("Plot of IStudRes v. floors"),ylab = "IStudRes")
#identify(dfNoOutlier$floors,IStudRes)

# Examining Transforming floors
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$floors = (df1$floors)^lambda[i]
  if(lambda[i]==0) {df1$floors=log(dfNoOutlier$floors)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor



# Predictor 6: date
# we know it is categorical, but we want to examine if their will be change by changing lambda
#windows()
#plot(dfNoOutlier$date,IStudRes,pch=19,xlab = names(dfNoOutlier$date),main=paste("Plot of IStudRes v. date"),ylab = "IStudRes")
#identify(dfNoOutlier$date,IStudRes)

# Examining Transforming date
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$date = as.numeric(df1$date)^lambda[i]
  if(lambda[i]==0) {df1$date=log(as.numeric(dfNoOutlier$date))}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor


# Predictor 7: waterfront
# we know it is categorical, but we want to examine if their will be change by changing lambda
#windows()
#plot(dfNoOutlier$waterfront,IStudRes,pch=19,xlab = names(dfNoOutlier$waterfront),main=paste("Plot of IStudRes v. waterfront"),ylab = "IStudRes")
#identify(dfNoOutlier$waterfront,IStudRes)

# Examining Transforming waterfront
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$waterfront = (as.numeric(df1$waterfront)+1)^lambda[i]
  if(lambda[i]==0) {df1$waterfront=log(as.numeric(dfNoOutlier$waterfront)+1)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor



# Predictor 8: view
# we know it is categorical, but we want to examine if their will be change by changing lambda
#windows()
#plot(dfNoOutlier$view,IStudRes,pch=19,xlab = names(dfNoOutlier$view),main=paste("Plot of IStudRes v. view"),ylab = "IStudRes")
#identify(dfNoOutlier$view,IStudRes)

# Examining Transforming view
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$view = (as.numeric(df1$view)+1)^lambda[i]
  if(lambda[i]==0) {df1$view=log(as.numeric(dfNoOutlier$view)+1)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor



# Predictor 9: condition
# we know it is categorical, but we want to examine if their will be change by changing lambda
#windows()
#plot(dfNoOutlier$condition,IStudRes,pch=19,xlab = names(dfNoOutlier$condition),main=paste("Plot of IStudRes v. condition"),ylab = "IStudRes")
#identify(dfNoOutlier$condition,IStudRes)

# Examining Transforming condition
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$condition = as.numeric(df1$condition)^lambda[i]
  if(lambda[i]==0) {df1$condition=log(as.numeric(dfNoOutlier$condition))}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor


# Predictor 10: yr_built
#windows()
#plot(dfNoOutlier$yr_built,IStudRes,pch=19,xlab = names(dfNoOutlier$yr_built),main=paste("Plot of IStudRes v. yr_built"),ylab = "IStudRes")
#identify(dfNoOutlier$yr_built,IStudRes)

# Examining Transforming yr_built
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$yr_built = df1$yr_built^lambda[i]
  if(lambda[i]==0) {df1$yr_built=log(dfNoOutlier$yr_built)}
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor


# Predictor 11: yr_renovated
#windows()
#plot(dfNoOutlier$yr_renovated,IStudRes,pch=19,xlab = names(dfNoOutlier$yr_renovated),main=paste("Plot of IStudRes v. yr_renovated"),ylab = "IStudRes")
#identify(dfNoOutlier$yr_renovated,IStudRes)

# Examining Transforming yr_renovated
lambda=c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
windows()
op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
for (i in 1:length(lambda)){
  df1=dfNoOutlier
  df1$yr_renovated = (df1$yr_renovated+1)^lambda[i]
  if(lambda[i]==0) {df1$yr_renovated=log(dfNoOutlier$yr_renovated+1)}
  #print(df1$yr_renovated)
  reg=lm(df1$price ~.,data=df1)
  IStudRes=rstandard(reg) 
  qqnorm(IStudRes, ylab = "IStudRes",pch=19,main = paste("lambda =",lambda[i]) )
  qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
}
par(op)
# there is no significant difference between plots of lambda and lambda = 1, 
# so no transformation will be done for this predictor

# Iteration 5 is done: only bathrooms transformation was useful for the model
# Now we will check graphs before moving to iteration 6
regTran = lm(dfNoOutlier$price~.,data = dfNoOutlier)
summary(regTran)
# Fitting the graphs of new model
IStudRes=rstandard(regTran) 
windows()
op <- par(mfrow = c(2,2)) 
hist(IStudRes,xlab="IStudRes")
qqnorm(IStudRes, ylab = "IStudRes",pch=19)
qqline(IStudRes, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75), qtype = 7)
plot(IStudRes,pch=19,xlab="Index",ylab="IStudRes")       # Index plot of internally studentized Residuals     # Index plot of internally studentized Residuals
plot(regTran$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
par(op)
# Now we will plot the residual graphs to verify the assumptions
# After the model changes
View(dfNoOutlier)
p=ncol(dfNoOutlier) - 1      # Number of predictors
p
# each predictor vs. residual
for(j in 2:(p+1)){  
  windows()
  plot(dfNoOutlier[,j],IStudRes,pch=19,xlab = names(dfNoOutlier)[j],main=paste("Plot of IStudRes v.",names(dfNoOutlier)[j]),ylab = "IStudRes")
}
# The graphs are still similar to the previous ones, showing that the problem is not solved
# In an attempt to make a better model, let's check if interactive variables will have a better impact on the model

summary(regTran)

#Iteration 6: Checking for interactive variables.
# Let's first plot the graphs of  the four graphs to check our current outliers
windows()
op <- par(mfrow = c(2,2)) 
plot(cooks.distance(regTran),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(regTran),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(regTran)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)

# Fitted values vs. residuals
windows()
plot(regTran$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
identify(regTran$fitted,IStudRes)
# The graphs of the previous iteration between predictor and residual showed outliers in each graph
# Let's examine closely the graphs of bedrooms and bathrooms
# Bathrooms vs residuals
windows()
plot(dfNoOutlier$bathrooms,IStudRes,pch=19,xlab = 'Bathrooms',main=paste("Plot of IStudRes v. bathrooms"),ylab = "IStudRes")
identify(dfNoOutlier$bathrooms,IStudRes)
# Bedrooms vs residuals
windows()
plot(dfNoOutlier$bedrooms,IStudRes,pch=19,xlab = 'Bedrooms',main=paste("Plot of IStudRes v. bedrooms"),ylab = "IStudRes")
identify(dfNoOutlier$bedrooms,IStudRes)
# Interactive Variable: bedrooms*bathrooms
inter1 = dfNoOutlier$bedrooms*dfNoOutlier$bathrooms
dfWithInter = dfNoOutlier
#View(dfWithInter)
dfWithInter['Bed*Bathroom'] = inter1
regWithout = lm(dfNoOutlier$price~.,data = dfNoOutlier)
summary(regWithout)
regWithInter = lm(dfWithInter$price~.,data = dfWithInter)
summary(regWithInter)
# since the interactive variable has a significant coefficient and the R2 adjusted slightly increase to 0.5941,
# then this interactive variable is important
dfNoOutlier['Bed*Bathroom'] = inter1
View(dfNoOutlier)
# Fitted values vs. residual graph
IStudRes=rstandard(regWithInter) 
windows()
plot(regWithInter$fitted,IStudRes,pch=19,xlab="Fitted Values",ylab="IStudres")
identify(regWithInter$fitted,IStudRes)
# The fitted values graph is still similar which suggests that more interactive variables can be added
# Let's see the four graphs again after the new model
windows()
op <- par(mfrow = c(2,2)) 
plot(cooks.distance(regWithInter),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(regWithInter),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(regWithInter)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)

# Let's examine if the year of building the house and its condition can be a significant interacitve variable
# from the graph of yr_built vs residuals, it is a band around zero with some extreme outliers
# the boxplot of condition shows also several outliers
# We will have a model with and without interactive variable = yr_built*condition
# Interactive Variable: bedrooms*bathrooms
inter2 = dfNoOutlier$yr_built*as.numeric(dfNoOutlier$condition)
dfWithInter = dfNoOutlier
#View(dfWithInter)
dfWithInter['Built*Condition'] = inter2
regWithout = lm(dfNoOutlier$price~.,data = dfNoOutlier)
summary(regWithout)
regWithInter = lm(dfWithInter$price~.,data = dfWithInter)
summary(regWithInter)
# The results between the 2 models show that the new interactive variable is insignificant
# Therefore it will not be added to the model
# We are now thinking that it is possible that unresolved collinearity is the cause of the problem
# Iteration 6 ends by adding the interactive variable


# Iteration 7: Examining the collinearity problem
#View(dfNoOutlier)

#Testing for Collinearity
#1-Condition Index
dfCol=sapply(dfNoOutlier,as.numeric)
dfCol=as.data.frame(dfCol)
C=cor(dfCol[,-1])
E=eigen(C)
L=E$val; V=E$vec
kappa=sqrt(L[1]/L) 
print(kappa)
# One of the values is more than 10. Thus, there is collinearity

# Another way of testing for collinearity
#2-Variance Inflation Factor
#reg_col=lm(dfCol$price~.,data=dfCol)
#ols_vif_tol(reg_col)

# Reason for collinearity
#Applying PC Regression
M=as.matrix(dfCol[,-1])
W= M %*% V
colnames(W)=paste("W",(1:ncol(W)),sep="")  
dfPC=data.frame(dfCol$price,W)
reg_PC=lm(dfCol.price ~ . -1,data=dfPC)
summary(reg_PC) 
#From the results, the insignificant W's are W7, W12, and we will consider
# W6 insignificant because it is 0.0515 
alpha=reg_PC$coef
V%*%alpha # same as beta hat
beta_pc=V[,c(1:5,8:11)]%*%alpha[c(1:5,8:11)] # PC Regression Coefficients
y_hat_pc=M%*%beta_pc               # PC Regression Fitted Values
windows()

plot(y_hat_pc,dfCol$price,pch=19,xlab="PC Fitted Values",ylab="Price") 

# Fitting W against residuals
#IStudRes=rstandard(reg_PC) 
summary(reg_PC)
windows()
op <- par(mfrow = c(3,3))
for (i in c(2:12)[-c(6,7,12)]){
  #print(i)
  x=names(dfPC)[i]
  y=rstandard(reg_PC)
  plot(dfPC[,i],rstandard(reg_PC),xlab=x,ylab='IStudRes', pch=19, cex=0.5, main = paste("Internally studentized residuals vs.",x))
}
par(op)

# Four graphs
windows()
op <- par(mfrow = c(2,2)) 
plot(cooks.distance(reg_PC),pch=19,xlab="Index",ylab="Cook's Distance") # Index plot of Cook's distance
plot(ols_leverage(reg_PC),pch=19,xlab="Index",ylab="Leverage Values")
Hinf=ols_hadi(reg_PC)        # Computes Hadi's Influence
plot(Hinf$hadi,pch=19,ylab="Hadi's Influence")  # Index plotof Hadi's Influence
#ols_plot_hadi(reg)       # Index plot of Hadi's Influence
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
par(op)


# From this graph, we conclude that there are still some unresolved issues
# which may be due to the fact that the dataset is incomplete or a part of a larger dataset
# However, by the end of iteraion 7, R2 adjusted has increased to 0.967
#but since the graphs are still the same then there are still unresolved issues
