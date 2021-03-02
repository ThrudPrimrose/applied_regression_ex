library(tidyverse)
data <- read_csv("path/to/csv.csv")

#linear model
lm(y ~ x1 + x2 + x1:x2, data = data)
#equal to
lm(y ~ x1*x2, data = data)
#intercept only
lm(y ~ 1, data = data)
#lm with polynom of degree 2
lm(y ~ x + I(x^2), data = data)
#any degree
lm(y ~ poly(x, degree = n, raw = TRUE, data= data))
#up to interactions of 2nd order
lm(y ~ .^2, data = data)

#covariance (dont forget to multiply by n-1)
cov(data)

summary(model)
anova(model)

#get correlations between covariates
cor(data)

#SS regression, k=num of covariates (no intercept!)
sum(anove(model)[1:k, "Sum Sq"])

#SS residueals
anova(model)["Residuals", "Sum Sq"]

#get coefficients of model
coef(model)

#T test with significance level alpha: get 1-alpha/2 
abs(T_val) > qt(1-alpha/2, df)? "discard H0" : "accept H0"

#F test with significance level alpha
#H0:B1,B2,B3 = 0; HA:full model
#a of restricted model = rank of A with AB = 0 (normally = amound of =0 covariates)
# n = df, k = num of covs, 1: intercept
qf(1-alpha,rank(A),n-k-1)

#SS residuals (multiple measurements per point)
SS_res <-sum(residuals(model)^2))
# Sum over the output for PESS: pure error sums of squares
data %>% group_by(x) %>% summaries(square_sum_mean = sum((y ~ mean(y))^2))

#Autocorrelation of residuals
#need residuals first
resid_data <-data.frame(resid =residuals(model),lag1_resid =lag(residuals(model)))
acf(resid_data$resid)
#or something like
acf(ts(fit$residuals))

#Durbin Watson test stat
SS_resid <-sum(resid_data$resid^2)
(r1 <-sum(resid_data$resid[-1]*resid_data$lag1_resid[-1])/SS_resid)
#or
resids <-residuals(model)
D <-sum(diff(resids)^2)/ sum(resids)
#or 
library(lmtest)
dwtest(fit,alternative="two.sided")

#HAT matrix
X <-cbind(rep(1,ncol(weight)), weight$x)
H <- X%*% solve(t(X)%*%X)%*% t(X)
#leverage ( leverage point if h_i > 2*mean(h))
h <-diag(H)

#Fit statistics
lev <-influence(model) #outlier if lev > 2*mean(lev_crime)
hatcook <-cooks.distance(model) #outlier if cook > 0.5
studres <-rstudent(model) #outlier if abs(studres) >2

#COX-boxtransforation
summary(lambda_fit <- car::powerTransform(model))
#results with output lambda that can be choosed for coxbox transformation
model_bc <-lm(car::bcPower(model, lambda_fit$roundlam)~., data = data)

#Forward | Backwards | Both selection 
#upper and lower limits as models
low = lm(y~1, data = data)
high = lm(y ~., data = data)
#k=2 -> AIC, k =log(n) -> BIC
#direction = both | backward | forward
step(lm(y ~ ., data = data), scope = list(lower = low, upper = high),
direction = "both", k = log(nrow(data)))
#call getCall to the output of step to see the formula
getCall(step(...))


#non-linear model
# x_i are varaibles from the data, betas are starting values for the formula
nls(y ~ x1 * beta1 / (x1+beta2), start = list(beta1= b1_val, beta2 = b2_val), data = data)

#time series
library(nlme)
fit=gls(y~x, correlation=corARMA(p=1,q=0), data = data)

#Survival fit
library(KMsurv)
surv_fit <- survfit(Surv(y, delta) ~ x, data = data)
#H0 = curves same, HA = curves different, reject dpending on p
survdiff(Surv(y, delta)~x, data = data)
#coxph survival fit
#coefficients retrieved here are the parts of the hazard function of form
# h_o(t)*exp(c1*x1 + c2*x2 + ...)
coxph_fit <-coxph(Surv(y, delta)~x1+x2, data = bfeed)
#tests
testlogrank=survdiff(Surv(times,tatus) ~ hpa, rho=0)
testwilcox=survdiff(Surv(times,status) ~ hpa ,rho=1)

#logistic regression
logreg <-glm(y~., data = data, family =binomial(link = "logit"))
#Select with from all covariates starting with intercept only model going upto intercept with ^2
model_aic <-step(model, scope =list(lower =~1, upper =~.^2)
#poisson regression, we are interested in ratio therefore offset=log(y)
#y is the amount of Pr(X)=1 and there is a total of popsize amount of trials, 
#we are interested in the probability of y/popsize
posreg <-glm(y~., offset =log(popsize),data = data, family =poisson(link = "log"))
#logistic -> how many cases will be good=1 or bad=0
#logistic -> minimize missclassification
#poission -> how much the cases are going to do (counts)
#poission -> 

#Returns fitted values of the data points -> compare y and y_hat
fitted(model)
#Call predict if you want to predict new y's from xi's, new data is a new dataframe
#if newdata is not set then uses the model's data
predict(model, newdata = anotherdataframe)

#k -> number of neighbors to look
#if prob is true then return to ratio in the majority vote
# train -> training data frame, test -> 
knn(train = train[, -1], test = train[, -1], 
    cl = zip_train$digit,
    k = k, prob = TRUE))

#knn returns factor with prediction for comparison etc transform the vecs to factor too
