---
title: "Chapter 5 Resampling Methods"
author: "JN"
date: "04/11/2020"
output: 
    html_document:
      keep_md: true
---



## Exercises 
#### Applied Q5 - Q9

### **Question 5**
In Chapter 4, we used logistic regression to predict the probability of `default` using `income` and `balance` on the `Default` data set. We will now estimate **_the test error_** of this logistic regression model **_using the validation set approach_**.

(a) Fit a logistic regression model that uses `income` and `balance` to predict `default`.

```r
set.seed(12)
glimpse(Default)
```

```
## Rows: 10,000
## Columns: 4
## $ default <fct> No, No, No, No, No, No, No, No, No, No, No, No, No, No, No,...
## $ student <fct> No, Yes, No, No, No, Yes, No, Yes, No, No, Yes, Yes, No, No...
## $ balance <dbl> 729.5265, 817.1804, 1073.5492, 529.2506, 785.6559, 919.5885...
## $ income  <dbl> 44361.625, 12106.135, 31767.139, 35704.494, 38463.496, 7491...
```

```r
Default <- na.omit(Default)
dim(Default)
```

```
## [1] 10000     4
```

```r
lr_0 <- glm(default ~ income + balance, data = Default, family = binomial)
summary(lr_0)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```

(b) Using the validation set approach, estimate the test error of this model.

* **Step 1**: Split the sample set into a training set and a validation set.

```r
n <- dim(Default)[1]

train <- sample(1:n, floor(n/2))
validation <- (1:n)[-train]
```

* **Step 2**: Fit a multiple logistic regression model using only the training observations.

```r
lr_1 <- glm(default ~ income + balance, data = Default, subset = train, family = binomial)
```

* **Step 3**: Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the `default` category if the posterior probability is greater than 0.5.

```r
lr_1_probs <- predict(lr_1, newdata = Default[validation, ], type = "response")
lr_1_pred <- factor(rep("No", length(validation)), c("No", "Yes"))
lr_1_pred[lr_1_probs > 0.5] <- factor("Yes", c("No", "Yes"))
```

* **Step 4**: Compute the validation set error, which is the fraction of the observations in the validation set that are misclassfied.

```r
test_error_rate <- mean(lr_1_pred != Default[validation, ]$default)
test_error_rate
```

```
## [1] 0.027
```
(c) Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set.

```r
test_error_rate <- function(){
  n <- dim(Default)[1]

  train <- sample(1:n, floor(n/2))
  validation <- (1:n)[-train]
  
  lr_1 <- glm(default ~ income + balance, data = Default, subset = train, family = binomial)
  
  lr_1_probs <- predict(lr_1, newdata = Default[validation, ], type = "response")
  lr_1_pred <- factor(rep("No", length(validation)), c("No", "Yes"))
  lr_1_pred[lr_1_probs > 0.5] <- factor("Yes", c("No", "Yes"))
  
  test_error_rate <- mean(lr_1_pred != Default[validation, ]$default)
  return(test_error_rate)
}

test_error_rate()
```

```
## [1] 0.0266
```

```r
test_error_rate()
```

```
## [1] 0.0262
```

```r
test_error_rate()
```

```
## [1] 0.0248
```

(d) Now consider a logistic regression model that predict the probability of `default` using `income`, `balance`, and a dummy variable for `student`. Estimate the test error for this model using the validation set approach.

```r
n <- dim(Default)[1]

train <- sample(1:n, floor(n/2))
validation <- (1:n)[-train]
  
lr_2 <- glm(default ~ income + balance + student, data = Default, subset = train, family = binomial)
  
lr_2_probs <- predict(lr_2, newdata = Default[validation, ], type = "response")
lr_2_pred <- factor(rep("No", length(validation)), c("No", "Yes"))
lr_2_pred[lr_2_probs > 0.5] <- factor("Yes", c("No", "Yes"))
  
test_error_rate <- mean(lr_2_pred != Default[validation, ]$default)
test_error_rate
```

```
## [1] 0.0292
```

### **Question 5**
We will now compute estimates for the standard errors of the income and balance logistic regression coefficients in two different ways: (1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the `glm()` function. 

(a) Using the `summary()` and `glm()` functions, determine the estimated standard errors for the coefficients associated with income and balance in a multiple logistic regression model that uses both predictors. 

```r
set.seed(5)
Default <- na.omit(Default)

lr <- glm(default ~ income + balance, data = Default, family = binomial)
summary(lr)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```

```r
summary(lr)$coefficients[, 2]
```

```
##  (Intercept)       income      balance 
## 4.347564e-01 4.985167e-06 2.273731e-04
```

(b) Write a function, `boot.fn()`, that takes as input the Default data set as well as an index of the observations, and that outputs the coefficient estimates for income and balance in the multiple logistic regression model.

```r
boot.fn <- function(data, index){
  m <- glm(default ~ income + balance, data = data[index, ], family = binomial)
  return(coefficients(m))
}
```

(c) Use the `boot()` function together with your `boot.fn()` function to estimate the standard errors of the logistic regression coefficients for income and balance.

```r
boot.fn(Default, 1:10000)
```

```
##   (Intercept)        income       balance 
## -1.154047e+01  2.080898e-05  5.647103e-03
```

```r
boot(Default, boot.fn, R = 1000 ) # produce 1000 bootstrap estimates
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Default, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##          original        bias     std. error
## t1* -1.154047e+01 -3.787648e-02 4.380326e-01
## t2*  2.080898e-05  2.733337e-07 4.909003e-06
## t3*  5.647103e-03  1.512143e-05 2.332406e-04
```

(d) Comment on the estimated standard errors obtained using the `glm()` function and using your bootstrap function.

This indicates that the bootstrap estimate for  <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_0)"> is 0.438, and that the bootstrap estimate for <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_1) = 4.909 * 10^{-6}">, and <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_2) = 2.332 * 10^{-4} ">.

The standard error estimates for <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_0)">, <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_1"> and <img src="https://render.githubusercontent.com/render/math?math=SE(\hat\beta_2"> obtained using the `summary()` function are 0.435, <img src="https://render.githubusercontent.com/render/math?math=4.985 * 10^{-6}">, and  <img src="https://render.githubusercontent.com/render/math?math=2.274 * 10^{-4}">, respectively. These are different from the estimates obtained using the boorstrap.

The boostrap approach is likely giving a more accurate estimate of the standard errors than is the `summary()` function since the bootstrap does not depend on any of assumptions that `glm()` has to rely on. 

### **Question 7**
You will compute the LOOCV error for a simple logistic regression model on the Weekly data set.

(a) Fit a logistic regression model that predicts Direction using Lag1 and Lag2.

```r
set.seed(5)
Weekly <- na.omit(Weekly)

m_lr <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
```

(b) Fit a logistic regression model that predicts Direction using Lag1 and Lag2 using all but the first observation.

```r
m_loocv <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
```

(c) Use the model from (b) to predict the direction of the first observation. 

```r
pred <- predict(m_loocv, Weekly[1, ], type = "response")
pred # >0.5: Up, and <0.5: Down
```

```
##         1 
## 0.5713923
```

```r
Weekly$Direction[1] # the true direction for the first observation
```

```
## [1] Down
## Levels: Down Up
```

The prediction of the direction of the first observation using the model in (b) is up, but the true direction is down. So, this observation is not correctly classified. 

(d) Write a for loop from i = 1 to i = n, where n is the number of observations in the data set.

* **Step i**: Fit a logistic regression model using all but the ith observation to predict Direction using Lag1 and Lag2.

* **Step ii**: Compute the posterior probability of the market moving up for the ith observation.

* **Step iii**: Use the posterior probability for the ith observation in order to predict whether or not the market moves up.

* **Step iv**: Determine whether or not an error was made in predicting the direction for the ith observation. If an error was made, then indicate this as a 1, and otherwise indicate it as a 0.


```r
n <- dim(Weekly)[1] # 1089 observations
number_errors <- 0
for(i in 1:n){
  m_loocv <- glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial) # step i
  pred <- predict(m_loocv, Weekly[i, ], type = "response") # step ii
  direction <- Weekly[i, ]$Direction # step iii
  error1 <- (pred > 0.5) & (direction == "Down") 
  error2 <- (pred < 0.5) & (direction == "Up")
  if(error1 | error2){  # step iv
    number_errors <- number_errors + 1 
  }
  
}
number_errors
```

```
## [1] 490
```

(e) Take the average of the n numbers obtained in (d)iv in order to obtain the LOOCV estimate for the test error. Comment on the results.

```r
number_errors/n # LOOCV test error rate
```

```
## [1] 0.4499541
```

### **Question 8**
We will now perform cross-validation on a simulated data set.

(a) Generate a simulated data set as follows:

```r
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
```

In this data set, what is n and waht is p? Write out the model used to generate the data in equation form.

n = 100, p = 2.

The model is <img src="https://render.githubusercontent.com/render/math?math=Y = X - 2X^2 %2B \epsilon">, and <img src="https://render.githubusercontent.com/render/math?math=\epsilon ~ N(0, 1)">.

(b) Create a scatterplot of X against Y. Comment on what you find.

```r
df <- data.frame(x, y)
ggplot(df, aes(x, y)) +
  geom_point()
```

![Fig 1: A scatterplot of X against Y](chapter-5-Resampling-Methods_files/figure-html/8(b)-1.png)

This scatter-plot is shown a quadratic pattern for this data set.

(c) Set a random seed, and then compute the LOOCV errors that result from fitting the following four models using least squares:

* i. <img src="https://render.githubusercontent.com/render/math?math=Y = \beta_0 %2B \beta_1X %2B \epsilon"> 

* ii. <img src="https://render.githubusercontent.com/render/math?math=Y = \beta_0 %2B \beta_1X %2B \beta_2X^2 %2B \epsilon">

* iii. <img src="https://render.githubusercontent.com/render/math?math=Y = \beta_0 %2B \beta_1X %2B \beta_2X^2 %2B \beta_3X^3 %2B \epsilon">

* iv. <img src="https://render.githubusercontent.com/render/math?math=Y = \beta_0 %2B \beta_1X %2B \beta_2X^2 %2B \beta_3X^3 %2B \beta_4X^4 %2B \epsilon">


```r
set.seed(15)

m_i <- glm(y ~ x, data = df)
m_ii <- glm(y ~ x + I(x^2), data = df)
m_iii <- glm(y ~ x + I(x^2) + I(x^3), data = df)
m_iv <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = df)

cv.err_i <- cv.glm(df, m_i)
cv.err_i$delta[1] # model i's LOOCV
```

```
## [1] 7.288162
```

```r
cv.err_ii <- cv.glm(df, m_ii)
cv.err_ii$delta[1] # model ii's LOOCV
```

```
## [1] 0.9374236
```

```r
cv.err_iii <- cv.glm(df, m_iii)
cv.err_iii$delta[1] # model iii's LOOCV
```

```
## [1] 0.9566218
```

```r
cv.err_iv <- cv.glm(df, m_iv)
cv.err_iv$delta[1] # model iv's LOOCV
```

```
## [1] 0.9539049
```

(d) Repeat (c) using another random seed, and report you results. Are your results the same as what you got in (c)? Why?

```r
set.seed(55)

m_i <- glm(y ~ x, data = df)
m_ii <- glm(y ~ x + I(x^2), data = df)
m_iii <- glm(y ~ x + I(x^2) + I(x^3), data = df)
m_iv <- glm(y ~ x + I(x^2) + I(x^3) + I(x^4), data = df)

cv.err_i <- cv.glm(df, m_i)
cv.err_i$delta[1] # model i's LOOCV
```

```
## [1] 7.288162
```

```r
cv.err_ii <- cv.glm(df, m_ii)
cv.err_ii$delta[1] # model ii's LOOCV
```

```
## [1] 0.9374236
```

```r
cv.err_iii <- cv.glm(df, m_iii)
cv.err_iii$delta[1] # model iii's LOOCV
```

```
## [1] 0.9566218
```

```r
cv.err_iv <- cv.glm(df, m_iv)
cv.err_iv$delta[1] # model iv's LOOCV
```

```
## [1] 0.9539049
```

The results are the same since there is no randomness in the training/validation set splits.

(e) Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.

The model ii has the smallest LOOCV. This result is what we expected since the scatter-plot in (b) has a quadratic pattern.

(f) Comment on the statistical significance of the coefficient estimates that results from fitting each of the models in (c) using least squares. Do these results agree with the conclusions drawn based on the cross-validation results?

```r
summary(m_i)
```

```
## 
## Call:
## glm(formula = y ~ x, data = df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -9.5161  -0.6800   0.6812   1.5491   3.8183  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.6254     0.2619  -6.205 1.31e-08 ***
## x             0.6925     0.2909   2.380   0.0192 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 6.760719)
## 
##     Null deviance: 700.85  on 99  degrees of freedom
## Residual deviance: 662.55  on 98  degrees of freedom
## AIC: 478.88
## 
## Number of Fisher Scoring iterations: 2
```

```r
summary(m_ii)
```

```
## 
## Call:
## glm(formula = y ~ x + I(x^2), data = df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9650  -0.6254  -0.1288   0.5803   2.2700  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.05672    0.11766   0.482    0.631    
## x            1.01716    0.10798   9.420  2.4e-15 ***
## I(x^2)      -2.11892    0.08477 -24.997  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9178258)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  89.029  on 97  degrees of freedom
## AIC: 280.17
## 
## Number of Fisher Scoring iterations: 2
```

```r
summary(m_iii)
```

```
## 
## Call:
## glm(formula = y ~ x + I(x^2) + I(x^3), data = df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9765  -0.6302  -0.1227   0.5545   2.2843  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.06151    0.11950   0.515    0.608    
## x            0.97528    0.18728   5.208 1.09e-06 ***
## I(x^2)      -2.12379    0.08700 -24.411  < 2e-16 ***
## I(x^3)       0.01764    0.06429   0.274    0.784    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9266599)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  88.959  on 96  degrees of freedom
## AIC: 282.09
## 
## Number of Fisher Scoring iterations: 2
```

```r
summary(m_iv)
```

```
## 
## Call:
## glm(formula = y ~ x + I(x^2) + I(x^3) + I(x^4), data = df)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0550  -0.6212  -0.1567   0.5952   2.2267  
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.156703   0.139462   1.124    0.264    
## x            1.030826   0.191337   5.387 5.17e-07 ***
## I(x^2)      -2.409898   0.234855 -10.261  < 2e-16 ***
## I(x^3)      -0.009133   0.067229  -0.136    0.892    
## I(x^4)       0.069785   0.053240   1.311    0.193    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9197797)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  87.379  on 95  degrees of freedom
## AIC: 282.3
## 
## Number of Fisher Scoring iterations: 2
```

Based on the results on the model iii and model iv, we noticed that the coefficients for <img src="https://render.githubusercontent.com/render/math?math=X^3"> and 
<img src="https://render.githubusercontent.com/render/math?math=X^4"> are zero since their p-values are greater than the 5% significance level. This results agree with the conclusion drawn based on the LOOCV results that the model ii is the best for this data set.

### **Question 9 **
We will now consider the Boston housing data set, from the MASS library.

(a) Based on this data set, provide an estimate for the population mean of **medv**. Call this estimate <img src="https://render.githubusercontent.com/render/math?math=\hat\mu">.

```r
Boston <- na.omit(Boston)
mu_hat_medv <- mean(Boston$medv)
mu_hat_medv
```

```
## [1] 22.53281
```

(b) Provide an estimate of the standard error of the <img src="https://render.githubusercontent.com/render/math?math=\hat\mu">. Interpret this result.

```r
n <- dim(Boston)[1]
se_mu_hat_medv <- sd(Boston$medv)/sqrt(n)
se_mu_hat_medv
```

```
## [1] 0.4088611
```

(c) Now estimate the standard error of <img src="https://render.githubusercontent.com/render/math?math=\hat\mu"> using the bootstrap. How does this compare to your answer from (b)?

```r
set.seed(12)
mu_boot.fn <- function(data, index){
  mean(data[index])
}

boot_se <- boot(Boston$medv, mu_boot.fn, 1000)
boot_se
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = mu_boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original     bias    std. error
## t1* 22.53281 0.01542372   0.4091716
```

The estmate of the se of  <img src="https://render.githubusercontent.com/render/math?math=\hat\mu"> is 0.4092 which is similar to the result 0.4089 in (b).

(d) Based on your bootstrap estimate from (c), provide a 95% CI for the mean of **medv**. Compare it to the results obtained using `t.test(Boston$medv)`.

```r
t.test(Boston$medv)
```

```
## 
## 	One Sample t-test
## 
## data:  Boston$medv
## t = 55.111, df = 505, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  21.72953 23.33608
## sample estimates:
## mean of x 
##  22.53281
```

```r
boot_CI <- c(mu_hat_medv - 2 * 0.4176, mu_hat_medv + 2 * 0.4176)
boot_CI
```

```
## [1] 21.69761 23.36801
```

bootstrap 95% CI: (21.6976, 23.3680)

t.test 95% CI: (21.7295, 23.3361)

(e) Based on the data set, provide an estimate,  <img src="https://render.githubusercontent.com/render/math?math=\hat\mu_med">, for the median value of **medv** in the population.

```r
med_medv <- median(Boston$medv)
med_medv
```

```
## [1] 21.2
```

(f) Estimate the se of the median using the bootstrap.

```r
set.seed(12)
med_boot.fn <- function(data, index){
  median(data[index])
}

boot(Boston$medv, med_boot.fn, 1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = med_boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*     21.2  0.0049   0.3769457
```

The standard error of the median is 0.3769.

(g) Provide an estimate for the tenth percentile of **medv** in Boston suburbs.

```r
quantile(Boston$medv, probs=c(0.1))
```

```
##   10% 
## 12.75
```

(h) Use the bootstrap to estimate the standard error of the tenth percentile of **medv**.

```r
set.seed(50)
quantile10_boot.fn <- function(data, index){
  quantile(data[index], probs=c(0.1))
}

boot(Boston$medv, quantile10_boot.fn, 1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Boston$medv, statistic = quantile10_boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*    12.75  0.0158   0.5028849
```

The standard error is slightly larger (0.5029) relative to the tenth percentile of **medv**, but it is still small.


























