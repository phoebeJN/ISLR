---
title: "Chapter 3 Linear Regression"
author: "JN"
date: "24/10/2020"
output: 
     html_document:
       keep_md: true
---



## Exercises 
### Applied Q8 - Q15


#### **Question 8**
(a) Use `lm()` function to perform a simple linear regression with **_mpg_** as the response and **_horsepower_** as the predictor.

```r
Auto = na.omit(Auto)
round(mean(Auto$mpg), 3)
```

```
## [1] 23.446
```

```r
lm.fit8 <- lm(mpg ~ horsepower, data = Auto)
summary(lm.fit8)
```

```
## 
## Call:
## lm(formula = mpg ~ horsepower, data = Auto)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.5710  -3.2592  -0.3435   2.7630  16.9240 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 39.935861   0.717499   55.66   <2e-16 ***
## horsepower  -0.157845   0.006446  -24.49   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.906 on 390 degrees of freedom
## Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
## F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16
```

```r
predict(lm.fit8, data.frame(horsepower = 98),
        interval = "confidence")
```

```
##        fit      lwr      upr
## 1 24.46708 23.97308 24.96108
```

```r
predict(lm.fit8, data.frame(horsepower = 98),
        interval = "prediction")
```

```
##        fit     lwr      upr
## 1 24.46708 14.8094 34.12476
```

i. Is there a relationship between **_mpg_** and **_horsepower_** ?

Let's test the hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0:\beta_{horsepower} = 0">

Based on the result above, we show that the p-value is very low, so we should reject the null hypothesis. Then, there is clear evidence of a relationship between **_mpg_** and **_horsepower_**. 

ii. How strong is the relationship between the predictor and the response?

the RSE(residual standard error) is 4.906 units while the mean value for the response is 23.446, indicating a percentage error of approximately 21%. And, the  <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic = 0.6059 which means that the predictor (**_horsepower_**) explain almost 61% of the variance in the response (**_mpg_**). 

iii. Is the relationship between the predictor and the response positive and negative?

The relationship is negative that an increase of 100 horsepower is associated with an decrease in miles per gallon (**_mpg_**)by around 16 units.

iv. What is the predicted **_mpg_** associated with a **_horsepower_** of 98? What are the associated 95% confidence and prediction intervals?

The predicted value of **_mpg_** is about 24.467 when **_horsepower_** is 98. The associated 95% confidence and prediction intervals are (23.973, 24.961) and (14.809, 34.125), respectively. 

(b) Plot the response and the predictor.

```r
ggplot(Auto, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](chapter-3-Linear-Regression_files/figure-html/8(b)-1.png)<!-- -->

(C) Produce diagnostic plots of the least squares regression fit. 

```r
par(mfrow = c(2, 2))
plot(lm.fit8)
```

![](chapter-3-Linear-Regression_files/figure-html/8(c)-1.png)<!-- -->

The residual plot shows that the residuals exhibit a U-shape, which provides a strong indication of non-linearity in the data.


#### **Question 9**
(a)

```r
pairs(Auto)
```

![](chapter-3-Linear-Regression_files/figure-html/9(a)-1.png)<!-- -->

(b)

```r
round(cor(subset(Auto, select = -name)), 3)
```

```
##                 mpg cylinders displacement horsepower weight acceleration
## mpg           1.000    -0.778       -0.805     -0.778 -0.832        0.423
## cylinders    -0.778     1.000        0.951      0.843  0.898       -0.505
## displacement -0.805     0.951        1.000      0.897  0.933       -0.544
## horsepower   -0.778     0.843        0.897      1.000  0.865       -0.689
## weight       -0.832     0.898        0.933      0.865  1.000       -0.417
## acceleration  0.423    -0.505       -0.544     -0.689 -0.417        1.000
## year          0.581    -0.346       -0.370     -0.416 -0.309        0.290
## origin        0.565    -0.569       -0.615     -0.455 -0.585        0.213
##                year origin
## mpg           0.581  0.565
## cylinders    -0.346 -0.569
## displacement -0.370 -0.615
## horsepower   -0.416 -0.455
## weight       -0.309 -0.585
## acceleration  0.290  0.213
## year          1.000  0.182
## origin        0.182  1.000
```

(c)

```r
lm.fit9 <- lm(mpg ~. -name, data = Auto)
summary(lm.fit9)
```

```
## 
## Call:
## lm(formula = mpg ~ . - name, data = Auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.5903 -2.1565 -0.1169  1.8690 13.0604 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -17.218435   4.644294  -3.707  0.00024 ***
## cylinders     -0.493376   0.323282  -1.526  0.12780    
## displacement   0.019896   0.007515   2.647  0.00844 ** 
## horsepower    -0.016951   0.013787  -1.230  0.21963    
## weight        -0.006474   0.000652  -9.929  < 2e-16 ***
## acceleration   0.080576   0.098845   0.815  0.41548    
## year           0.750773   0.050973  14.729  < 2e-16 ***
## origin         1.426141   0.278136   5.127 4.67e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.328 on 384 degrees of freedom
## Multiple R-squared:  0.8215,	Adjusted R-squared:  0.8182 
## F-statistic: 252.4 on 7 and 384 DF,  p-value: < 2.2e-16
```

i. Is there a relationship between the predictors and the response?

Let's test the hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0:\beta_{cylinders}"> = <img src="https://render.githubusercontent.com/render/math?math=\beta_{displacement}">= <img src="https://render.githubusercontent.com/render/math?math=\beta_{horsepower}"> = <img src="https://render.githubusercontent.com/render/math?math=\beta_{weight}"> = <img src="https://render.githubusercontent.com/render/math?math=\beta_{acceleration}"> = <img src="https://render.githubusercontent.com/render/math?math=\beta_{year}"> = <img src="https://render.githubusercontent.com/render/math?math=\beta_{origin}"> = 0.

 <img src="https://render.githubusercontent.com/render/math?math=H_a"> : at least one <img src="https://render.githubusercontent.com/render/math?math=\beta_j"> is non-zero.

Since the F-statistic for the multiple linear regression model is 252.4 which is far larger than 1, it provides compelling evidence against the null hypotheses H<sub>0</sub>. So, at least one of the predictors must be related to the response.


ii. Which predictors appear to have a statistically significant relationship to the response?
Based on the result above, we look at the individual p-values. We notice that **_displacement_**, **_weight_**, **_year_**, and **_origin_** are statistically significant related to the response since their p-values are smaller than the 5% significant level.  

iii. What does the coefficient for the year variable suggest?
For a given amount of other variables, each passing year leads to an increase in miles per gallon (**_mpg_**) by approximately 0.75 units. 

(d)

```r
par(mfrow = c(3, 2))
plot(lm.fit9)
plot(predict(lm.fit9), rstudent(lm.fit9), xlab = "Fitted Value", ylab = "Studentized residuals", main = "Studentized residuals vs Fitted")
```

![](chapter-3-Linear-Regression_files/figure-html/9(d)-1.png)<!-- -->

The residual plot shows that the residuals exhibit a U-shape, which provides a strong indication of non-linearity in the data. Therefor, the multiple linear regression model is not fit for the data set.

Studentized residuals plots can show which observations are possible outliers. Observations whose studentized residuals are greater than 3 in absolute value are possible outliers. The studentized residuals plot in this data set is shown above. We can see there are studentized residuals of several points are greater than 3, so there are some unusually large outliers.

Observations with high leverage have an unusual value for the predictor. In the leverage plot above, observation 14 clearly has a very high leverage statistic compared with other observations.

(e)

```r
summary(lm(mpg ~ displacement + horsepower + acceleration + horsepower:acceleration + cylinders*weight + year + origin, data = Auto))
```

```
## 
## Call:
## lm(formula = mpg ~ displacement + horsepower + acceleration + 
##     horsepower:acceleration + cylinders * weight + year + origin, 
##     data = Auto)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.3499  -1.6685  -0.0141   1.5009  12.0571 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             -7.4837124  5.5164273  -1.357 0.175702    
## displacement            -0.0048855  0.0075637  -0.646 0.518716    
## horsepower               0.0806652  0.0236283   3.414 0.000709 ***
## acceleration             0.7885834  0.1514762   5.206 3.16e-07 ***
## cylinders               -4.0179263  0.5878598  -6.835 3.26e-11 ***
## weight                  -0.0120372  0.0012008 -10.024  < 2e-16 ***
## year                     0.7814074  0.0447175  17.474  < 2e-16 ***
## origin                   0.5833654  0.2552730   2.285 0.022845 *  
## horsepower:acceleration -0.0092849  0.0016782  -5.533 5.86e-08 ***
## cylinders:weight         0.0013150  0.0001633   8.051 1.05e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.912 on 382 degrees of freedom
## Multiple R-squared:  0.864,	Adjusted R-squared:  0.8608 
## F-statistic: 269.7 on 9 and 382 DF,  p-value: < 2.2e-16
```

The two interactions are both statistically significant related to the response since the p-values are both very small. The results suggest that the model that includes the interaction term is superior to the model that contains only main effects. The <img src="https://render.githubusercontent.com/render/math?math=R^2"> for the model is 86.4%, compared to 82.2% for the model without the two interaction terms. This means that 23.6% of the variability in **_mpg_** that remains after the model had been explained by the interaction terms.

(f)

```r
lm.fit_update <- lm(mpg ~. -name + horsepower:acceleration + cylinders:weight + I(displacement^2) + I(horsepower^2) + I(weight^2) + I(acceleration^2), data = Auto)
summary(lm.fit_update)
```

```
## 
## Call:
## lm(formula = mpg ~ . - name + horsepower:acceleration + cylinders:weight + 
##     I(displacement^2) + I(horsepower^2) + I(weight^2) + I(acceleration^2), 
##     data = Auto)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9.6637 -1.5620 -0.0046  1.4700 12.2571 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)              1.923e+01  1.232e+01   1.561   0.1193    
## cylinders               -9.569e-01  1.449e+00  -0.660   0.5095    
## displacement            -9.203e-03  2.226e-02  -0.413   0.6796    
## horsepower              -1.462e-01  8.334e-02  -1.754   0.0803 .  
## weight                  -1.331e-02  2.875e-03  -4.630 5.02e-06 ***
## acceleration            -1.796e+00  1.029e+00  -1.745   0.0818 .  
## year                     7.794e-01  4.499e-02  17.324  < 2e-16 ***
## origin                   6.257e-01  2.657e-01   2.354   0.0191 *  
## I(displacement^2)        6.757e-06  4.093e-05   0.165   0.8690    
## I(horsepower^2)          4.080e-04  1.673e-04   2.438   0.0152 *  
## I(weight^2)              1.124e-06  6.401e-07   1.755   0.0800 .  
## I(acceleration^2)        5.207e-02  2.345e-02   2.220   0.0270 *  
## horsepower:acceleration -1.158e-03  3.362e-03  -0.344   0.7307    
## cylinders:weight         3.713e-04  4.358e-04   0.852   0.3947    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.864 on 378 degrees of freedom
## Multiple R-squared:  0.8698,	Adjusted R-squared:  0.8653 
## F-statistic: 194.3 on 13 and 378 DF,  p-value: < 2.2e-16
```

```r
par(mfrow = c(3, 2))
plot(lm.fit_update)
plot(predict(lm.fit_update), rstudent(lm.fit_update), xlab = "Fitted Value", ylab = "Studentized residuals", main = "Studentized residuals vs Fitted")

anova(lm.fit9, lm.fit_update)
```

```
## Analysis of Variance Table
## 
## Model 1: mpg ~ (cylinders + displacement + horsepower + weight + acceleration + 
##     year + origin + name) - name
## Model 2: mpg ~ (cylinders + displacement + horsepower + weight + acceleration + 
##     year + origin + name) - name + horsepower:acceleration + 
##     cylinders:weight + I(displacement^2) + I(horsepower^2) + 
##     I(weight^2) + I(acceleration^2)
##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
## 1    384 4252.2                                  
## 2    378 3101.0  6    1151.2 23.389 < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

![](chapter-3-Linear-Regression_files/figure-html/9(f)-1.png)<!-- -->

The residual plot in this data set shows a U-shape pattern which means that the model contains at least a quadratic term.

There appears to be little pattern in the residuals from the updated model which includes quadratic terms. Therefore, the quadratic terms improve the fit to the data set. The <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic increases to approximately 87%.

The `anova()` function performs a hypothesis test compaing the two models. 

<img src="https://render.githubusercontent.com/render/math?math=H_0:">the two models fit the data equally well.

<img src="https://render.githubusercontent.com/render/math?math=H_a:">the full model is superior.

The F-statistic is 23.389 and the associated p-value is close to zero, so we can reject the null hypothesis. This provides strong evidence that model containing quadratic terms is far better than the model only contains main effects.


#### **Question 10**
(a)

```r
glimpse(Carseats)
```

```
## Rows: 400
## Columns: 11
## $ Sales       <dbl> 9.50, 11.22, 10.06, 7.40, 4.15, 10.81, 6.63, 11.85, 6.5...
## $ CompPrice   <dbl> 138, 111, 113, 117, 141, 124, 115, 136, 132, 132, 121, ...
## $ Income      <dbl> 73, 48, 35, 100, 64, 113, 105, 81, 110, 113, 78, 94, 35...
## $ Advertising <dbl> 11, 16, 10, 4, 3, 13, 0, 15, 0, 0, 9, 4, 2, 11, 11, 5, ...
## $ Population  <dbl> 276, 260, 269, 466, 340, 501, 45, 425, 108, 131, 150, 5...
## $ Price       <dbl> 120, 83, 80, 97, 128, 72, 108, 120, 124, 124, 100, 94, ...
## $ ShelveLoc   <fct> Bad, Good, Medium, Medium, Bad, Bad, Medium, Good, Medi...
## $ Age         <dbl> 42, 65, 59, 55, 38, 78, 71, 67, 76, 76, 26, 50, 62, 53,...
## $ Education   <dbl> 17, 10, 12, 14, 13, 16, 15, 10, 10, 17, 10, 13, 18, 18,...
## $ Urban       <fct> Yes, Yes, Yes, Yes, Yes, No, Yes, Yes, No, No, No, Yes,...
## $ US          <fct> Yes, Yes, Yes, Yes, No, Yes, No, Yes, No, Yes, Yes, Yes...
```

```r
Carseats = na.omit(Carseats)
dim(Carseats)
```

```
## [1] 400  11
```

```r
lm.fit10 <- lm(Sales ~ Price + Urban + US, data = Carseats)
```

(b)

```r
summary(lm.fit10)
```

```
## 
## Call:
## lm(formula = Sales ~ Price + Urban + US, data = Carseats)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9206 -1.6220 -0.0564  1.5786  7.0581 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 13.043469   0.651012  20.036  < 2e-16 ***
## Price       -0.054459   0.005242 -10.389  < 2e-16 ***
## UrbanYes    -0.021916   0.271650  -0.081    0.936    
## USYes        1.200573   0.259042   4.635 4.86e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.472 on 396 degrees of freedom
## Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2335 
## F-statistic: 41.52 on 3 and 396 DF,  p-value: < 2.2e-16
```

For a given amount of other quantitative variables and constant qualitative variables, an increase by $100 on price leads to an decrease in sales by approximately 5.4 units (in thousands). 

Hold other variables constant, a store is in urban that leads to an decrease in sales by approximately 0.02 units (in thousands).

Hold other variables constant, a store is in the US that leads to an increase in sales by approximately 1.2 units (in thousands).

(c)
Sales = <img src="https://render.githubusercontent.com/render/math?math=\beta_{Intercept}"> + <img src="https://render.githubusercontent.com/render/math?math=\beta_{Price}"> * Price + <img src="https://render.githubusercontent.com/render/math?math=\beta_{Urban}"> * Urban + <img src="https://render.githubusercontent.com/render/math?math=\beta_{US} ">* US + <img src="https://render.githubusercontent.com/render/math?math=\epsilon">

where

Sales: Unit sales (in thousands) at each location

Price: Price charged by competitor at each location

Urban: A factor with levels No (Urban = 0) and Yes (Urban = 1) to indicate whether the store is in an urban or rural location

US: A factor with levels No (US = 0) and Yes (US = 1) to indicate whether the store is in the US or not

<img src="https://render.githubusercontent.com/render/math?math=\epsilon">: a random error term

(d)
We can reject the null hypothesises <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_{Price} = 0">, and <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_{US} = 0"> since the associated p-values are really small.

(e)

```r
lm.fit_s <- lm(Sales ~ Price + US, data = Carseats)
```

(f)

```r
summary(lm.fit_s)
```

```
## 
## Call:
## lm(formula = Sales ~ Price + US, data = Carseats)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.9269 -1.6286 -0.0574  1.5766  7.0515 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 13.03079    0.63098  20.652  < 2e-16 ***
## Price       -0.05448    0.00523 -10.416  < 2e-16 ***
## USYes        1.19964    0.25846   4.641 4.71e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.469 on 397 degrees of freedom
## Multiple R-squared:  0.2393,	Adjusted R-squared:  0.2354 
## F-statistic: 62.43 on 2 and 397 DF,  p-value: < 2.2e-16
```

```r
anova(lm.fit10, lm.fit_s)
```

```
## Analysis of Variance Table
## 
## Model 1: Sales ~ Price + Urban + US
## Model 2: Sales ~ Price + US
##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
## 1    396 2420.8                           
## 2    397 2420.9 -1  -0.03979 0.0065 0.9357
```

The two different models' RSEs and <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistics are similar, and the results of ANOVA method shows that the two models fit the data equally well. 

<img src="https://render.githubusercontent.com/render/math?math=H_0:">the two models fit the data equally well.

<img src="https://render.githubusercontent.com/render/math?math=H_a:">the full model is superior.


The F-statistic is 0.0065 and the associat p-value is 0.9357, so we do not reject the null hypothesis. 

(g)

```r
confint(lm.fit_s)
```

```
##                   2.5 %      97.5 %
## (Intercept) 11.79032020 14.27126531
## Price       -0.06475984 -0.04419543
## USYes        0.69151957  1.70776632
```

(h)

```r
par(mfrow = c(3, 2))
plot(lm.fit_s)
plot(predict(lm.fit_s), rstudent(lm.fit_s), xlab = "Fitted Value", ylab = "Studentized residuals", main = "Studentized residuals vs Fitted")
```

![](chapter-3-Linear-Regression_files/figure-html/10(h)-1.png)<!-- -->

Since all observations have sudentized residuals between -3 and 3, there is no possible outliers.

If a given observation has a leverage statistic that exceeds (p+1)/n, then we may claim that this point has high leverage. In this data set, (p+1)/n = 0.0075. Look at the leverage plot above, we notice that there are some observations have high leverage.


#### **Question 11**
We generate a predictor x and response y as follows.

```r
set.seed(1)
n <- 100
x <- rnorm(n)
y <- 2 * x + rnorm(n)
```

(a) Perform a simple linear regression of y onto x, _without_ an intercept.


```r
lm.fit11 <- lm(y ~ x + 0) # without an intercept
summary(lm.fit11)
```

```
## 
## Call:
## lm(formula = y ~ x + 0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.9154 -0.6472 -0.1771  0.5056  2.3109 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## x   1.9939     0.1065   18.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9586 on 99 degrees of freedom
## Multiple R-squared:  0.7798,	Adjusted R-squared:  0.7776 
## F-statistic: 350.7 on 1 and 99 DF,  p-value: < 2.2e-16
```

The estimatied coefficient <img src="https://render.githubusercontent.com/render/math?math=\hat\beta_a"> is about 2.

The standard error of this coefficient estimate <img src="https://render.githubusercontent.com/render/math?math=se(\hat\beta_a)"> is 0.1065.

The t-statistic is 18.73, and the associated p-value is close to zero. Therefore, we reject the null hypothesis  <img src="https://render.githubusercontent.com/render/math?math=H_0:\beta_a = 0.">, there is clear evidence to show that x is related to y.

(b) Perform a simple linear regression of x onto y without an intercept.

```r
lm.fit_rev <- lm(x ~ y + 0)
summary(lm.fit_rev)
```

```
## 
## Call:
## lm(formula = x ~ y + 0)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8699 -0.2368  0.1030  0.2858  0.8938 
## 
## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## y  0.39111    0.02089   18.73   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4246 on 99 degrees of freedom
## Multiple R-squared:  0.7798,	Adjusted R-squared:  0.7776 
## F-statistic: 350.7 on 1 and 99 DF,  p-value: < 2.2e-16
```

The estimatied coefficient <img src="https://render.githubusercontent.com/render/math?math=\hat\beta_b"> is about 0.39.

The standard error of this coefficient estimate <img src="https://render.githubusercontent.com/render/math?math=se(\hat\beta_b)"> is approximately 0.02.

The t-statistic is 18.73, and the associated p-value is close to zero. Therefore, we reject the null hypothesis  <img src="https://render.githubusercontent.com/render/math?math=H_0:\beta_b = 0.">, there is clear evidence to show that y is related to x.

(c)

The values of the t-statistic are exactly the same. 
Since y = 2x + <img src="https://render.githubusercontent.com/render/math?math=\epsilon"> , x = 0.5(y - <img src="https://render.githubusercontent.com/render/math?math=\epsilon">) . We would expect <img src="https://render.githubusercontent.com/render/math?math=\hat\beta_a"> is approximately 2, and <img src="https://render.githubusercontent.com/render/math?math=\hat\beta_b"> is approximately 0.5. However, our <img src="https://render.githubusercontent.com/render/math?math=\hat\beta_b"> is about 0.39 which is not close to 0.5.

AS <img src="https://render.githubusercontent.com/render/math?math=R^2\rightarrow 1,\hat\beta_b \rightarrow \frac1\hat\beta_a">.

(d)

```r
(sqrt(n - 1) * sum(x * y)) / (sqrt(sum(x * x) * sum(y * y) - (sum(x * y)) ^ 2))
```

```
## [1] 18.72593
```

(Skip the proof)

(e)
See part(c)

```r
df <- data.frame(x, y)
plot1 <- ggplot(df, aes(x = x, y = y)) +
  geom_point()
plot2 <- ggplot(df, aes(x = y, y = x)) +
  geom_point()
ggarrange(plot1, plot2)
```

![](chapter-3-Linear-Regression_files/figure-html/11(e)-1.png)<!-- -->

 These two plot are just switched x- and y- axes.
 Also, based on the t-statistic formula, we can get <img src="https://render.githubusercontent.com/render/math?math=T_{y~x} = T_{x~y}"> 

(f)

```r
summary(lm(y ~ x))
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.8768 -0.6138 -0.1395  0.5394  2.3462 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.03769    0.09699  -0.389    0.698    
## x            1.99894    0.10773  18.556   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9628 on 98 degrees of freedom
## Multiple R-squared:  0.7784,	Adjusted R-squared:  0.7762 
## F-statistic: 344.3 on 1 and 98 DF,  p-value: < 2.2e-16
```

```r
summary(lm(x ~ y))
```

```
## 
## Call:
## lm(formula = x ~ y)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.90848 -0.28101  0.06274  0.24570  0.85736 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  0.03880    0.04266    0.91    0.365    
## y            0.38942    0.02099   18.56   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4249 on 98 degrees of freedom
## Multiple R-squared:  0.7784,	Adjusted R-squared:  0.7762 
## F-statistic: 344.3 on 1 and 98 DF,  p-value: < 2.2e-16
```

Look at the results, we can say that the values of the t-statistic are the same (both are 18.56).


#### **Question 12**
(a)

When <img src="https://render.githubusercontent.com/render/math?math=\displaystyle\sum_{i=1}^n x_i^2 = \displaystyle\sum_{i=1}^n y_i^2 ">, the coefficient estimate for the regression of X onto Y is the same as the coefficient estimate for the regression of Y onto X.

(b)

```r
set.seed(1)
n <- 100
x <- rnorm(n)
y <- 2 * x + rnorm(n)
coef(lm(y ~ x))
```

```
## (Intercept)           x 
## -0.03769261  1.99893961
```

```r
coef(lm(x ~ y))
```

```
## (Intercept)           y 
##  0.03880394  0.38942451
```

(c)

```r
set.seed(1)
n <- 100
x <- rnorm(n)
y <- x 
coef(lm(y ~ x))
```

```
##   (Intercept)             x 
## -2.220446e-17  1.000000e+00
```

```r
coef(lm(x ~ y))
```

```
##   (Intercept)             y 
## -2.220446e-17  1.000000e+00
```

#### Question 13
(a)

```r
set.seed(1)
n <- 100
x <- rnorm(n)
```

(b)

```r
eps <- rnorm(n, mean = 0, sd = sqrt(0.25)) # var = sd^2
```

(c)

```r
y <- -1 + 0.5 * x + eps
length(y)
```

```
## [1] 100
```

<img src="https://render.githubusercontent.com/render/math?math=\beta_0 = -1, \beta_1 = 0.5">

(d)

```r
data <- data.frame(x, y)
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "pink") +
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  ggtitle("A Scatterplot") 
```

```
## `geom_smooth()` using formula 'y ~ x'
```

![](chapter-3-Linear-Regression_files/figure-html/13(d)-1.png)<!-- -->

* The intercept is -1 and the slope is approximately 0.5.

* Positive linear relationship

* Weak relationship because lots of points are not close enough to the straight line.

(e)

```r
lm.fit13 <- lm(y ~ x)
summary(lm.fit13)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.93842 -0.30688 -0.06975  0.26970  1.17309 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.01885    0.04849 -21.010  < 2e-16 ***
## x            0.49947    0.05386   9.273 4.58e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.4814 on 98 degrees of freedom
## Multiple R-squared:  0.4674,	Adjusted R-squared:  0.4619 
## F-statistic: 85.99 on 1 and 98 DF,  p-value: 4.583e-15
```

<img src="https://render.githubusercontent.com/render/math?math=\hat\beta_0 \approx -1, \hat\beta_1 \approx 0.5">

The <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic is approximately 47%. This value is small that means the regression did not explain much of the variability in the response. We should improve our model. 

(f)

```r
gg1 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "pink") +
  geom_abline(aes(intercept = coef(lm.fit13)[1], slope = coef(lm.fit13)[2], col = "fit")) +
  geom_abline(aes(intercept = -1, slope = 0.5, col = "true")) + 
  scale_colour_manual(name = "", values = c("orange", "purple")) +
  theme(legend.position = "bottom") +
  ggtitle("Model-1")
gg1
```

![](chapter-3-Linear-Regression_files/figure-html/13(f)-1.png)<!-- -->

(g)

```r
lm.fit_sq <- lm(y ~ x + I(x ^ 2))

summary(lm.fit_sq)
```

```
## 
## Call:
## lm(formula = y ~ x + I(x^2))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.98252 -0.31270 -0.06441  0.29014  1.13500 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.97164    0.05883 -16.517  < 2e-16 ***
## x            0.50858    0.05399   9.420  2.4e-15 ***
## I(x^2)      -0.05946    0.04238  -1.403    0.164    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.479 on 97 degrees of freedom
## Multiple R-squared:  0.4779,	Adjusted R-squared:  0.4672 
## F-statistic:  44.4 on 2 and 97 DF,  p-value: 2.038e-14
```

The p-value of the quadratic term is 0.164 which is greater than the 5% significant level, so we do not reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_{x^2} = 0">. 

The <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic is approximately 47.8% that does not increase in a significant amount.

Therefore, the quadratic term does not improve the model fit.

(h)

```r
eps_less <- rnorm(100, mean = 0, sd = sqrt(0.1))
y <- -1 + 0.5 * x + eps_less
lm.fit_less <- lm(y ~ x)
summary(lm.fit_less)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.92152 -0.15252 -0.01433  0.20531  0.83534 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.99135    0.03311  -29.94   <2e-16 ***
## x            0.50669    0.03678   13.78   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3287 on 98 degrees of freedom
## Multiple R-squared:  0.6595,	Adjusted R-squared:  0.656 
## F-statistic: 189.8 on 1 and 98 DF,  p-value: < 2.2e-16
```

```r
data <- data.frame(x, y)
gg2 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "grey") +
  geom_abline(aes(intercept = coef(lm.fit_less)[1], slope = coef(lm.fit_less)[2], col = "fit"))  +
  geom_abline(aes(intercept = -1, slope = 0.5, col = "true")) + 
  scale_colour_manual(name = "", values =  c("orange", "purple")) +
  theme(legend.position = "bottom") +
  ggtitle("Less Noise")
gg2
```

![](chapter-3-Linear-Regression_files/figure-html/13(h)-1.png)<!-- -->

Compared to the model-1, the RSE decreases and <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic increases greatly. Therefore, the linear model is better fit the data set that is less noise.

(i)

```r
eps_more <- rnorm(100, mean = 0, sd = sqrt(0.5))
y <- -1 + 0.5 * x + eps_more
lm.fit_more <- lm(y ~ x)
summary(lm.fit_more)
```

```
## 
## Call:
## lm(formula = y ~ x)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.7793 -0.3856 -0.0267  0.4758  1.3286 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.95922    0.07091 -13.527  < 2e-16 ***
## x            0.46062    0.07876   5.848 6.55e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7039 on 98 degrees of freedom
## Multiple R-squared:  0.2587,	Adjusted R-squared:  0.2512 
## F-statistic:  34.2 on 1 and 98 DF,  p-value: 6.553e-08
```

```r
data <- data.frame(x, y)
gg3 <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_abline(aes(intercept = coef(lm.fit_more)[1], slope = coef(lm.fit_more)[2], col = "fit"))  +
  geom_abline(aes(intercept = -1, slope = 0.5, col = "true")) + 
  scale_colour_manual(name = "", values =  c("orange", "purple")) +
  theme(legend.position = "bottom") +
  ggtitle("More Noise")
gg3
```

![](chapter-3-Linear-Regression_files/figure-html/13(i)-1.png)<!-- -->

Compared to the model-1, the RSE increases and <img src="https://render.githubusercontent.com/render/math?math=R^2"> statistic decreases considerably. Therefore, the linear model is worse fit the data set that is more noise.

(j)

```r
ggarrange(gg1,gg2,gg3, ncol = 3, common.legend = TRUE)
```

![](chapter-3-Linear-Regression_files/figure-html/13(j)-1.png)<!-- -->

```r
round(confint(lm.fit13, level = 0.95), 3)
```

```
##              2.5 % 97.5 %
## (Intercept) -1.115 -0.923
## x            0.393  0.606
```

```r
round(confint(lm.fit_less, level = 0.95), 3)
```

```
##              2.5 % 97.5 %
## (Intercept) -1.057 -0.926
## x            0.434  0.580
```

```r
round(confint(lm.fit_more, level = 0.95), 3)
```

```
##              2.5 % 97.5 %
## (Intercept) -1.100 -0.819
## x            0.304  0.617
```

* Less noise model: the 95% confidence interval is narrower than model-1's, since <img src="https://render.githubusercontent.com/render/math?math=se(\hat\beta_1)"> decreases.

* More noise model: the 95% confidence interval is wider than model-1's, since <img src="https://render.githubusercontent.com/render/math?math=se(\hat\beta_1)"> increases.

* All the 95% confidence intervals contains -1 for <img src="https://render.githubusercontent.com/render/math?math=\beta_0">, and 0.5 for <img src="https://render.githubusercontent.com/render/math?math=\beta_1">. This is what we expect.

* None of the confidence intervals contains 0 for <img src="https://render.githubusercontent.com/render/math?math=\beta_1">.  


#### **Question 14**

This problem focuses on the _collinearity_ problem.

(a)

```r
set.seed(1)
x1 <- runif(100)
x2 <- 0.5 * x1 + rnorm(100)/10
y <- 2 + 2 * x1 + 0.3 * x2 + rnorm(100)
```

The linear model:

<img src="https://render.githubusercontent.com/render/math?math=y %3D \beta_0 %2B \beta_1x_1 %2B \beta_2x_2 %2B \epsilon">

The regression coefficients:

<img src="https://render.githubusercontent.com/render/math?math=\beta_0 %3D 2">

<img src="https://render.githubusercontent.com/render/math?math=\beta_1 %3D 2">

<img src="https://render.githubusercontent.com/render/math?math=\beta_2 %3D 0.3">

(b)

```r
cor(x1, x2)
```

```
## [1] 0.8351212
```

```r
ggplot() +
  geom_point(mapping = aes(x = x1, y = x2))
```

![](chapter-3-Linear-Regression_files/figure-html/14(b)-1.png)<!-- -->

The correlation between x1 and x2 is positive.

(c)

```r
lm.fit14 <- lm(y ~ x1 + x2)
summary(lm.fit14)
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8311 -0.7273 -0.0537  0.6338  2.3359 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1305     0.2319   9.188 7.61e-15 ***
## x1            1.4396     0.7212   1.996   0.0487 *  
## x2            1.0097     1.1337   0.891   0.3754    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.056 on 97 degrees of freedom
## Multiple R-squared:  0.2088,	Adjusted R-squared:  0.1925 
## F-statistic:  12.8 on 2 and 97 DF,  p-value: 1.164e-05
```

The regression coefficients estimates:

<img src="https://render.githubusercontent.com/render/math?math=\hat\beta_0 %3D 2.13 %2C \beta_0 %3D 2"> 

<img src="https://render.githubusercontent.com/render/math?math=\hat\beta_1 %3D 1.44 %2C \beta_1 %3D 2">

<img src="https://render.githubusercontent.com/render/math?math=\hat\beta_2 %3D 1.01 %2C \beta_2 %3D 0.3">

Since the t-statistic for x1 is 1.996 and the associated p-value is 0.0487 which is smaller than the 5% significance level, we reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_1 = 0">.

However, the t-statistic for x2 is 0.891 and the associated p-value is 0.3754 which is greater than the 5% significance level, so we do not reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_2 = 0">.

(d)

```r
lm.fit_x1 <- lm(y ~ x1)
summary(lm.fit_x1)
```

```
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.89495 -0.66874 -0.07785  0.59221  2.45560 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.1124     0.2307   9.155 8.27e-15 ***
## x1            1.9759     0.3963   4.986 2.66e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.055 on 98 degrees of freedom
## Multiple R-squared:  0.2024,	Adjusted R-squared:  0.1942 
## F-statistic: 24.86 on 1 and 98 DF,  p-value: 2.661e-06
```

For this scenario, the linear model is
<img src="https://render.githubusercontent.com/render/math?math=y %3D \beta_0 %2B \beta_1x_1 %2B \epsilon">

Since the p-value for x1 is very close to zero, we reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_1 = 0"> 

(e)

```r
lm.fit_x2 <- lm(y ~ x2)
summary(lm.fit_x2)
```

```
## 
## Call:
## lm(formula = y ~ x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.62687 -0.75156 -0.03598  0.72383  2.44890 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.3899     0.1949   12.26  < 2e-16 ***
## x2            2.8996     0.6330    4.58 1.37e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.072 on 98 degrees of freedom
## Multiple R-squared:  0.1763,	Adjusted R-squared:  0.1679 
## F-statistic: 20.98 on 1 and 98 DF,  p-value: 1.366e-05
```

For this scenario, the linear model is
<img src="https://render.githubusercontent.com/render/math?math=y %3D \beta_0 %2B \beta_1x_2 %2B \epsilon">

Since the p-value for x1 is very close to zero, we reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_1 = 0"> 

(f)
The results obtained in (c)-(e) are not contradict each other.

From part(b), we find that x1 and x2 are highly correlated. Because collinearity reduces the accuracy of the estimates of the regression coefficients, it leads to the standard error of the regression coefficients estimates to increase. This is the reason why x2 is not significant in the model that contains two variables in part(c), but x2 can be significant individually.

(g)

```r
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)

lm.fit14_mis <- lm(y ~ x1 + x2)
summary(lm.fit14_mis)
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.73348 -0.69318 -0.05263  0.66385  2.30619 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.2267     0.2314   9.624 7.91e-16 ***
## x1            0.5394     0.5922   0.911  0.36458    
## x2            2.5146     0.8977   2.801  0.00614 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.075 on 98 degrees of freedom
## Multiple R-squared:  0.2188,	Adjusted R-squared:  0.2029 
## F-statistic: 13.72 on 2 and 98 DF,  p-value: 5.564e-06
```

```r
mis1 <- predict(lm.fit14_mis, data.frame(x1 = 0.1, x2 = 0.8))
leverage_mis1 <- last(hatvalues(lm.fit14_mis))

lm.fit_x1_mis <- lm(y ~ x1)
summary(lm.fit_x1_mis)
```

```
## 
## Call:
## lm(formula = y ~ x1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8897 -0.6556 -0.0909  0.5682  3.5665 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.2569     0.2390   9.445 1.78e-15 ***
## x1            1.7657     0.4124   4.282 4.29e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.111 on 99 degrees of freedom
## Multiple R-squared:  0.1562,	Adjusted R-squared:  0.1477 
## F-statistic: 18.33 on 1 and 99 DF,  p-value: 4.295e-05
```

```r
mis2 <- predict(lm.fit_x1_mis, data.frame(x1 = 0.1))
leverage_mis2 <- last(hatvalues(lm.fit_x1_mis))

lm.fit_x2_mis <- lm(y ~ x2)
summary(lm.fit_x2_mis)
```

```
## 
## Call:
## lm(formula = y ~ x2)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.64729 -0.71021 -0.06899  0.72699  2.38074 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   2.3451     0.1912  12.264  < 2e-16 ***
## x2            3.1190     0.6040   5.164 1.25e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.074 on 99 degrees of freedom
## Multiple R-squared:  0.2122,	Adjusted R-squared:  0.2042 
## F-statistic: 26.66 on 1 and 99 DF,  p-value: 1.253e-06
```

```r
mis3 <- predict(lm.fit_x2_mis, data.frame(x2 = 0.8))
leverage_mis3 <- last(hatvalues(lm.fit_x2_mis))


par(mfrow = c(2, 2))
plot(predict(lm.fit14), rstudent(lm.fit14), xlab = "Fitted values", ylab = "Studentized Residuals")
plot(hatvalues(lm.fit14), ylab = "Leverage Statistics")
plot(predict(lm.fit14_mis), rstudent(lm.fit14_mis), xlab = "Fitted values with mismeasured point", ylab = "Studentized Residuals", col = ifelse(predict(lm.fit14_mis) == mis1, "red", "black"))
plot(hatvalues(lm.fit14_mis), xlab = "Index with mismeasured point", ylab = "Leverage Statistics", col = ifelse(hatvalues(lm.fit14_mis) == leverage_mis1, "red", "black"))
mtext("Model: y ~ x1 + x2", side = 3, line = -1, font = 2, col = "blue", outer = TRUE)
```

![](chapter-3-Linear-Regression_files/figure-html/14(g)-1.png)<!-- -->

```r
par(mfrow = c(2, 2))
plot(predict(lm.fit_x1), rstudent(lm.fit_x1), xlab = "Fitted values", ylab = "Studentized Residuals")
plot(hatvalues(lm.fit_x1), ylab = "Leverage Statistics")
plot(predict(lm.fit_x1_mis), rstudent(lm.fit_x1_mis), xlab = "Fitted values with mismeasured point", ylab = "Studentized Residuals", col = ifelse(predict(lm.fit_x1_mis) == mis2, "red", "black"))
plot(hatvalues(lm.fit_x1_mis), xlab = "Index with mismeasured point", ylab = "Leverage Statistics", col = ifelse(hatvalues(lm.fit_x1_mis) == leverage_mis2, "red", "black"))
mtext("Model: y ~ x1", side = 3, line = -1, font = 2, col = "blue", outer = TRUE)
```

![](chapter-3-Linear-Regression_files/figure-html/14(g)-2.png)<!-- -->

```r
par(mfrow = c(2, 2))
plot(predict(lm.fit_x2), rstudent(lm.fit_x2), xlab = "Fitted values", ylab = "Studentized Residuals")
plot(hatvalues(lm.fit_x2), ylab = "Leverage Statistics")
plot(predict(lm.fit_x2_mis), rstudent(lm.fit_x2_mis), xlab = "Fitted values with mismeasured point", ylab = "Studentized Residuals", col = ifelse(predict(lm.fit_x2_mis) == mis3, "red", "black"))
plot(hatvalues(lm.fit_x2_mis), xlab = "Index with mismeasured point", ylab = "Leverage Statistics", col = ifelse(hatvalues(lm.fit_x2_mis) == leverage_mis3, "red", "black"))
mtext("Model: y ~ x2", side = 3, line = -1, font = 2, col = "blue", outer = TRUE)
```

![](chapter-3-Linear-Regression_files/figure-html/14(g)-3.png)<!-- -->

* Model: `y ~ x1 + x2` with a mismeasured point

X2 is the only significant variable. 

Since the p-value for x1 is 0.36 which is greater than the 5% significance level, we do not reject the null hypothesis <img src="https://render.githubusercontent.com/render/math?math=H_0: \beta_1 = 0">.

However, in the model without the new observation, x1 is significant. 

Look at the corresponding studentized redisuals and leverage plots, we notice that the new observation is not a possible outlier, but it is a high-leverage point in the model (since the leverage statistic is clearly greater than the average leverage statistic 0.029703).

* Model: `y ~ x1` with a mismeasured point

x1 is significant in this model as well as in the model without the new point.

Look at the corresponding studentized redisuals and leverage plots, we notice that the new observation is a possible outlier, and it is a high-leverage point in the model (since the leverage statistic is clearly greater than the average leverage statistic 0.019802).

* Model: `y ~ x2` with a mismeasured point

x2 is significant in this model as well as in the model without the new point.

Look at the corresponding studentized redisuals and leverage plots, we notice that the new observation is not a possible outlier, but it is a high-leverage point in the model (since the leverage statistic is clearly greater than the average leverage statistic 0.019802).


#### **Question 15**
We will try to predict per capita crime rate using the **Boston** data set. 

(a)

```r
glimpse(Boston)
```

```
## Rows: 506
## Columns: 14
## $ crim    <dbl> 0.00632, 0.02731, 0.02729, 0.03237, 0.06905, 0.02985, 0.088...
## $ zn      <dbl> 18.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.5, 12.5, 12.5, 12.5, 12.5...
## $ indus   <dbl> 2.31, 7.07, 7.07, 2.18, 2.18, 2.18, 7.87, 7.87, 7.87, 7.87,...
## $ chas    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
## $ nox     <dbl> 0.538, 0.469, 0.469, 0.458, 0.458, 0.458, 0.524, 0.524, 0.5...
## $ rm      <dbl> 6.575, 6.421, 7.185, 6.998, 7.147, 6.430, 6.012, 6.172, 5.6...
## $ age     <dbl> 65.2, 78.9, 61.1, 45.8, 54.2, 58.7, 66.6, 96.1, 100.0, 85.9...
## $ dis     <dbl> 4.0900, 4.9671, 4.9671, 6.0622, 6.0622, 6.0622, 5.5605, 5.9...
## $ rad     <int> 1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4,...
## $ tax     <dbl> 296, 242, 242, 222, 222, 222, 311, 311, 311, 311, 311, 311,...
## $ ptratio <dbl> 15.3, 17.8, 17.8, 18.7, 18.7, 18.7, 15.2, 15.2, 15.2, 15.2,...
## $ black   <dbl> 396.90, 396.90, 392.83, 394.63, 396.90, 394.12, 395.60, 396...
## $ lstat   <dbl> 4.98, 9.14, 4.03, 2.94, 5.33, 5.21, 12.43, 19.15, 29.93, 17...
## $ medv    <dbl> 24.0, 21.6, 34.7, 33.4, 36.2, 28.7, 22.9, 27.1, 16.5, 18.9,...
```

```r
coefficient <- c()
se <- c()
p_value <- c()
R_squared <- c()
crim <- Boston$crim
predictors <- Boston[, 2:14]

for(p in 1:length(predictors)){
  x <- predictors[,p]
  coefficient[p] <- summary(lm(crim ~ x))$coefficients[2, 1]
  se[p] <- summary(lm(crim ~ x))$coefficients[2, 2]
  p_value[p] <- summary(lm(crim ~ x))$coefficients[2, 4]
  R_squared[p] <- summary(lm(crim ~ x))$r.sq
}

crim_reg <- data.frame(predictor = names(predictors), coefficient = round(coefficient, 2), se = round(se, 2), p_value = round(p_value, 2), R_squared = round(R_squared, 2))
crim_reg
```

```
##    predictor coefficient   se p_value R_squared
## 1         zn       -0.07 0.02    0.00      0.04
## 2      indus        0.51 0.05    0.00      0.17
## 3       chas       -1.89 1.51    0.21      0.00
## 4        nox       31.25 3.00    0.00      0.18
## 5         rm       -2.68 0.53    0.00      0.05
## 6        age        0.11 0.01    0.00      0.12
## 7        dis       -1.55 0.17    0.00      0.14
## 8        rad        0.62 0.03    0.00      0.39
## 9        tax        0.03 0.00    0.00      0.34
## 10   ptratio        1.15 0.17    0.00      0.08
## 11     black       -0.04 0.00    0.00      0.15
## 12     lstat        0.55 0.05    0.00      0.21
## 13      medv       -0.36 0.04    0.00      0.15
```

```r
g_tax <- ggplot(Boston) +
  geom_point(aes(x = tax, y = crim))
g_rad <- ggplot(Boston) +
  geom_point(aes(x = rad, y = crim))
g_dis <- ggplot(Boston) +
  geom_point(aes(x = dis, y = crim))
g_chas <- ggplot(Boston) +
  geom_point(aes(x = chas, y = crim))
ggarrange(g_tax, g_rad, g_dis, g_chas, nrow = 2, ncol = 2)
```

![](chapter-3-Linear-Regression_files/figure-html/15(a)-1.png)<!-- -->

Based on the results above, we find that **chas** is the only variable that not significant, since its p-value (0.21) is greater than the 5% significance level. Therefore, all predictors are statistically significant associated with the response **crim**, but **chas**. 

(b)

```r
lm.fit15 <- lm(crim ~., data = Boston)
summary(lm.fit15)
```

```
## 
## Call:
## lm(formula = crim ~ ., data = Boston)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.924 -2.120 -0.353  1.019 75.051 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  17.033228   7.234903   2.354 0.018949 *  
## zn            0.044855   0.018734   2.394 0.017025 *  
## indus        -0.063855   0.083407  -0.766 0.444294    
## chas         -0.749134   1.180147  -0.635 0.525867    
## nox         -10.313535   5.275536  -1.955 0.051152 .  
## rm            0.430131   0.612830   0.702 0.483089    
## age           0.001452   0.017925   0.081 0.935488    
## dis          -0.987176   0.281817  -3.503 0.000502 ***
## rad           0.588209   0.088049   6.680 6.46e-11 ***
## tax          -0.003780   0.005156  -0.733 0.463793    
## ptratio      -0.271081   0.186450  -1.454 0.146611    
## black        -0.007538   0.003673  -2.052 0.040702 *  
## lstat         0.126211   0.075725   1.667 0.096208 .  
## medv         -0.198887   0.060516  -3.287 0.001087 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.439 on 492 degrees of freedom
## Multiple R-squared:  0.454,	Adjusted R-squared:  0.4396 
## F-statistic: 31.47 on 13 and 492 DF,  p-value: < 2.2e-16
```

At the 5% significance level, we do not reject the null hypothesis for the predictors **indis**, **chas**, **nox**, **rm**, **age**, **tax**, **ptratio**, and **lstat** since the associated p-values are greater than the significance level. 

And, we reject the null hypothsis for the predictors **zn**, **dis**, **rad**, **black**, and **medv** since the associated p-values are smaller than the significance level. 
(c)
Significant predictors in (b) are less than that in (a).


```r
crim_reg <- data.frame(predictor = names(predictors), coefficient = coefficient, se = se, p_value = p_value, R_squared = R_squared)

coef_a <- crim_reg$coefficient
b <- summary(lm.fit15)$coefficients
coef_b <- b[2:14,1]

df <- data.frame(coef_a, coef_b)
ggplot(df) +
  geom_point(aes(x = coef_a, y = coef_b)) +
  labs(x = "univariate reg coefs",
       y = "multiple reg coefs",
       title = "uni vs multi reggression coefficients"
       )
```

![](chapter-3-Linear-Regression_files/figure-html/15(c)-1.png)<!-- -->

(d)

To answer this quesion, for each predictor X, fit a model of the form 

<img src="https://render.githubusercontent.com/render/math?math=Y %3D \beta_0 %2B \beta_1X %2B \beta_2X^2 %2B \beta_3X^3 %2B \epsilon">


```r
plot(lm.fit15, which = c(1, 1))
```

![](chapter-3-Linear-Regression_files/figure-html/15(d)-1.png)<!-- -->

```r
anova_F <- c()
anova_p <- c()
crim <- Boston$crim
Boston_sub <- Boston[, 2:14]
predictors <- colnames(Boston[, 2:14])

for(p in 1:length(predictors)){
  x <- Boston_sub[, p]
  if (predictors[p] == "chas"){
    F_value = NA}
  else{
  m1 <- lm(crim ~ x)
  m2 <- lm(crim ~ poly(x, 3))
  anova_F[p] <- anova(m1, m2)$F[2]
  anova_p[p] <- anova(m1, m2)$'Pr(>F)'[2]
  }
  }

m1_vs_m2 <- data.frame(predictor = names(Boston_sub), F_value = anova_F, p_value = round(anova_p, 3) )
m1_vs_m2 %>%
  arrange(desc(F_value))
```

```
##    predictor     F_value p_value
## 1       medv 116.6340058   0.000
## 2        dis  46.4603654   0.000
## 3        nox  42.7581707   0.000
## 4      indus  31.9869602   0.000
## 5        age  15.1400633   0.000
## 6        tax  11.6400227   0.000
## 7    ptratio   8.4155300   0.000
## 8         rm   5.3088168   0.005
## 9         zn   4.8118205   0.009
## 10       rad   3.6732699   0.026
## 11     lstat   3.3190437   0.037
## 12     black   0.4622222   0.630
## 13      chas          NA      NA
```

```r
lm.medv <- lm(crim ~ medv, data = Boston)
lm.medv_non_linear <- lm(crim ~ poly(medv, 3), data = Boston)
par(mfrow = c(1, 2))
plot(lm.medv, which = c(1, 1))
plot(lm.medv_non_linear, which = c(1, 1))
mtext("Residual Plot for Linear Fit         Residual Plot for Polynomial Fit", side = 3, line = -1, font = 2, col = "blue", outer = TRUE )
mtext("    predictor: medv", side = 3, line = -17, font = 2,col = "orange", outer = TRUE)
```

![](chapter-3-Linear-Regression_files/figure-html/15(d)-2.png)<!-- -->

```r
plot( crim ~ medv, data=Boston )
lines( Boston$medv, predict( lm.medv ), col='red', type='p' )
lines( Boston$medv, predict( lm.medv_non_linear ), col='green', type='p' )

ggplot(Boston, aes(x = medv, y = crim)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x + I(x^2) + I(x^3)", se = FALSE, aes( color = "Degree 3")) + 
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, aes(color = "Linear")) +
  scale_colour_manual(name = "", values =  c("orange", "purple")) +
  theme(legend.position = c(0.9,0.9), legend.direction = "vertical") +
  labs(title = "The Linear Regression Fit") 
```

![](chapter-3-Linear-Regression_files/figure-html/15(d)-3.png)<!-- -->

We can see a U-shape pattern in the residual plot from the linear regression of **crim** onto all the other predictors. So, a polynomial regression model may fit the data set better. 

The `anova()` function performs a hypothesis test comparing the two models. So, let's use this function to test whether a polynomial model is superior or not.

Here, the F-statistic for **medv** is the highest value (approx.117) among the predictors, and the associated p-value is virtually zero. This provides clear evidence that the model containing the quadratic and cubic terms is far better to the simple linear regression model. 

Look at the residual plots from the linear regression of **crim** onto **medv**, we notice that the residuals exhibit little pattern in the polynomial regression model, suggesting the quadratic and cubic terms improve the fit to the data.

Finally, let's look at the linear regression fit plot, it is clear evidence that the polynomial regression model with degree 3 is fit better than the simple linear regression model. 


