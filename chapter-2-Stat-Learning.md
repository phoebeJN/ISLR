---
title: "Chapter 2 Stat Learing"
author: "JN"
date: "18/10/2020"
output: 
  html_document:
    keep_md: true
---



## Exercises 
### Applied Q8, Q9, Q10

#### Question 8
(a) Read the data into R.

```r
glimpse(College)
```

```
## Rows: 777
## Columns: 18
## $ Private     <fct> Yes, Yes, Yes, Yes, Yes, Yes, Yes, Yes, Yes, Yes, Yes, ...
## $ Apps        <dbl> 1660, 2186, 1428, 417, 193, 587, 353, 1899, 1038, 582, ...
## $ Accept      <dbl> 1232, 1924, 1097, 349, 146, 479, 340, 1720, 839, 498, 1...
## $ Enroll      <dbl> 721, 512, 336, 137, 55, 158, 103, 489, 227, 172, 472, 4...
## $ Top10perc   <dbl> 23, 16, 22, 60, 16, 38, 17, 37, 30, 21, 37, 44, 38, 44,...
## $ Top25perc   <dbl> 52, 29, 50, 89, 44, 62, 45, 68, 63, 44, 75, 77, 64, 73,...
## $ F.Undergrad <dbl> 2885, 2683, 1036, 510, 249, 678, 416, 1594, 973, 799, 1...
## $ P.Undergrad <dbl> 537, 1227, 99, 63, 869, 41, 230, 32, 306, 78, 110, 44, ...
## $ Outstate    <dbl> 7440, 12280, 11250, 12960, 7560, 13500, 13290, 13868, 1...
## $ Room.Board  <dbl> 3300, 6450, 3750, 5450, 4120, 3335, 5720, 4826, 4400, 3...
## $ Books       <dbl> 450, 750, 400, 450, 800, 500, 500, 450, 300, 660, 500, ...
## $ Personal    <dbl> 2200, 1500, 1165, 875, 1500, 675, 1500, 850, 500, 1800,...
## $ PhD         <dbl> 70, 29, 53, 92, 76, 67, 90, 89, 79, 40, 82, 73, 60, 79,...
## $ Terminal    <dbl> 78, 30, 66, 97, 72, 73, 93, 100, 84, 41, 88, 91, 84, 87...
## $ S.F.Ratio   <dbl> 18.1, 12.2, 12.9, 7.7, 11.9, 9.4, 11.5, 13.7, 11.3, 11....
## $ perc.alumni <dbl> 12, 16, 30, 37, 2, 11, 26, 37, 23, 15, 31, 41, 21, 32, ...
## $ Expend      <dbl> 7041, 10527, 8735, 19016, 10922, 9727, 8861, 11487, 116...
## $ Grad.Rate   <dbl> 60, 56, 54, 59, 15, 55, 63, 73, 80, 52, 73, 76, 74, 68,...
```
(b) Look at the data using the `fix()` function.

Note: The first column in the data set from package ISLR already satisfies the requirement which is the first data column is `Private`. 

(c)

i. Use the `summary()` function.

```r
summary(College)
```

```
##  Private        Apps           Accept          Enroll       Top10perc    
##  No :212   Min.   :   81   Min.   :   72   Min.   :  35   Min.   : 1.00  
##  Yes:565   1st Qu.:  776   1st Qu.:  604   1st Qu.: 242   1st Qu.:15.00  
##            Median : 1558   Median : 1110   Median : 434   Median :23.00  
##            Mean   : 3002   Mean   : 2019   Mean   : 780   Mean   :27.56  
##            3rd Qu.: 3624   3rd Qu.: 2424   3rd Qu.: 902   3rd Qu.:35.00  
##            Max.   :48094   Max.   :26330   Max.   :6392   Max.   :96.00  
##    Top25perc      F.Undergrad     P.Undergrad         Outstate    
##  Min.   :  9.0   Min.   :  139   Min.   :    1.0   Min.   : 2340  
##  1st Qu.: 41.0   1st Qu.:  992   1st Qu.:   95.0   1st Qu.: 7320  
##  Median : 54.0   Median : 1707   Median :  353.0   Median : 9990  
##  Mean   : 55.8   Mean   : 3700   Mean   :  855.3   Mean   :10441  
##  3rd Qu.: 69.0   3rd Qu.: 4005   3rd Qu.:  967.0   3rd Qu.:12925  
##  Max.   :100.0   Max.   :31643   Max.   :21836.0   Max.   :21700  
##    Room.Board       Books           Personal         PhD        
##  Min.   :1780   Min.   :  96.0   Min.   : 250   Min.   :  8.00  
##  1st Qu.:3597   1st Qu.: 470.0   1st Qu.: 850   1st Qu.: 62.00  
##  Median :4200   Median : 500.0   Median :1200   Median : 75.00  
##  Mean   :4358   Mean   : 549.4   Mean   :1341   Mean   : 72.66  
##  3rd Qu.:5050   3rd Qu.: 600.0   3rd Qu.:1700   3rd Qu.: 85.00  
##  Max.   :8124   Max.   :2340.0   Max.   :6800   Max.   :103.00  
##     Terminal       S.F.Ratio      perc.alumni        Expend     
##  Min.   : 24.0   Min.   : 2.50   Min.   : 0.00   Min.   : 3186  
##  1st Qu.: 71.0   1st Qu.:11.50   1st Qu.:13.00   1st Qu.: 6751  
##  Median : 82.0   Median :13.60   Median :21.00   Median : 8377  
##  Mean   : 79.7   Mean   :14.09   Mean   :22.74   Mean   : 9660  
##  3rd Qu.: 92.0   3rd Qu.:16.50   3rd Qu.:31.00   3rd Qu.:10830  
##  Max.   :100.0   Max.   :39.80   Max.   :64.00   Max.   :56233  
##    Grad.Rate     
##  Min.   : 10.00  
##  1st Qu.: 53.00  
##  Median : 65.00  
##  Mean   : 65.46  
##  3rd Qu.: 78.00  
##  Max.   :118.00
```

ii. Use the `pairs()` function to produce a scatter plot.

```r
pairs(College[,1:10]) # the first ten variables (cols)
```

![](chapter-2-Stat-Learning_files/figure-html/8(c)(ii)-1.png)<!-- -->

iii. Plot side-by-side boxplots of `Outstate` versus `Private`.

```r
ggplot(College, aes(x = Private, y = Outstate)) +
  geom_boxplot()
```

![](chapter-2-Stat-Learning_files/figure-html/8(c)(iii)-1.png)<!-- -->

iv. Create a new qualitative variable, called `Elite`. Then, use the `summary()` and plot side-by-side boxplots of `Outstate` versus `Elite`.

```r
Elite <- rep("No", nrow(College)) # replicates the values in x.
Elite[College$Top10perc > 50] <- "Yes" 
Elite <- as.factor(Elite) # factors with 2 levels: Yes, No.
College <- data.frame(College, Elite) # add one col, Elite.

summary(College$Elite)
```

```
##  No Yes 
## 699  78
```

```r
ggplot(College, aes(x = Elite, y = Outstate)) +
  geom_boxplot()
```

![](chapter-2-Stat-Learning_files/figure-html/8(c)(iv)-1.png)<!-- -->

v. Produce some histograms with differing numbers of bins for a few of the **quantitative** variables.

```r
binwidth_Apps <- round((max(College$Apps) - min(College$Apps))/ceiling(sqrt(nrow(College))), 0)
binwidth_Accept <- round((max(College$Accept) - min(College$Accept))/ceiling(sqrt(nrow(College))), 0)
binwidth_Expend <-  round((max(College$Expend) - min(College$Expend))/ceiling(sqrt(nrow(College))), 0)
binwidth_Grad.Rate <-  round((max(College$Grad.Rate) - min(College$Grad.Rate))/ceiling(sqrt(nrow(College))), 0)

Apps <- ggplot(College) +
  geom_histogram(mapping = aes(x = Apps), 
                 binwidth = binwidth_Apps )
# To examine the distribution of a continuous variable, Apps.
College %>%
  count(cut_width(Apps, binwidth_Apps)) # compute by hand
```

```
##    cut_width(Apps, binwidth_Apps)   n
## 1                      [-858,858] 225
## 2                  (858,2.57e+03] 287
## 3             (2.57e+03,4.29e+03] 102
## 4                (4.29e+03,6e+03]  53
## 5                (6e+03,7.72e+03]  34
## 6             (7.72e+03,9.43e+03]  27
## 7             (9.43e+03,1.11e+04]  11
## 8             (1.11e+04,1.29e+04]  12
## 9             (1.29e+04,1.46e+04]  10
## 10            (1.46e+04,1.63e+04]   8
## 11             (1.63e+04,1.8e+04]   1
## 12             (1.8e+04,1.97e+04]   3
## 13            (1.97e+04,2.14e+04]   2
## 14            (2.14e+04,2.32e+04]   1
## 15            (4.72e+04,4.89e+04]   1
```

```r
Accept <- ggplot(College) +
  geom_histogram(mapping = aes(x = Accept), binwidth = binwidth_Accept)
College %>%
  count(cut_width(Accept, binwidth_Accept))
```

```
##    cut_width(Accept, binwidth_Accept)   n
## 1                          [-469,469] 134
## 2                      (469,1.41e+03] 312
## 3                 (1.41e+03,2.34e+03] 134
## 4                 (2.34e+03,3.28e+03]  63
## 5                 (3.28e+03,4.22e+03]  40
## 6                 (4.22e+03,5.16e+03]  21
## 7                  (5.16e+03,6.1e+03]  25
## 8                  (6.1e+03,7.04e+03]  11
## 9                 (7.04e+03,7.97e+03]  12
## 10                (7.97e+03,8.91e+03]   6
## 11                (8.91e+03,9.85e+03]   3
## 12                (9.85e+03,1.08e+04]   6
## 13                (1.08e+04,1.17e+04]   3
## 14                (1.17e+04,1.27e+04]   1
## 15                (1.27e+04,1.36e+04]   3
## 16                (1.45e+04,1.55e+04]   1
## 17                (1.83e+04,1.92e+04]   1
## 18                (2.58e+04,2.67e+04]   1
```

```r
Expend <- ggplot(College) +
  geom_histogram(mapping = aes(x = Expend), binwidth = binwidth_Expend)
College %>%
  count(cut_width(Expend, binwidth_Expend))
```

```
##    cut_width(Expend, binwidth_Expend)   n
## 1                 [2.84e+03,4.74e+03]  36
## 2                 (4.74e+03,6.63e+03] 142
## 3                 (6.63e+03,8.53e+03] 222
## 4                 (8.53e+03,1.04e+04] 166
## 5                 (1.04e+04,1.23e+04]  75
## 6                 (1.23e+04,1.42e+04]  43
## 7                 (1.42e+04,1.61e+04]  33
## 8                  (1.61e+04,1.8e+04]  21
## 9                  (1.8e+04,1.99e+04]  11
## 10                (1.99e+04,2.18e+04]   7
## 11                (2.18e+04,2.37e+04]   3
## 12                (2.37e+04,2.56e+04]   2
## 13                (2.56e+04,2.75e+04]   4
## 14                (2.75e+04,2.94e+04]   2
## 15                (2.94e+04,3.13e+04]   2
## 16                (3.32e+04,3.51e+04]   1
## 17                 (3.51e+04,3.7e+04]   1
## 18                 (3.7e+04,3.88e+04]   1
## 19                (3.88e+04,4.07e+04]   1
## 20                (4.07e+04,4.26e+04]   1
## 21                (4.26e+04,4.45e+04]   1
## 22                (4.45e+04,4.64e+04]   1
## 23                (5.59e+04,5.78e+04]   1
```

```r
Grad.Rate <- ggplot(College) +
  geom_histogram(mapping = aes(x = Grad.Rate), binwidth = binwidth_Grad.Rate)
College %>%
  count(cut_width(Grad.Rate, binwidth_Grad.Rate))
```

```
##    cut_width(Grad.Rate, binwidth_Grad.Rate)  n
## 1                                   [10,14]  1
## 2                                   (14,18]  3
## 3                                   (18,22]  4
## 4                                   (22,26]  4
## 5                                   (26,30]  4
## 6                                   (30,34] 12
## 7                                   (34,38] 20
## 8                                   (38,42] 17
## 9                                   (42,46] 37
## 10                                  (46,50] 44
## 11                                  (50,54] 67
## 12                                  (54,58] 61
## 13                                  (58,62] 54
## 14                                  (62,66] 79
## 15                                  (66,70] 70
## 16                                  (70,74] 61
## 17                                  (74,78] 53
## 18                                  (78,82] 48
## 19                                  (82,86] 44
## 20                                  (86,90] 30
## 21                                  (90,94] 25
## 22                                  (94,98] 23
## 23                                 (98,102] 15
## 24                                (114,118]  1
```

```r
ggarrange(Apps, Accept, Expend, Grad.Rate,
          ncol = 2, nrow = 2)
```

![](chapter-2-Stat-Learning_files/figure-html/8(c)(v)-1.png)<!-- -->

vi. Provide a brief summary of what you discover.

```r
College[which.max(College$Top10perc),]
```

```
##                                       Private Apps Accept Enroll Top10perc
## Massachusetts Institute of Technology     Yes 6411   2140   1078        96
##                                       Top25perc F.Undergrad P.Undergrad
## Massachusetts Institute of Technology        99        4481          28
##                                       Outstate Room.Board Books Personal PhD
## Massachusetts Institute of Technology    20100       5975   725     1600  99
##                                       Terminal S.F.Ratio perc.alumni Expend
## Massachusetts Institute of Technology       99      10.1          35  33541
##                                       Grad.Rate Elite
## Massachusetts Institute of Technology        94   Yes
```

```r
acceptance_rate = College$Accept / College$Apps
College[which.min(acceptance_rate),]
```

```
##                      Private  Apps Accept Enroll Top10perc Top25perc
## Princeton University     Yes 13218   2042   1153        90        98
##                      F.Undergrad P.Undergrad Outstate Room.Board Books Personal
## Princeton University        4540         146    19900       5910   675     1575
##                      PhD Terminal S.F.Ratio perc.alumni Expend Grad.Rate Elite
## Princeton University  91       96       8.4          54  28320        99   Yes
```

```r
College[which.max(acceptance_rate),]
```

```
##                          Private Apps Accept Enroll Top10perc Top25perc
## Emporia State University      No 1256   1256    853        43        79
##                          F.Undergrad P.Undergrad Outstate Room.Board Books
## Emporia State University        3957         588     5401       3144   450
##                          Personal PhD Terminal S.F.Ratio perc.alumni Expend
## Emporia State University     1888  72       75      19.3           4   5527
##                          Grad.Rate Elite
## Emporia State University        50    No
```

```r
College[which.min(College$Grad.Rate),]
```

```
##                           Private Apps Accept Enroll Top10perc Top25perc
## Texas Southern University      No 4345   3245   2604        15        85
##                           F.Undergrad P.Undergrad Outstate Room.Board Books
## Texas Southern University        5584        3101     7860       3360   600
##                           Personal PhD Terminal S.F.Ratio perc.alumni Expend
## Texas Southern University     1700  65       75      18.2          21   3605
##                           Grad.Rate Elite
## Texas Southern University        10    No
```

```r
College[which.max(College$Grad.Rate),]
```

```
##                   Private Apps Accept Enroll Top10perc Top25perc F.Undergrad
## Cazenovia College     Yes 3847   3433    527         9        35        1010
##                   P.Undergrad Outstate Room.Board Books Personal PhD Terminal
## Cazenovia College          12     9384       4840   600      500  22       47
##                   S.F.Ratio perc.alumni Expend Grad.Rate Elite
## Cazenovia College      14.3          20   7697       118    No
```

Since we are usually interested in acceptance rate, graduation rate of universities, and which school is more attractive to top students, we can find answers from the `College` data set.

Based on the results above, we notice that Princeton University has the lowest acceptance rate. And, Texas Southern University has the lowest graduation rate. Also, we find out that 96% of new students from top 10% of high school class attend to Massachusetts Institute of Technology (MIT). 

#### Question 9
(a)

```r
glimpse(Auto)
```

```
## Rows: 392
## Columns: 9
## $ mpg          <dbl> 18, 15, 18, 16, 17, 15, 14, 14, 14, 15, 15, 14, 15, 14...
## $ cylinders    <dbl> 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 4, 6, 6, 6, ...
## $ displacement <dbl> 307, 350, 318, 304, 302, 429, 454, 440, 455, 390, 383,...
## $ horsepower   <dbl> 130, 165, 150, 150, 140, 198, 220, 215, 225, 190, 170,...
## $ weight       <dbl> 3504, 3693, 3436, 3433, 3449, 4341, 4354, 4312, 4425, ...
## $ acceleration <dbl> 12.0, 11.5, 11.0, 12.0, 10.5, 10.0, 9.0, 8.5, 10.0, 8....
## $ year         <dbl> 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70...
## $ origin       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, ...
## $ name         <fct> chevrolet chevelle malibu, buick skylark 320, plymouth...
```

Quantitative predictors: `mpg`, `displacement`, `horsepower`, `weight`, `acceleration`, and `year`.

qualitative predictors: `cylinders`, `origin`, `name`

(b) Use the `range()` function.

```r
Auto <- na.omit(Auto)
dim(Auto)
```

```
## [1] 392   9
```

```r
summary(Auto)
```

```
##       mpg          cylinders      displacement     horsepower        weight    
##  Min.   : 9.00   Min.   :3.000   Min.   : 68.0   Min.   : 46.0   Min.   :1613  
##  1st Qu.:17.00   1st Qu.:4.000   1st Qu.:105.0   1st Qu.: 75.0   1st Qu.:2225  
##  Median :22.75   Median :4.000   Median :151.0   Median : 93.5   Median :2804  
##  Mean   :23.45   Mean   :5.472   Mean   :194.4   Mean   :104.5   Mean   :2978  
##  3rd Qu.:29.00   3rd Qu.:8.000   3rd Qu.:275.8   3rd Qu.:126.0   3rd Qu.:3615  
##  Max.   :46.60   Max.   :8.000   Max.   :455.0   Max.   :230.0   Max.   :5140  
##                                                                                
##   acceleration        year           origin                      name    
##  Min.   : 8.00   Min.   :70.00   Min.   :1.000   amc matador       :  5  
##  1st Qu.:13.78   1st Qu.:73.00   1st Qu.:1.000   ford pinto        :  5  
##  Median :15.50   Median :76.00   Median :1.000   toyota corolla    :  5  
##  Mean   :15.54   Mean   :75.98   Mean   :1.577   amc gremlin       :  4  
##  3rd Qu.:17.02   3rd Qu.:79.00   3rd Qu.:2.000   amc hornet        :  4  
##  Max.   :24.80   Max.   :82.00   Max.   :3.000   chevrolet chevette:  4  
##                                                  (Other)           :365
```

```r
qualitative_cols <- c(2, 8, 9)
sapply(Auto[, -qualitative_cols], range)
```

```
##       mpg displacement horsepower weight acceleration year
## [1,]  9.0           68         46   1613          8.0   70
## [2,] 46.6          455        230   5140         24.8   82
```

(c)

```r
sapply(Auto[, -qualitative_cols], mean)
```

```
##          mpg displacement   horsepower       weight acceleration         year 
##     23.44592    194.41199    104.46939   2977.58418     15.54133     75.97959
```

```r
sapply(Auto[, -qualitative_cols], sd)
```

```
##          mpg displacement   horsepower       weight acceleration         year 
##     7.805007   104.644004    38.491160   849.402560     2.758864     3.683737
```

(d)

```r
sapply(Auto[-seq(10,85), -qualitative_cols], range)
```

```
##       mpg displacement horsepower weight acceleration year
## [1,] 11.0           68         46   1649          8.5   70
## [2,] 46.6          455        230   4997         24.8   82
```

```r
sapply(Auto[-seq(10,85), -qualitative_cols], mean)
```

```
##          mpg displacement   horsepower       weight acceleration         year 
##     24.40443    187.24051    100.72152   2935.97152     15.72690     77.14557
```

```r
sapply(Auto[-seq(10,85), -qualitative_cols], sd)
```

```
##          mpg displacement   horsepower       weight acceleration         year 
##     7.867283    99.678367    35.708853   811.300208     2.693721     3.106217
```

(e)

```r
ggplot(Auto, aes(x = mpg, y = weight)) +
  geom_point()
```

![](chapter-2-Stat-Learning_files/figure-html/9(e)-1.png)<!-- -->

```r
ggplot(Auto, aes(x = mpg, y = horsepower)) +
  geom_point()
```

![](chapter-2-Stat-Learning_files/figure-html/9(e)-2.png)<!-- -->

```r
ggplot(Auto, aes(x = horsepower, y = acceleration)) +
  geom_point()
```

![](chapter-2-Stat-Learning_files/figure-html/9(e)-3.png)<!-- -->

(f)

```r
pairs(Auto[,-qualitative_cols])
```

![](chapter-2-Stat-Learning_files/figure-html/9(f)-1.png)<!-- -->

```r
ggplot(Auto, aes(x = as.factor(cylinders), y = mpg)) +
  geom_point() +
  labs(x = "Cylinders")
```

![](chapter-2-Stat-Learning_files/figure-html/9(f)-2.png)<!-- -->

```r
ggplot(Auto, aes(x = as.factor(origin), y = mpg)) +
  geom_point() +
  labs(x = "Origin")
```

![](chapter-2-Stat-Learning_files/figure-html/9(f)-3.png)<!-- -->

```r
ggplot(Auto, aes(x = as.factor(name), y = mpg)) +
  geom_point() +
  labs(x = "name")
```

![](chapter-2-Stat-Learning_files/figure-html/9(f)-4.png)<!-- -->

All predictors have some correlations with mpg but `name`. The name-mpg scatter plot above shows no relation between them (random pattern). 

#### Question 10
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
?Boston
```

```
## starting httpd help server ... done
```

```r
summary(Boston)
```

```
##       crim                zn             indus            chas        
##  Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000  
##  1st Qu.: 0.08205   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000  
##  Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000  
##  Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917  
##  3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000  
##  Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000  
##       nox               rm             age              dis        
##  Min.   :0.3850   Min.   :3.561   Min.   :  2.90   Min.   : 1.130  
##  1st Qu.:0.4490   1st Qu.:5.886   1st Qu.: 45.02   1st Qu.: 2.100  
##  Median :0.5380   Median :6.208   Median : 77.50   Median : 3.207  
##  Mean   :0.5547   Mean   :6.285   Mean   : 68.57   Mean   : 3.795  
##  3rd Qu.:0.6240   3rd Qu.:6.623   3rd Qu.: 94.08   3rd Qu.: 5.188  
##  Max.   :0.8710   Max.   :8.780   Max.   :100.00   Max.   :12.127  
##       rad              tax           ptratio          black       
##  Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32  
##  1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38  
##  Median : 5.000   Median :330.0   Median :19.05   Median :391.44  
##  Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67  
##  3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23  
##  Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90  
##      lstat            medv      
##  Min.   : 1.73   Min.   : 5.00  
##  1st Qu.: 6.95   1st Qu.:17.02  
##  Median :11.36   Median :21.20  
##  Mean   :12.65   Mean   :22.53  
##  3rd Qu.:16.95   3rd Qu.:25.00  
##  Max.   :37.97   Max.   :50.00
```

Rows: 506 observations housing values in suburbs of Boston

Columns: 14 variables/features

(b)

```r
library(GGally)
```

```
## Warning: package 'GGally' was built under R version 4.0.3
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

```r
ggpairs(Boston[, 1:6])
```

![](chapter-2-Stat-Learning_files/figure-html/10(b)-1.png)<!-- -->

(c)

```r
all_correlations = round(cor(Boston), 2)
print(all_correlations[,1])
```

```
##    crim      zn   indus    chas     nox      rm     age     dis     rad     tax 
##    1.00   -0.20    0.41   -0.06    0.42   -0.22    0.35   -0.38    0.63    0.58 
## ptratio   black   lstat    medv 
##    0.29   -0.39    0.46   -0.39
```

```r
rad_vs_crim <- ggplot(Boston, aes(x = rad, y = crim)) +
  geom_point()
tax_vs_crim <- ggplot(Boston, aes(x = tax, y = crim)) +
  geom_point()
ggarrange(rad_vs_crim, tax_vs_crim)
```

![](chapter-2-Stat-Learning_files/figure-html/10(c)-1.png)<!-- -->

(d)

```r
binwidth_crim <- round((max(Boston$crim) - min(Boston$crim))/ceiling(sqrt(nrow(Boston))), 0)
binwidth_tax <- round((max(Boston$tax) - min(Boston$tax))/ceiling(sqrt(nrow(Boston))), 0)
binwidth_ptratio <- round((max(Boston$ptratio) - min(Boston$ptratio))/ceiling(sqrt(nrow(Boston))), 1)


crim <- ggplot(Boston) +
  geom_histogram(mapping = aes(x = crim), binwidth = binwidth_crim)
tax <- ggplot(Boston) +
  geom_histogram(mapping = aes(x = tax), binwidth = binwidth_tax)
ptratio <- ggplot(Boston) +
  geom_histogram(mapping = aes(x = ptratio), binwidth = binwidth_ptratio )
ggarrange(crim, tax, ptratio,
         ncol = 2, nrow = 2)
```

![](chapter-2-Stat-Learning_files/figure-html/10(d)-1.png)<!-- -->

```r
group <- c(1, 10, 11)
sapply(Boston[, group], range)
```

```
##          crim tax ptratio
## [1,]  0.00632 187    12.6
## [2,] 88.97620 711    22.0
```

(e)

```r
Boston %>%
  filter(chas == 1) %>%
  count() 
```

```
##    n
## 1 35
```

There are 35 suburbs bound the Charles river.

(f)

```r
median(Boston$ptratio)
```

```
## [1] 19.05
```

(g)

```r
Boston %>%
  filter(medv == min(medv))
```

```
##      crim zn indus chas   nox    rm age    dis rad tax ptratio  black lstat
## 1 38.3518  0  18.1    0 0.693 5.453 100 1.4896  24 666    20.2 396.90 30.59
## 2 67.9208  0  18.1    0 0.693 5.683 100 1.4254  24 666    20.2 384.97 22.98
##   medv
## 1    5
## 2    5
```

There are 2 suburbs has the lowest median value of owner-occupied homes. The values of the other predictors for these suburbs are shown above. Compare to the overall ranges, we find out that the value of `age` and `rad` of the two suburbs both reach the maximum values.

(h)

```r
Boston %>%
  filter(rm > 7) %>%
  count()
```

```
##    n
## 1 64
```

```r
Boston_8 <- Boston %>%
  filter(rm > 8) 

Boston_8 %>%
  count()
```

```
##    n
## 1 13
```

```r
summary(Boston_8)
```

```
##       crim               zn            indus             chas       
##  Min.   :0.02009   Min.   : 0.00   Min.   : 2.680   Min.   :0.0000  
##  1st Qu.:0.33147   1st Qu.: 0.00   1st Qu.: 3.970   1st Qu.:0.0000  
##  Median :0.52014   Median : 0.00   Median : 6.200   Median :0.0000  
##  Mean   :0.71879   Mean   :13.62   Mean   : 7.078   Mean   :0.1538  
##  3rd Qu.:0.57834   3rd Qu.:20.00   3rd Qu.: 6.200   3rd Qu.:0.0000  
##  Max.   :3.47428   Max.   :95.00   Max.   :19.580   Max.   :1.0000  
##       nox               rm             age             dis       
##  Min.   :0.4161   Min.   :8.034   Min.   : 8.40   Min.   :1.801  
##  1st Qu.:0.5040   1st Qu.:8.247   1st Qu.:70.40   1st Qu.:2.288  
##  Median :0.5070   Median :8.297   Median :78.30   Median :2.894  
##  Mean   :0.5392   Mean   :8.349   Mean   :71.54   Mean   :3.430  
##  3rd Qu.:0.6050   3rd Qu.:8.398   3rd Qu.:86.50   3rd Qu.:3.652  
##  Max.   :0.7180   Max.   :8.780   Max.   :93.90   Max.   :8.907  
##       rad              tax           ptratio          black      
##  Min.   : 2.000   Min.   :224.0   Min.   :13.00   Min.   :354.6  
##  1st Qu.: 5.000   1st Qu.:264.0   1st Qu.:14.70   1st Qu.:384.5  
##  Median : 7.000   Median :307.0   Median :17.40   Median :386.9  
##  Mean   : 7.462   Mean   :325.1   Mean   :16.36   Mean   :385.2  
##  3rd Qu.: 8.000   3rd Qu.:307.0   3rd Qu.:17.40   3rd Qu.:389.7  
##  Max.   :24.000   Max.   :666.0   Max.   :20.20   Max.   :396.9  
##      lstat           medv     
##  Min.   :2.47   Min.   :21.9  
##  1st Qu.:3.32   1st Qu.:41.7  
##  Median :4.14   Median :48.3  
##  Mean   :4.31   Mean   :44.2  
##  3rd Qu.:5.12   3rd Qu.:50.0  
##  Max.   :7.44   Max.   :50.0
```

These suburbs has lower crime rate (`crim`) and less lower status of the population (`lstat`).
