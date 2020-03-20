setwd("/Users/marie1227/Desktop/R")
OnlineShoppers <- read.table("/Users/marie1227/Desktop/R/online_shoppers_intention.csv",
                              sep=",", header=TRUE)
summary(OnlineShoppers)
#Administrative   Administrative_Duration Informational     Informational_Duration
#Min.   : 0.000   Min.   :   0.00         Min.   : 0.0000   Min.   :   0.00       
#1st Qu.: 0.000   1st Qu.:   0.00         1st Qu.: 0.0000   1st Qu.:   0.00       
#Median : 1.000   Median :   7.50         Median : 0.0000   Median :   0.00       
#Mean   : 2.315   Mean   :  80.82         Mean   : 0.5036   Mean   :  34.47       
#3rd Qu.: 4.000   3rd Qu.:  93.26         3rd Qu.: 0.0000   3rd Qu.:   0.00       
#Max.   :27.000   Max.   :3398.75         Max.   :24.0000   Max.   :2549.38       
#
#ProductRelated   ProductRelated_Duration  BounceRates         ExitRates      
#Min.   :  0.00   Min.   :    0.0         Min.   :0.000000   Min.   :0.00000  
#1st Qu.:  7.00   1st Qu.:  184.1         1st Qu.:0.000000   1st Qu.:0.01429  
#Median : 18.00   Median :  598.9         Median :0.003112   Median :0.02516  
#Mean   : 31.73   Mean   : 1194.8         Mean   :0.022191   Mean   :0.04307  
#3rd Qu.: 38.00   3rd Qu.: 1464.2         3rd Qu.:0.016813   3rd Qu.:0.05000  
#Max.   :705.00   Max.   :63973.5         Max.   :0.200000   Max.   :0.20000  
#
#PageValues        SpecialDay          Month      OperatingSystems    Browser      
#Min.   :  0.000   Min.   :0.00000   May    :3364   Min.   :1.000    Min.   : 1.000  
#1st Qu.:  0.000   1st Qu.:0.00000   Nov    :2998   1st Qu.:2.000    1st Qu.: 2.000  
#Median :  0.000   Median :0.00000   Mar    :1907   Median :2.000    Median : 2.000  
#Mean   :  5.889   Mean   :0.06143   Dec    :1727   Mean   :2.124    Mean   : 2.357  
#3rd Qu.:  0.000   3rd Qu.:0.00000   Oct    : 549   3rd Qu.:3.000    3rd Qu.: 2.000  
#Max.   :361.764   Max.   :1.00000   Sep    : 448   Max.   :8.000    Max.   :13.000  
#(Other):1337                                    
#Region       TrafficType               VisitorType     Weekend         Revenue       
#Min.   :1.000   Min.   : 1.00   New_Visitor      : 1694   Mode :logical   Mode :logical  
#1st Qu.:1.000   1st Qu.: 2.00   Other            :   85   FALSE:9462      FALSE:10422    
#Median :3.000   Median : 2.00   Returning_Visitor:10551   TRUE :2868      TRUE :1908     
#Mean   :3.147   Mean   : 4.07                                                            
#3rd Qu.:4.000   3rd Qu.: 4.00                                                            
#Max.   :9.000   Max.   :20.00                                            
#There are no missing values(NAs) in the data set


library(ggplot2)
#The distribution of numberical features
ggplot(OnlineShoppers, aes(Administrative)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(Administrative_Duration)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(Informational)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(Informational_Duration)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(ProductRelated)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(ProductRelated_Duration)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(BounceRates)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(ExitRates)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(PageValues)) + geom_histogram(bins=30)
ggplot(OnlineShoppers, aes(SpecialDay)) + geom_histogram(bins=30)

#The barplots of categorical features
ggplot(OnlineShoppers) + geom_bar(aes(Month))
ggplot(OnlineShoppers) + geom_bar(aes(VisitorType))
ggplot(OnlineShoppers) + geom_bar(aes(Weekend))
ggplot(OnlineShoppers) + geom_bar(aes(Revenue))


#Extract the observations with "Others" in Visitor Type
library(sqldf)
OnShop <- sqldf("SELECT 
Administrative as Admin, Administrative_Duration as Admin_Dur, Informational as Info,
Informational_Duration as Info_Dur, ProductRelated as PdRel,
ProductRelated_Duration as PdRel_Dur, BounceRates as BounceR, ExitRates as ExitR, PageValues,
SpecialDay, Month, VisitorType, Weekend, Revenue 
                FROM OnlineShoppers WHERE VisitorType != 'Other'")
#Since we are not going to analyze Operating Systems, Browser, Region, and Traffic Type,
#so the variables mentioned above are excluded in the new data frame

#We select columns where VisitorType=New_Visitor and Returning_Visitor in our new data frame, "OnShop";
#however, "Other" is still one of the levels in VisitorType. So, we are going to drop the unused level.
levels(OnShop$VisitorType)
#[1] "New_Visitor"       "Other"             "Returning_Visitor"
OnShop$VisitorType <- droplevels(OnShop$VisitorType)
levels(OnShop$VisitorType)
#[1] "New_Visitor"       "Returning_Visitor"

#Convert seconds to minutes
OnShop$Admin_Dur <- OnShop$Admin_Dur/60
OnShop$Info_Dur <- OnShop$Info_Dur/60
OnShop$PdRel_Dur <- OnShop$PdRel_Dur/60
#Convert rate to percentage
OnShop$BounceR <- OnShop$BounceR*100
OnShop$ExitR <- OnShop$ExitR*100


#Explore the relationship between each numerical predictor and response variable by scatterplots and boxplots
pairs(OnShop)
#We can use pairs() to see the scatterplots; however, the graphs are too small so we use scatter plot function in ggplot2.

#Scatterplots
ggplot(OnShop, aes(x=Admin, y=Revenue)) + geom_point() + labs(x = "Administrative")
ggplot(OnShop, aes(x=Admin_Dur, y=Revenue)) + geom_point() + labs(x = "Administrative Duration")
ggplot(OnShop, aes(x=Info, y=Revenue)) + geom_point() + labs(x = "Informational")
ggplot(OnShop, aes(x=Info_Dur, y=Revenue)) + geom_point() + labs(x = "Informational Duration")
ggplot(OnShop, aes(x=PdRel, y=Revenue)) + geom_point() + labs(x = "Product Related")
ggplot(OnShop, aes(x=PdRel_Dur, y=Revenue)) + geom_point() + labs(x = "Product Related Duration")
ggplot(OnShop, aes(x=BounceR, y=Revenue)) + geom_point() + labs(x = "Bounce Rate")
ggplot(OnShop, aes(x=ExitR, y=Revenue)) + geom_point() + labs(x = "Exit Rate")
ggplot(OnShop, aes(x=PageValues, y=Revenue)) + geom_point() + labs(x = "Page Values")
ggplot(OnShop, aes(x=SpecialDay, y=Revenue)) + geom_point() + labs(x = "Special Day")

#Boxplots
ggplot(OnShop, aes(x=Revenue, y=Admin, fill=Revenue)) + geom_boxplot() + labs(y = "Administrative") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=Admin_Dur, fill=Revenue)) + geom_boxplot() + labs(y = "Administrative Duration") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=Info, fill=Revenue)) + geom_boxplot() + labs(y = "Informational") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=Info_Dur, fill=Revenue)) + geom_boxplot() + labs(y = "Informational Duration") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=PdRel, fill=Revenue)) + geom_boxplot() + labs(y = "Product Related") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=PdRel_Dur, fill=Revenue)) + geom_boxplot() + labs(y = "Product Related Duration") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=BounceR, fill=Revenue)) + geom_boxplot() + labs(y = "Bounce Rate") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=ExitR, fill=Revenue)) + geom_boxplot() + labs(y = "Exit Rate") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=PageValues, fill=Revenue)) + geom_boxplot() + labs(y = "Page Values") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))
ggplot(OnShop, aes(x=Revenue, y=SpecialDay, fill=Revenue)) + geom_boxplot() + labs(y = "Special Day") + scale_fill_manual(values=c("#92C5DE", "#F4A582"))


#Transfer Revenue value into 0 or 1 since y value in ggplot(logistic regression plot) must be 0~1
OnShop1 <- OnShop #create a new frame
OnShop1$Revenue1 <- ifelse(OnShop$Revenue == 'TRUE', 1, 0) 


#------------------------------------------------------------------------
#Logisitc regression for single numerical predictor vs. response variable
#------------------------------------------------------------------------

#Logistic regression for Administrative
glmAdmin <- glm(Revenue1 ~ Admin, data=OnShop1, family=binomial)
summary(glmAdmin)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.4643  -0.5873  -0.5132  -0.5132   2.0457    
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.960687   0.032149  -60.99   <2e-16 ***
#Admin        0.096803   0.006508   14.87   <2e-16 ***
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10334  on 12243  degrees of freedom
#AIC: 10338
#Number of Fisher Scoring iterations: 4
#For every one unit change in Administrative, the log odds of Revenue=1 increases by 0.097.


#Logistic regression for Administrative Duration
glmAdminDur <- glm(Revenue1 ~ Admin_Dur, data=OnShop1, family=binomial)
summary(glmAdminDur)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.0527  -0.5708  -0.5525  -0.5525   1.9774    
#Coefficients:
#                  Estimate Std. Error  z value  Pr(>|z|)    
#(Intercept)      -1.802430   0.027778  -64.888    <2e-16 ***
#Admin_Dur         0.066721   0.006998    9.534    <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10455  on 12243  degrees of freedom
#AIC: 10459
#Number of Fisher Scoring iterations: 4
#For every one unit change in Administrative Duration, the log odds of Revenue=1 increases by 0.067.


#Logistic regression for Informational
glmInfo <- glm(Revenue1 ~ Info, data=OnShop1, family=binomial)
summary(glmInfo)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.1373  -0.5535  -0.5535  -0.5535   1.9758   
#Coefficients:
#                Estimate Std. Error  z value  Pr(>|z|)    
#(Intercept)     -1.79872    0.02752   -65.36    <2e-16 ***
#Info             0.16564    0.01646    10.06    <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10447  on 12243  degrees of freedom
#AIC: 10451
#Number of Fisher Scoring iterations: 4
#For every one unit change in Informational, the log odds of Revenue=1 increases by 0.165.


#Logistic regression for Informational Duration
glmInfoDur <- glm(Revenue1 ~ Info_Dur, data=OnShop1, family=binomial)
summary(glmInfoDur)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-1.5577  -0.5682  -0.5682  -0.5682   1.9511 
#Coefficients:
#                Estimate Std. Error  z value  Pr(>|z|)    
#(Intercept)    -1.742083   0.025935  -67.172   < 2e-16 ***
#Info_Dur        0.061251   0.008419    7.275  3.46e-13 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10494  on 12243  degrees of freedom
#AIC: 10498
#Number of Fisher Scoring iterations: 4
#For every one unit change in Informational Duration, the log odds of Revenue=1 increases by 0.061.


#Logistic regression for Product Related 
glmPdRel <- glm(Revenue1 ~ PdRel, data=OnShop1, family=binomial)
summary(glmPdRel)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-2.5720  -0.5668  -0.5327  -0.5163   2.0498  
#Coefficients:
#                 Estimate Std. Error  z value  Pr(>|z|)    
#(Intercept)    -1.9703068  0.0316728   -62.21    <2e-16 ***
#PdRel           0.0074335  0.0004801    15.48    <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10298  on 12243  degrees of freedom
#AIC: 10302
#Number of Fisher Scoring iterations: 4
#For every one unit change in Product Related, the log odds of Revenue=1 increases by 0.007.


#Logistic regression for Product Related Duration
glmPdRelDur <- glm(Revenue1 ~ PdRel_Dur, data=OnShop1, family=binomial)
summary(glmPdRelDur)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-4.3569  -0.5673  -0.5322  -0.5185   2.0391  
#Coefficients:
#                 Estimate Std. Error   z value   Pr(>|z|)    
#(Intercept)    -1.9453888  0.0310355    -62.68     <2e-16 ***
#PdRel_Dur       0.0107264  0.0007182     14.94     <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10310  on 12243  degrees of freedom
#AIC: 10314
#Number of Fisher Scoring iterations: 4
#For every one unit change in Product Related Duration, the log odds of Revenue=1 increases by 0.0107.


#Logistic regression for Bounce Rates
glmBounceR <- glm(Revenue1 ~ BounceR, data=OnShop1, family=binomial)
summary(glmBounceR)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.6712  -0.6712  -0.5940  -0.2565   3.9613 
#Coefficients:
#                 Estimate Std. Error   z value   Pr(>|z|)    
#(Intercept)      -1.37581    0.02854    -48.21     <2e-16 ***
#BounceR          -0.32348    0.02338    -13.84     <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10012  on 12243  degrees of freedom
#AIC: 10016
#Number of Fisher Scoring iterations: 7
#For every one unit change in Bounce Rates, the log odds of Revenue=1 decreases by 0.323.


#Logistic regression for Exit Rates
glmExitR <- glm(Revenue1 ~ ExitR, data=OnShop1, family=binomial)
summary(glmExitR)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.8855  -0.6798  -0.5170  -0.1724   3.9167  
#Coefficients:
#                 Estimate Std. Error   z value   Pr(>|z|)    
#(Intercept)      -0.73399    0.04243    -17.30     <2e-16 ***
#ExitR            -0.34680    0.01617    -21.45     <2e-16 ***
#    Null deviance: 10541.9  on 12244  degrees of freedom
#Residual deviance: 9600.6  on 12243  degrees of freedom
#AIC: 9604.6
#Number of Fisher Scoring iterations: 6
#For every one unit change in Exit Rates, the log odds of Revenue=1 decreases by 0.347.


#Logistic regression for Page Values
glmPgValues <- glm(Revenue1 ~ PageValues, data=OnShop1, family=binomial)
summary(glmPgValues)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-6.2582  -0.4139  -0.4139  -0.4139   2.2361 
#Coefficients:
#                Estimate Std. Error  z value  Pr(>|z|)    
#(Intercept)    -2.414464   0.034510   -69.96    <2e-16 ***
#PageValues      0.089143   0.002352    37.90    <2e-16 ***
#    Null deviance: 10541.9  on 12244  degrees of freedom
#Residual deviance: 7850.6  on 12243  degrees of freedom
#AIC: 7854.6
#Number of Fisher Scoring iterations: 5
#For every one unit change in Page Values, the log odds of Revenue=1 increases by 0.089.


#Logistic regression for Special Day
glmSpec <- glm(Revenue1 ~ SpecialDay, data=OnShop1, family=binomial)
summary(glmSpec)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.5989  -0.5989  -0.5989  -0.5103   2.6034 
#Coefficients:
#                Estimate Std. Error   z value   Pr(>|z|)    
#(Intercept)     -1.62735    0.02562   -63.511     <2e-16 ***
#SpecialDay      -1.72719    0.20039    -8.619     <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10435  on 12243  degrees of freedom
#AIC: 10439
#Number of Fisher Scoring iterations: 5
#For every one unit change in Special Day, the log odds of Revenue=1 decreases by 1.727.


#------------------------------------------------------------------------
#Chi-squre test for single categorical predictor vs. response variable
#------------------------------------------------------------------------

#two-way contingency table of categorical outcome and predictor(month)
xtabs(~Revenue1 + Month, data = OnShop1)
#Revenue1  Aug  Dec  Feb  Jul June  Mar  May  Nov  Oct  Sep
#       0  357 1462  181  366  258 1715 2999 2219  434  362
#       1   76  203    3   66   29  192  365  757  115   86
tmonth <- table(OnShop1$Revenue1, OnShop$Month)
chisq.test(tmonth)
#Pearson's Chi-squared test
#X-squared = 389.66, df = 9, p-value < 2.2e-16

glmMonth <- glm(Revenue1 ~ Month, data=OnShop1, family=binomial)
summary(glmMonth)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.7662  -0.6213  -0.4793  -0.4607   2.8693 
#Coefficients:
#                Estimate Std. Error z value   Pr(>|z|)    
#(Intercept)      -1.5470     0.1263 -12.246    < 2e-16 ***
#MonthDec         -0.4274     0.1469  -2.910   0.003616 ** 
#MonthFeb         -2.5529     0.5955  -4.287   1.81e-05 ***
#MonthJul         -0.1660     0.1840  -0.902   0.366940    
#MonthJune        -0.6387     0.2331  -2.740   0.006138 ** 
#MonthMar         -0.6427     0.1475  -4.358   1.31e-05 ***
#MonthMay         -0.5591     0.1380  -4.053   5.06e-05 ***
#MonthNov          0.4716     0.1332   3.541   0.000398 ***
#MonthOct          0.2189     0.1642   1.333   0.182486    
#MonthSep          0.1097     0.1742   0.630   0.528872    
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10158  on 12235  degrees of freedom
#AIC: 10178
#Number of Fisher Scoring iterations: 6
#December, versus August, changes the log odds of Revenue=1 by -0.427.
#June, versus August, changes the log odds of Revenue=1 by -0.638.

#two-way contingency table of categorical outcome and predictor(Visitor Type)
xtabs(~Revenue1 + VisitorType, data = OnShop1)
#Revenue1  New_Visitor  Returning_Visitor
#       0        1272                9081
#       1         422                1470
tvisitor <- table(OnShop1$Revenue1, OnShop$VisitorType)
chisq.test(tvisitor)
#Pearson's Chi-squared test with Yates' continuity correction
#X-squared = 133.84, df = 1, p-value < 2.2e-16
glmVisitorTy <- glm(Revenue1 ~ VisitorType, data=OnShop1, family=binomial)
summary(glmVisitorTy)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.7570  -0.5478  -0.5478  -0.5478   1.9854  
#Coefficients:
#                                  Estimate  Std. Error  z value  Pr(>|z|)    
#(Intercept)                       -1.10334     0.05618   -19.64    <2e-16 ***
#VisitorTypeReturning_Visitor      -0.71758     0.06282   -11.42    <2e-16 ***
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10421  on 12243  degrees of freedom
#AIC: 10425
#Number of Fisher Scoring iterations: 4
#Returning visitor, versus new visitor, changes the log odds of Revenue=1 by -0.717.

#two-way contingency table of categorical outcome and predictor(Visitor Type)
xtabs(~Revenue1 + Weekend, data = OnShop1)
#Revenue1 FALSE TRUE
#       0  7990 2363
#       1  1394  498
tweek <- table(OnShop1$Revenue1, OnShop$Weekend)
chisq.test(tweek)
#Pearson's Chi-squared test with Yates' continuity correction
#X-squared = 10.731, df = 1, p-value = 0.001053
glmWeekend <- glm(Revenue1 ~ Weekend, data=OnShop1, family=binomial)
summary(glmWeekend)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-0.6185  -0.5671  -0.5671  -0.5671   1.9529   
#Coefficients:
#             Estimate Std.     Error  z value  Pr(>|z|)    
#(Intercept)      -1.74601    0.02903  -60.153   < 2e-16 ***
#WeekendTRUE       0.18893    0.05722    3.302   0.00096 ** 
#    Null deviance: 10542  on 12244  degrees of freedom
#Residual deviance: 10531  on 12243  degrees of freedom
#AIC: 10535
#Number of Fisher Scoring iterations: 4
#Weekend, versus weekday, changes the log odds of Revenue=1 by 0.189.



############################################################
# Machine Learning
############################################################
#Split the train set and test set
n <- nrow(OnShop1)                #number of observations=392
ntrain <- round(n*0.6)          #60% for training set
set.seed(12345)                     #set seed for reproducible results
tindex <- sample(n, ntrain)     #create an index
train_OnShop1 <- OnShop1[tindex,]   #create training set
test_OnShop1 <- OnShop1[-tindex,]   #create test set


#---------------------
# Logistic Regression
#---------------------
#First, we use all predictors in the model
formulaLR1 <- Revenue1 ~ Admin + Admin_Dur + Info + Info_Dur + PdRel + PdRel_Dur +
  BounceR + ExitR + PageValues + SpecialDay + Month + VisitorType + Weekend
glmOnShop1 <- glm(formulaLR1, data=train_OnShop1, family="binomial")
summary(glmOnShop1)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-5.3480  -0.4701  -0.3389  -0.1699   3.4198  
#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -1.9728671  0.2427002  -8.129 4.33e-16 ***
#Admin                         0.0043925  0.0145619   0.302  0.76292    
#Admin_Dur                    -0.0051657  0.0149630  -0.345  0.72992    
#Info                          0.0591965  0.0346805   1.707  0.08784 .  
#Info_Dur                     -0.0171747  0.0170241  -1.009  0.31305    
#PdRel                        -0.0002256  0.0016148  -0.140  0.88889    
#PdRel_Dur                     0.0068956  0.0024014   2.871  0.00409 ** 
#BounceR                      -0.0491826  0.0414355  -1.187  0.23524    
#ExitR                        -0.1415917  0.0305279  -4.638 3.52e-06 ***
#PageValues                    0.0858299  0.0031985  26.834  < 2e-16 ***
#SpecialDay                   -0.1485088  0.3121915  -0.476  0.63429    
#MonthDec                     -0.3716158  0.2480719  -1.498  0.13413    
#MonthFeb                     -1.0436695  0.6803777  -1.534  0.12504    
#MonthJul                      0.2347731  0.2955459   0.794  0.42698    
#MonthJune                     0.1951902  0.3445806   0.566  0.57108    
#MonthMar                     -0.2477442  0.2438618  -1.016  0.30967    
#MonthMay                     -0.4037243  0.2377170  -1.698  0.08944 .  
#MonthNov                      0.6509257  0.2248981   2.894  0.00380 ** 
#MonthOct                      0.2280177  0.2719879   0.838  0.40184    
#MonthSep                      0.2474398  0.2778638   0.891  0.37319    
#VisitorTypeReturning_Visitor -0.2841421  0.1118217  -2.541  0.01105 *  
#WeekendTRUE                   0.2109702  0.0895392   2.356  0.01846 *   
#Null deviance: 6405.4  on 7346  degrees of freedom
#Residual deviance: 4290.0  on 7325  degrees of freedom
#AIC: 4334
#Number of Fisher Scoring iterations: 7

#Let's predict the probabilities by using all the predictors
predlr1 <- predict(glmOnShop1, test_OnShop1, type="response")  #make predictions for the test set
pred_valueslr1 <- round(predlr1)
table(pred_valueslr1, test_OnShop1$Revenue1)
misclass_error_ratelr1 <- sum(test_OnShop1$Revenue1 != pred_valueslr1) / nrow(test_OnShop1)*100
#The misclassification error rate is 11.37%.

#From the logistic regression results, it can be noticed that some variables-Admin, PdRel, BounceR,
#SpecialDay are not statistically significant. Keeping them in the model may contribute to overfitting. 
#We will use the p-values to determine if the predictors are appropriate to be included in the final model.

#New Logistic Regression model after eliminating non-significant predictors
formulaLR2 <- Revenue1 ~ PdRel_Dur + ExitR + PageValues + Month + VisitorType + Weekend
glmOnShop2 <- glm(formulaLR2, data=train_OnShop1, family="binomial")
summary(glmOnShop2)
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-5.3400  -0.4677  -0.3393  -0.1679   3.2802  
#Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -1.929104   0.237361  -8.127 4.39e-16 ***
#PdRel_Dur                     0.006924   0.001125   6.152 7.63e-10 ***
#ExitR                        -0.169652   0.020744  -8.179 2.87e-16 ***
#PageValues                    0.085918   0.003170  27.106  < 2e-16 ***
#MonthDec                     -0.361624   0.247843  -1.459  0.14454    
#MonthFeb                     -1.091546   0.676611  -1.613  0.10669    
#MonthJul                      0.228245   0.295952   0.771  0.44058    
#MonthJune                     0.216825   0.344880   0.629  0.52955    
#MonthMar                     -0.231315   0.243175  -0.951  0.34149    
#MonthMay                     -0.416136   0.232740  -1.788  0.07378 .  
#MonthNov                      0.658004   0.224974   2.925  0.00345 ** 
#MonthOct                      0.230185   0.272246   0.846  0.39783    
#MonthSep                      0.249910   0.278085   0.899  0.36882    
#VisitorTypeReturning_Visitor -0.283314   0.111229  -2.547  0.01086 *  
#WeekendTRUE                   0.212838   0.089455   2.379  0.01735 *  
#Null deviance: 6405.4  on 7346  degrees of freedom
#Residual deviance: 4294.9  on 7332  degrees of freedom
#AIC: 4324.9
#Number of Fisher Scoring iterations: 6

#Let's predict the probabilities with new model
predlr2 <- predict(glmOnShop2, test_OnShop1, type="response")  #make predictions for the test set
pred_valueslr2 <- round(predlr2)
table(pred_valueslr2, test_OnShop1$Revenue1)
#pred_valueslr2    0    1
#             0 4054  450
#             1  111  283
misclass_error_ratelr2 <- sum(test_OnShop1$Revenue1 != pred_valueslr2) / nrow(test_OnShop1)*100
#The misclassification error rate is 11.45%.


#---------------------
#Random Forests
#---------------------
install.packages("randomForest")
library(randomForest)

#Train randomForest to predict Species using all predictors
rf <- randomForest(Revenue1 ~ Admin + Admin_Dur + Info + Info_Dur + PdRel + PdRel_Dur +
                  BounceR + ExitR + PageValues + SpecialDay + Month + VisitorType + Weekend, 
                  data=train_OnShop1, ntree=500, mtry=2, importance=TRUE)
predictionrf <- predict(rf, newdata=test_OnShop1, type="class")
predrf_values <- round(predictionrf)
table(predrf_values, test_OnShop1$Revenue1)
#predrf_values    0    1
#            0 4038  328
#            1  127  405
rfmisclass_error_rate <- sum(test_OnShop1$Revenue1 != predrf_values) / nrow(test_OnShop1)*100
#The misclassification error rate is 9.29%
I <- importance(rf)
I <- I[order(I[,2], decreasing=T),]
#              %IncMSE IncNodePurity
#PageValues  91.465678    324.495442
#PdRel_Dur   26.001122     74.681953
#ExitR       25.195344     71.205553
#PdRel       26.989451     60.030982
#Month       23.387631     55.209833
#Admin_Dur   24.915822     50.646922
#BounceR     26.687636     47.180624
#Admin       23.761413     37.246078
#Info_Dur    14.410882     25.689160
#Info        14.061272     17.738401
#VisitorType 16.900466     11.630860
#Weekend      6.806637      8.769775
#SpecialDay   2.301986      4.320227


#------------------------------
# Support Vector Machine(SVM)
#------------------------------
library(e1071)
svm1 <- svm(Revenue1 ~ Admin + Admin_Dur + Info + Info_Dur + PdRel + PdRel_Dur +
            BounceR + ExitR + PageValues + SpecialDay + Month + VisitorType + Weekend,
            data=train_OnShop1, type="C-classification",
            kernal="radial", gamma=0.1, cost=10)
summary(svm1)
#Parameters:
#    SVM-Type:  C-classification  
#  SVM-Kernel:  radial 
#        cost:  10 
#Number of Support Vectors:  1949
#( 1118 831 )
#Number of Classes:  2 
#Levels: 
#  0 1

#Predict the result and Calculate the misclassification error rate
predictionsvm <- predict(svm1, test_OnShop1)
xtabsvm <- table(test_OnShop1$Revenue1, predictionsvm)
xtabsvm
#predictionsvm
#     0    1
#0 3999  166
#1  329  404
svmmisclass_error_rate <- sum(test_OnShop1$Revenue1 != predictionsvm) / nrow(test_OnShop1)*100
#The misclassification error rate is 10.106%.


