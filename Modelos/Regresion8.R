datag5AI$group <- cut(datag5AI$popularity, c(-1, 20, 40, 60 ,80 ,100), labels =c('group0','group1','group2','group3','group4'))
datag5AI$group <- as.numeric(datag5AI$group)
g5AI.glm <- glm(formulag5AI, data = datag5AI)
summary(g5AI.glm)

Call:
glm(formula = formulag5AI, data = datag5AI)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.7789  -0.6876   0.2230   0.4616   2.9346  

Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)                2.778949   0.003109  893.78   <2e-16 ***
datag5AI$acousticness     -0.667477   0.006537 -102.10   <2e-16 ***
datag5AI$instrumentalness -0.332964   0.007949  -41.89   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.6416746)

    Null deviance: 96445  on 133948  degrees of freedom
Residual deviance: 85950  on 133946  degrees of freedom
AIC: 320706

Number of Fisher Scoring iterations: 2

 g5AI.glm$coefficients
              (Intercept)     datag5AI$acousticness 
                2.7789495                -0.6674767 
datag5AI$instrumentalness 
               -0.3329641 
 lmg5AI<-lm(datag5AI$group ~ datag5AI$acousticness + datag5AI$instrumentalness)
 summary(lmg5AI)

Call:
lm(formula = datag5AI$group ~ datag5AI$acousticness + datag5AI$instrumentalness)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.7789 -0.6876  0.2230  0.4616  2.9346 

Coefficients:
                           Estimate Std. Error t value
(Intercept)                2.778949   0.003109  893.78
datag5AI$acousticness     -0.667477   0.006537 -102.10
datag5AI$instrumentalness -0.332964   0.007949  -41.89
                          Pr(>|t|)    
(Intercept)                 <2e-16 ***
datag5AI$acousticness       <2e-16 ***
datag5AI$instrumentalness   <2e-16 ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.801 on 133946 degrees of freedom
Multiple R-squared:  0.1088,	Adjusted R-squared:  0.1088 
F-statistic:  8178 on 2 and 133946 DF,  p-value: < 2.2e-16


 ECMg5AI <- sum((residuals(lmg5AI)^2))/length(residuals(lmg5AI))
 EMAg5AI <- sqrt(ECMg5AI);
 EMRg5AI <- EMAg5AI / mean(datag5AI$group);
 ECMg5AI
[1] 0.6416602
 EMAg5AI
[1] 0.801037
 EMRg5AI
[1] 0.3204622
