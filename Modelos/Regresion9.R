datasgAI<-dataCopy
formulaAI<-dataCopy$popularity ~ dataCopy$acousticness + dataCopy$instrumentalness glmAI.glm <- glm(formulaAI, data = dataCopy)
summary(glmAI.glm)

Call:
glm(formula = formulaAI, data = dataCopy)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-45.816  -10.642    1.434   10.833   62.625  

Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)                45.81707    0.06246  733.53   <2e-16 ***
dataCopy$acousticness     -14.60533    0.13133 -111.21   <2e-16 ***
dataCopy$instrumentalness  -6.14939    0.15969  -38.51   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 258.9617)

    Null deviance: 39401652  on 133948  degrees of freedom
Residual deviance: 34686887  on 133946  degrees of freedom
AIC: 1124448

Number of Fisher Scoring iterations: 2


pred<- predict(glmAI.glm, type = "response")
plot(dataCopy$popularity, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)
glmAI.glm$coefficients
              (Intercept)     dataCopy$acousticness dataCopy$instrumentalness 
                45.817066                -14.605327                 -6.149392 
 lmAI<-lm(dataCopy$popularity ~ dataCopy$acousticness + dataCopy$instrumentalness)

summary(lmAI)

Call:
lm(formula = dataCopy$popularity ~ dataCopy$acousticness + dataCopy$instrumentalness)

Residuals:
    Min      1Q  Median      3Q     Max 
-45.816 -10.642   1.434  10.833  62.625 

Coefficients:
                           Estimate Std. Error t value Pr(>|t|)    
(Intercept)                45.81707    0.06246  733.53   <2e-16 ***
dataCopy$acousticness     -14.60533    0.13133 -111.21   <2e-16 ***
dataCopy$instrumentalness  -6.14939    0.15969  -38.51   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 16.09 on 133946 degrees of freedom
Multiple R-squared:  0.1197,	Adjusted R-squared:  0.1196 
F-statistic:  9103 on 2 and 133946 DF,  p-value: < 2.2e-16

ECMAI <- sum((residuals(lmAI)^2))/length(residuals(lmAI))
EMAAI <- sqrt(ECMAI);
EMRAI <- EMAAI / mean(dataCopy$popularity);
ECMAI
[1] 258.9559
EMAAI
[1] 16.09211
EMRAI
[1] 0.4036794
