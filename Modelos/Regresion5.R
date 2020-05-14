> formulag5AD<-datag5AD$group ~ datag5AD$acousticness + datag5AD$danceability
> g5AD.glm <- glm(formulag5AD, data = datag5AD)
> summary(g5AD.glm)

Call:
glm(formula = formulag5AD, data = datag5AD)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.9582  -0.6665   0.1799   0.4586   2.9505  

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)            2.371702   0.008793  269.72   <2e-16 ***
datag5AD$acousticness -0.627859   0.006737  -93.19   <2e-16 ***
datag5AD$danceability  0.608134   0.012837   47.37   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.6393676)

    Null deviance: 96445  on 133948  degrees of freedom
Residual deviance: 85641  on 133946  degrees of freedom
AIC: 320224

Number of Fisher Scoring iterations: 2

//Hacemos la representacion de esta iteración:
//Imagen:

> g5AD.glm$coefficients
          (Intercept) datag5AD$acousticness 
            2.3717017            -0.6278590 
datag5AD$danceability 
            0.6081335 
> lmg5AD<-lm(datag5AD$group ~ datag5AD$acousticness + datag5AD$danceability)
> summary(lmg5AD)

Call:
lm(formula = datag5AD$group ~ datag5AD$acousticness + datag5AD$danceability)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.9582 -0.6665  0.1799  0.4586  2.9505 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)            2.371702   0.008793  269.72   <2e-16 ***
datag5AD$acousticness -0.627859   0.006737  -93.19   <2e-16 ***
datag5AD$danceability  0.608134   0.012837   47.37   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.7996 on 133946 degrees of freedom
Multiple R-squared:  0.112,	Adjusted R-squared:  0.112 
F-statistic:  8449 on 2 and 133946 DF,  p-value: < 2.2e-16

> lmg5AD$coefficients
          (Intercept) datag5AD$acousticness datag5AD$danceability 
            2.3717017            -0.6278590             0.6081335 
> ECMg5AD <- sum((residuals(lmg5AD)^2))/length(residuals(lmg5AD))
> EMAg5AD <- sqrt(ECMg5AD);
> EMRg5AD <- EMAg5AD / mean(datag5AD$group);
> ECMg5AD
[1] 0.6393533
> EMAg5AD
[1] 0.7995957
> EMRg5AD
[1] 0.3198856
