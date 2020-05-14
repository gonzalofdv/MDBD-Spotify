

> dataGrupos5<-dataCopy2
> View(dataCopy2)
> dataGrupos5<-dataCopy
> View(dataGrupos5)
> dataGrupos5$group <- cut(dataGrupos5$popularity, c(-1, 20, 40, 60 ,80 ,100), labels = 
+                        c('group0','group1','group2','group3','group4'))
> View(dataGrupos5)
> formulaG5EL<-dataGrupos5$group ~ dataGrupos5$energy + dataGrupos5$loudness
> g5EL.glm <- glm(formulaG5EL, data = dataGrupos5)
> summary(g5EL.glm)

Call:
glm(formula = formulaG5EL, data = dataGrupos5)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.0227  -0.6282   0.2443   0.4494   3.0219  

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)           3.2797088  0.0142052  230.88   <2e-16 ***
dataGrupos5$energy   -0.3451095  0.0147611  -23.38   <2e-16 ***
dataGrupos5$loudness  0.0617160  0.0006527   94.56   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 0.6305153)

    Null deviance: 96445  on 133948  degrees of freedom
Residual deviance: 84455  on 133946  degrees of freedom
AIC: 318356

Number of Fisher Scoring iterations: 2

//Hacemos plot de la gráfica nueva.
> plot(dataGrupos5$group, pred, xlab="Observed Values", ylab="Predicted Values")
> abline(a=0, b=1)
//Ver foto: Regresión5GruposEL.png


//Evaluación de esta iteración (5 grupos, energy y loudness)
> lmg5EL<-lm(dataGrupos5$group ~ dataGrupos5$energy + dataGrupos5$loudness)
> summary(lmg5EL)

Call:
lm(formula = dataGrupos5$group ~ dataGrupos5$energy + dataGrupos5$loudness)

Residuals:
    Min      1Q  Median      3Q     Max 
-2.0227 -0.6282  0.2443  0.4494  3.0219 

Coefficients:
                       Estimate Std. Error t value Pr(>|t|)    
(Intercept)           3.2797088  0.0142052  230.88   <2e-16 ***
dataGrupos5$energy   -0.3451095  0.0147611  -23.38   <2e-16 ***
dataGrupos5$loudness  0.0617160  0.0006527   94.56   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.794 on 133946 degrees of freedom
Multiple R-squared:  0.1243,	Adjusted R-squared:  0.1243 
F-statistic:  9508 on 2 and 133946 DF,  p-value: < 2.2e-16

//Errores y coefficients
> lmg5EL$coefficients
         (Intercept)   dataGrupos5$energy dataGrupos5$loudness 
          3.27970877          -0.34510949           0.06171598 
> ECMg5EL <- sum((residuals(lmg5EL)^2))/length(residuals(lmg5EL))
> EMAg5EL <- sqrt(ECMg5EL);
> EMRg5EL <- EMAg5EL / mean(dataGrupos5$group);
> ECMg5EL
[1] 0.6305012
> EMAg5EL
[1] 0.7940411
> EMRg5EL
[1] 0.3176634
