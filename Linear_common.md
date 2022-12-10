Прочитаем данные в виде таблицы
```{r}
data <- read.table('mortality.txt',             
                   header = TRUE) 
```
Так как факторов много, чтобы не идти вслепую, посмотрим сразу, \
присутствует ли в датасете мультиколлинеарность через корреляционную матрицу
```{r}
library(corrplot)
corr<-cor(data)
corrplot(corr)
```
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Rplot.png)

Как видим, переменные HC ~ NOX коррелируют друг с другом очень сильно
Также можно отметить такие переменные, как: nonw ~ poor, educ ~ wwdrk, nonw ~ mortality

Итак, мы будем объяснять уровень смертности через следующие ожидаемые переменные: 
annual_precipitation (prec), popn(the number of members per household)
the size of the nonwhite population (nonw)
the number of families with an income less than $3000; (poor)
и отдельно загрязнения (hc, noc, so2) '''

Возьмем данные целиком, чтобы понять, с чем мы имеем дело в цифрах и построим регрессию

```{r}
model1 <- lm(mort~., data=data)
summary(model1)
```
```{r}
Call:
lm(formula = mort ~ ., data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-75.285 -14.640   0.694  14.790  75.586 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.863e+03  4.108e+02   4.535  4.4e-05 ***
prec         2.072e+00  8.418e-01   2.462  0.01781 *  
jant        -2.178e+00  6.752e-01  -3.225  0.00238 ** 
jult        -2.834e+00  1.771e+00  -1.600  0.11670    
ovr65       -1.404e+01  7.746e+00  -1.813  0.07670 .  
popn        -1.154e+02  6.200e+01  -1.862  0.06933 .  
educ        -2.425e+01  1.121e+01  -2.163  0.03605 *  
hous        -1.146e+00  1.467e+00  -0.781  0.43871    
dens         1.004e-02  4.123e-03   2.435  0.01899 *  
nonw         3.533e+00  1.282e+00   2.755  0.00850 ** 
wwdrk        5.229e-01  1.551e+00   0.337  0.73760    
poor         2.671e-01  2.565e+00   0.104  0.91755    
hc          -8.890e-01  4.524e-01  -1.965  0.05574 .  
nox          1.866e+00  9.345e-01   1.997  0.05201 .  
so2         -3.447e-02  1.423e-01  -0.242  0.80968    
humid        5.331e-01  1.052e+00   0.507  0.61474    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 32.33 on 44 degrees of freedom
Multiple R-squared:  0.7985,	Adjusted R-squared:  0.7298 
F-statistic: 11.63 on 15 and 44 DF,  p-value: 9.56e-11
```
