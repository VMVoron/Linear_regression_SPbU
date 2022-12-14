
Итак, возьмем модель с двумя факторами - nonw и educ
```{r}

data <- read.table('mortality.txt',             
                   header = TRUE)       

lm.model<-lm(formula = mort ~ nonw + educ, data = data)
lm.model$coefficients
summary(lm.model)

```

```{r}
(Intercept)        nonw        educ 
1211.856232    3.916516  -28.979335 
> summary(lm.model)

Call:
lm(formula = mort ~ nonw + educ, data = data)

Residuals:
     Min       1Q   Median       3Q      Max 
-103.857  -28.671    1.775   27.397   90.692 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1211.8562    74.4179  16.284  < 2e-16 ***
nonw           3.9165     0.6245   6.271 5.16e-08 ***
educ         -28.9793     6.5904  -4.397 4.85e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 41.85 on 57 degrees of freedom
Multiple R-squared:  0.5628,	Adjusted R-squared:  0.5474 
F-statistic: 36.68 on 2 and 57 DF,  p-value: 5.761e-11

```
Распределение частоты факторов и корреляция между ними
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/psych.png)

Также корреляции можно посмотреть с помощью ызова функции cor

```{r}
> round(cor(data2), 2)
     nonw mort
nonw 1.00 0.64
mort 0.64 1.00
> round(cor(data2, method="spearman"), 2)
     nonw mort
nonw 1.00 0.61
mort 0.61 1.00
```
Посмотрим на остатки
```{r}
n = 60
P = 2
a <- min(std_resid)+4/(n-P-1)
b <- max(std_resid)-4/(n-P-1)
library(stats)

std_resid <- rstandard(lm.model)
cooks_D  <- cooks.distance(lm.model)
hat_values <- hatvalues(lm.model)

par(mfrow=c(2,2))

plot(hat_values, std_resid, cex = 10 * sqrt(cooks_D), xlab="hat-значения", ylab="Стандартизированные остатки", main='Пузырьковая диаграмма расстояний Кука', col = 'darkturquoise')
abline(h =c(a, b), lty = 2, col = "steelblue")

plot(cooks_D, type="h",lwd=3,col="red", ylab="Cook's Distance", main='Дистанция Кука у наблюдений по индексу')

abline(0,0,col="red")


qqnorm(resid(lm.model), main = "Нормальный Q-Q график", col = "darkgrey")
qqline(resid(lm.model), col = "dodgerblue", lwd = 2)

hist(residuals(lm.model), xlab="Residuals", main="Частота остатков по отклонению",nclass=30,col="orange")


outliers <- data$nonw
#identify influential points
influential_obs <- as.numeric(names(cooks_D)[(cooks_D > 4/(n-P-1))])

#define new data frame with influential points removed
outliers_removed <- outliers[-influential_obs]
data2 <- data[data$nonw %in% c(outliers_removed), ]

```
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/%D0%9E%D1%81%D1%82%D0%B0%D1%82%D0%BA%D0%B8_%D0%BC%D0%BD.png)



Нашли пять выбросов и удалили их
```{r}
(Intercept)        nonw        educ 
 1286.90187     3.24624   -34.69579 
> summary(lm.model2)

Call:
lm(formula = mort ~ nonw + educ, data = data2)

Residuals:
    Min      1Q  Median      3Q     Max 
-70.610 -25.254  -4.257  26.311  80.940 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 1286.9019    75.0972  17.136  < 2e-16 ***
nonw           3.2462     0.6377   5.091 5.01e-06 ***
educ         -34.6958     6.6157  -5.244 2.91e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 36.83 on 52 degrees of freedom
Multiple R-squared:  0.5636,	Adjusted R-squared:  0.5468 
F-statistic: 33.57 on 2 and 52 DF,  p-value: 4.347e-10

```
R^2 улучшился, разброс остатков уменьшился
По тесту Дурбина Уотсона тоже стало лучше, остатки не автокоррелированы

```{r}
	Durbin-Watson test

data:  lm.model
DW = 1.8849, p-value = 0.3237
alternative hypothesis: true autocorrelation is greater than 0

> dwtest(lm.model2)

	Durbin-Watson test

data:  lm.model2
DW = 2.1688, p-value = 0.7334
alternative hypothesis: true autocorrelation is greater than 0

```
Посмотрим, насколько зорощо модель угадывает значения
Для этого разделим датасет на train и test выборку

```{r}
  train.ids <- 1:50
  test.idz <- 51:55
  #test.ids <- seq(from = 0, to = length(data2), by = 2)
  
  m1 <- lm(mort ~ nonw + educ, data2[train.ids,])
  summary(m1)  
  
  Call:
    lm(formula = mort ~ nonw + educ, data = data2[train.ids, ])
  
  Residuals:
    Min      1Q  Median      3Q     Max 
  -69.870 -25.324  -3.189  27.380  79.427 
  
  Coefficients:
    Estimate Std. Error t value Pr(>|t|)    
  (Intercept) 1287.3821    78.0771  16.489  < 2e-16 ***
    nonw           3.0124     0.6631   4.543 3.87e-05 ***
    educ         -34.5253     6.8632  -5.031 7.59e-06 ***
    ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  Residual standard error: 35.82 on 47 degrees of freedom
  Multiple R-squared:  0.5721,	Adjusted R-squared:  0.5538 
  F-statistic: 31.41 on 2 and 47 DF,  p-value: 2.176e-09
  
pred.m1 <- predict(m1, data2[test.ids,])

```
Видим, что угадывает модель не очень хорошо, но сносно: 85% точность. 


```{r}
cor(pred.m1, data2$mort[test.ids])  
[1] 0.8519073
cor(pred.m1, data2$mort[test.ids]) ^ 2 
[1] 0.725746

print(pred.m1)
55       56       57       58       60 
940.7411 892.2187 933.6958 907.1636 953.2060 


plot(pred.m1, data2$mort[test.ids])
abline(0, 1)
```
Построим график зависимости между предсказанными и наблюдаемыми значениями
![jpg](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/pred.png.jpg)
