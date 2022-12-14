Итак, Ручным методом отобрали 6 объясняющих переменных:
1. nonw
2. educ
3. prec
4. so2
5. jult
6. dens
### Сравним наш результат с методом подгона путем уменьшения MSE 
```{r}
data <- read.table('mortality.txt',             
                   header = TRUE) 
                   
library(leaps)
# Remove the target variable.  VERY IMPORTANT!
features <- data[,-c(16)]
out<-leaps(features,data$mort)
best.model <- which(out$Cp==min(out$Cp))
print("Complete.")
print(colnames(data[,-c(16)]))
print(as.matrix(out$which)[best.model,])
```
```{r}
[1] "Complete."
[1] "prec"  "jant"  "jult"  "ovr65" "popn"  "educ"  "hous"  "dens"  "nonw"  "wwdrk" "poor"  "hc"    "nox"   "so2"   "humid"
      1       2        3       4      5        6       7       8      9        A       B     C        D       E        F 
     TRUE   TRUE     TRUE    FALSE  FALSE    TRUE   FALSE    TRUE   TRUE     FALSE   FALSE  TRUE    TRUE   FALSE    FALSE
```
1. prec 
2. jant
3. jult
4. educ
5. dens
6. nonw 
7. hc
8. nox

Из набора данных исчез so2, зато добавились переменные jant, hc, nox

Посмотрим на коэффициенты нашей модели
```{r}
best.cp_model <- which.min(reg.summary$cp)
coef(reg, best.cp_model)
```
```{r}
(Intercept)          prec          jant          jult          educ          dens          nonw            hc           nox 
 1.181381e+03  1.334171e+00 -1.438639e+00 -2.197716e+00 -1.564402e+01  8.837313e-03  4.594440e+00 -7.740279e-01  1.568656e+00 
```
Получили уравнение вида: 
```{r}
mort = 1.181381e+03 + 1.334171e+00 * prec - -1.438639e+00 * jant -2.197716e+00 *  jult + 8.837313e-03 * dens + 4.594440e+00 * nonw - -7.740279e-01 * hc + 1.568656e+00  * nox
```

```{r}
features <- data[,-c(16)]
reg <- regsubsets(features, data$mort)
reg.summary <- summary(reg)
summary(as.matrix(out$which)[best.model,])
```

Также тут мы можем посмотреть оптимальные переменные для каждого числа переменных (от 1 до 8)
```{r}
Selection Algorithm: exhaustive
         prec jant jult ovr65 popn educ hous dens nonw wwdrk poor hc  nox so2 humid
1  ( 1 ) " "  " "  " "  " "   " "  " "  " "  " "  "*"  " "   " "  " " " " " " " "  
2  ( 1 ) " "  " "  " "  " "   " "  "*"  " "  " "  "*"  " "   " "  " " " " " " " "  
3  ( 1 ) " "  "*"  " "  " "   " "  "*"  " "  " "  "*"  " "   " "  " " " " " " " "  
4  ( 1 ) " "  "*"  " "  " "   " "  "*"  " "  "*"  "*"  " "   " "  " " " " " " " "  
5  ( 1 ) "*"  "*"  " "  " "   " "  "*"  " "  "*"  "*"  " "   " "  " " " " " " " "  
6  ( 1 ) "*"  "*"  " "  " "   " "  "*"  " "  "*"  "*"  " "   " "  " " " " "*" " "  
7  ( 1 ) "*"  "*"  " "  " "   " "  "*"  " "  "*"  "*"  " "   " "  "*" "*" " " " "  
8  ( 1 ) "*"  "*"  "*"  " "   " "  "*"  " "  "*"  "*"  " "   " "  "*" "*" " " " " 
```
Так какое же количество переменных оптимально для модели? 
Мы можем узнать это, посмотрев на следующие графики
```{r}
par(mfrow=c(2,2))
# RSS
plot(reg.summary$rss, xlab="Number of Variables", ylab="Resid. Sum of Squares", type="l")
best.rss <- which.min(reg.summary$rss)
points(best.rss, reg.summary$rss[best.rss], col="red", cex=2,pch=20)


# AdjR2
plot(reg.summary$adjr2, xlab="Number of Variables", ylab="Adjusted R2", type="l")
best.adjr2 <- which.max(reg.summary$adjr2)
points(best.adjr2, reg.summary$adjr2[best.adjr2], col="red", cex=2,pch=20)

# Note in this case that AIC and Cp are the same.
# Mallows Cp
plot(reg.summary$cp, xlab="Number of Variables", ylab="Mallows Cp", type="l")
best.cp <- which.min(reg.summary$cp)
points(best.cp, reg.summary$cp[best.cp], col="red", cex=2,pch=20)

# BIC
plot(reg.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
best.bic <- which.min(reg.summary$bic)
points(best.bic, reg.summary$bic[best.bic], col="red", cex=2,pch=20)

```
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/plots.png)

Подробнее про метрики можно прочитать тут: 
> [Residual sum of squares](http://www.machinelearning.ru/wiki/index.php?title=Остаточная_сумма_квадратов) \
> [Adjusted R^2](http://www.machinelearning.ru/wiki/index.php?title=Коэффициент_детерминации) \
> [Mallow CP](https://en.wikipedia.org/wiki/Mallows%27s_Cp) \
> [Bayesian information criterion](http://www.machinelearning.ru/wiki/index.php?title=Байесовский_информационный_критерий)

Для улучшения интерпретации модели, остановимся на 6 переменных, чем их меньше  - тем проще интерпретировать

1. nonw
2. educ
3. prec
4. so2
5. jant вместо jult
6. dens

Как мы видим, в ручном отборе я ошиблась только в одной переменной: видимо, у jant корреляция с остальнымии переменными меньше. Это показывает, что и ручной отбор может иметь неплохой результат 

Посторим множественную ЛР
```{r}
data <- read.table('mortality.txt',             
                   header = TRUE)       

lm.model<-lm(formula = mort ~ nonw + educ + prec + so2 + jant + dens , data = data)
lm.model$coefficients

```
Коэффициенты:
```{r}
(Intercept)          nonw          educ          prec           so2          jant          dens 
 1.020645e+03  4.040963e+00 -1.526192e+01  1.394245e+00  1.909573e-01 -1.597973e+00  8.379177e-03 
 
```
Интерпретативно направшивается вывод, что принадлежность к non-white увеличивает смертность, образование - снижает, prec -  суммарное количество осадков - увеличивает, как и количество so2, а также плотность населения играет не последнюю роль.

```{r}
Call:
lm(formula = mort ~ nonw + educ + prec + so2 + jant + dens, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-85.182 -18.398  -0.011  18.905  97.897 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.021e+03  8.582e+01  11.893  < 2e-16 ***
nonw         4.041e+00  5.964e-01   6.775 1.04e-08 ***
educ        -1.526e+01  6.293e+00  -2.425 0.018745 *  
prec         1.394e+00  5.659e-01   2.464 0.017035 *  
so2          1.910e-01  8.479e-02   2.252 0.028485 *  
jant        -1.598e+00  4.093e-01  -3.904 0.000269 ***
dens         8.379e-03  3.538e-03   2.368 0.021542 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 33.1 on 53 degrees of freedom
Multiple R-squared:  0.7457,	Adjusted R-squared:  0.7169 
F-statistic: 25.91 on 6 and 53 DF,  p-value: 3.885e-14
```
Все факторы статистически значимы на уровне 0.01, статистики неплохие
Однако, здесь требуется держать в голове несколько вещей
1) Возможно, плотность населения выше там, гже ниже доход, т.к. датасет американский
2) Возможно, более богатые белые долгожители селятся там, где погода лучше, поэтому более теплый январь снижает уровень смертности в модели, а также в suburban areas в коттеджах, где выбросы so2 меньше
3) Возможно, такое большое количество факторов исказило интепретативную составляющую, если мы уберем несколько факторов из модели, влияние оставшихся увеличится, как и их пропорция. Так, в модели, где действуют лишь два фактора: nonw и educ, влияние образование в несколько раз выше, чем принадлежность к nonw
Таким образом, модель с шестью переменными имеет меньше остатков, но хуже поддается интерпретации.
Поэтому, если делать модель, лучше всего ОБЪЯСНЯЮЩУЮ факторы, но не лучшую с точки зрения метода MSE, то можно оставить два фактора: nonw и educ
Загоню модель с двумя факторами в программу и покажу результаты

```{r}

data <- read.table('mortality.txt',             
                   header = TRUE)       

lm.model<-lm(formula = mort ~ nonw + educ, data = data)
lm.model$coefficients
summary(lm.model)

```
Вывод: 
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
В плане интерпретации стало гораздо лучше:

mort = 1211.856232 + 3.916516 * nonw -28.979335 * educ

the number of years of schooling for persons over 22; - educ 
the size of the nonwhite population;  - nonw

Видим, что отклонения увеличились, все факторы крайне значимы 0.0001, но R^2 подкачал: всего 0.5474
А имели ли мы право просто так выкинуть несколько переменных, даже если эти перменные - наилучших выбор при их количестве = 2
Определим с помощью дисперсионного анализа является ли отличие моделей значимым или нет.

```{r}

lm.model2<-lm(formula = mort ~ nonw + educ, data = data)
lm.model6<-lm(formula = mort ~ nonw + educ + prec + so2 + jant + dens, data = data)
lm.model8<- lm(formula = mort ~ nonw + educ + prec + jant + jult + dens + hc + nox, data = data)

```
```{r}
anova(lm.model8, lm.model6, lm.model2)
Analysis of Variance Table

Model 1: mort ~ nonw + educ + prec + jant + jult + dens + hc + nox
Model 2: mort ~ nonw + educ + prec + so2 + jant + dens
Model 3: mort ~ nonw + educ
  Res.Df   RSS Df Sum of Sq       F    Pr(>F)    
1     51 51446                                   
2     53 58052 -2     -6606  3.2743   0.04594 *  
3     57 99825 -4    -41773 10.3527 3.247e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

```
Видим, что с вероятностью 99.999% отличие модели 2  от модели 8 не значимо, от модель 6 тоже не значимо, и мы вправе выбрать любую модель.
