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
features <- data[,-c(16)]
reg <- regsubsets(features, data$mort)
reg.summary <- summary(reg)
summary(as.matrix(out$which)[best.model,])
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

nonw
educ
prec
so2
jant вместо jult
dens

Как мы видим, в ручном отборе я ошиблась только в одной переменной: видимо, у jant корреляция с остальнымии переменными меньше

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
