Читаем данные
```{r}
data <- read.table('mortality.txt',             
                   header = TRUE)
```
Строим модель
```{r}
lm.model1<-lm(formula = mort ~ nonw, data = data)
```
Посмотрим на коэффициенты
```{r}
lm.model1$coefficients
```
```{r}
(Intercept)        nonw 
 887.051909    4.489601 
```
Получили уравнение вида: \
mort = 887.051909 + 4.489601 * nonw \
Увеличение количества не-белого населения на 1 (%) увеличивает коэффициент смертности почти на 4.5 пункта \

Построим график
```{r}
b0 <- lm.model1$coefficient[1]
b1 <- lm.model1$coefficient[2]
x1 <- min(data$nonw)
x2 <- max(data$nonw)
x <- seq(from = x1, to = x2, length.out =100)
y <- b0 + b1*x
plot(data$nonw, data$mort, main="Linear regression 1: Mortality rate and the size of the nonwhite population", xlab="Nonwhite population", ylab="Mortality", pch = 16, col = 'dimgray')
grid()
lines(x, y, col="firebrick", lwd = 5)
```
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Rplot06.png)

Посмотрим результаты линейной аппроксимации: 
```{r}
summary(lm.model1)
```

```{r}
Call:
lm(formula = mort ~ nonw, data = data)

Residuals:
     Min       1Q   Median       3Q      Max 
-109.788  -32.738   -4.006   35.068   94.661 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 887.0519    10.3748  85.501  < 2e-16 ***
nonw          4.4896     0.7007   6.407 2.88e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 48.01 on 58 degrees of freedom
Multiple R-squared:  0.4144,	Adjusted R-squared:  0.4043 
F-statistic: 41.05 on 1 and 58 DF,  p-value: 2.88e-08
```
Наибольшее положительное отклонение истинного значения отклика от модельного -  94.661  \
Наибольшее отрицательное  отклонение истинного значения отклика от модельного - -109.788 \

Почти все остатки находятся в квартилях от первой до третьей. \

Все коэффициенты значимы на уровне p-value < 0.001 \ 
Значения p-value Pr(>|t|): < 2e-16 для b0 (Intercept), 2.88e-08 для b1 (nonw) \ 

Среднеквадратичное отклонение составляет 0.4043. 
Лишь 40% вариации данных могут быть объяснены моделью. \
Это не так уж и много, но у нас фактор значим, значит, со спокойной душой учтём это и пропустим мимо. \

Степени свободны DF: 1 и 58

F статистика высокая, во много раз выше табличной. - 41.05
Общий уровень значимости по этой статистике - p-value: \ 2.88e-08 - 0.0000000288, что намного меньше 0.001


```{r}

```
