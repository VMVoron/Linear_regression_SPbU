Читаем данные
```{r}
data <- read.table('mortality.txt',             
                   header = TRUE)
```
Посмотрим на распределение величины \
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/hist.jpg)

Распределение колокообразное, не увеличенное по краям, работать можно
### Строим модель
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
Увеличение количества не-белого населения на 1 (%) увеличивает коэффициент смертности почти на 4.5 пункта -----  человека (на 100000 населения) \

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
summary(aov(lm.model1))
```

```{r}
            Df Sum Sq Mean Sq F value   Pr(>F)    
nonw         1  94621   94621   41.05 2.88e-08 ***
Residuals   58 133687    2305                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

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
**Наибольшее положительное отклонение истинного значения отклика от модельного** -  94.661  \
**Наибольшее отрицательное  отклонение истинного значения отклика от модельного** - -109.788 

Почти все остатки находятся в квартилях от первой до третьей. 

Все коэффициенты значимы на уровне **p-value** < 0.001 \ 
Значения **p-value Pr(>|t|)**: < 2e-16 для b0 (Intercept), 2.88e-08 для b1 (nonw) 

**Среднеквадратичное отклонение** составляет 0.4043. 
Лишь **40%** вариации данных могут быть объяснены моделью. \
Это не так уж и много, но у нас фактор значим, значит, со спокойной душой учтём это и пропустим мимо. 

**Степени свободны DF: 1 и 58**

**F статистика** высокая, во много раз выше табличной. - 41.05
Общий **уровень значимости** по этой статистике - p-value: 2.88e-08 - 0.0000000288, что намного меньше 0.001

Вернемся к нашему уравнению \
**mort = 887.051909 + 4.489601 * nonw**

Попробуем посчитать доверительный интервал для значимого коэффициента регрессии 
(вот тут я сомневаюсь, если кто-то нашел ошибку - поправьте)

![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/doverit_form.jpg)
> где x - среднее значение выборки ( 4.489601) \
> z - доверительный интервал - 0.001 \
> σ - стандартное отклонение -  0.7007 \
> n - размер выборки - 60

Доверительный интервал:	4.4896 ± 0.352 \
Нижняя граница:	4.1376 \
Верхняя граница:	4.8416

Мы 99,99% уверен, что среднее значение находится между 4.1376 и 4.8416 \

### Посмотрим на выбросы. 
Построим пузырьковый график влиятельности, чтобы посмотреть на остатки. \
Эмпирическое правило - наблюдение имеет высокое влияение, если расстояние Кука превышает 4/(n-P-1) \
n - количество наблюдений
P - число предикторов

```{r}
n = 60
P = 1
a <- min(std_resid)+4/(n-P-1)
b <- max(std_resid)-4/(n-P-1)
library(stats)

std_resid <- rstandard(lm.model1)
cooks_D  <- cooks.distance(lm.model1)
hat_values <- hatvalues(lm.model1)

par(mfrow=c(2,2))

plot(hat_values, std_resid, cex = 10 * sqrt(cooks_D), xlab="hat-значения", ylab="Стандартизированные остатки", main='Пузырьковая диаграмма расстояний Кука', col = 'darkturquoise')
abline(h =c(a, b), lty = 2, col = "steelblue")

plot(cook, type="h",lwd=3,col="red", ylab="Cook's Distance", main='Дистанция Кука у наблюдений по индексу')

abline(0,0,col="red")


qqnorm(resid(lm.model1), main = "Нормальный Q-Q график", col = "darkgrey")
qqline(resid(lm.model1), col = "dodgerblue", lwd = 2)

hist(residuals(lm.model1), xlab="Residuals", main="Частота остатков по отклонению",nclass=30,col="orange")

```
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/%D0%9E%D1%81%D1%82%D0%B0%D1%82%D0%BA%D0%B8_%D1%81%D0%BB%D0%B0%D0%B4%D0%BA%D0%B8.png)

Похоже, что есть несколько наблюдений, которые портят всю малину. \
Будем их вычислять и думать, есть ли смысл мочить их по сортирам, чтобы улучшить модель

```{r}
outliers <- data$nonw
#identify influential points
influential_obs <- as.numeric(names(cooks_D)[(cooks_D > 4/(n-P-1))])

#define new data frame with influential points removed
outliers_removed <- outliers[-influential_obs]
data2 <- data[data$nonw %in% c(outliers_removed), ]
#удалили 3 выброса
library(ggplot2)
outliers_present <- ggplot(data, aes(x = data$nonw, y = data$mort)) + geom_point() +
  geom_smooth(method = lm) + ggtitle("Outliers Present")
                
outliers_removed <- ggplot(data2, aes(x = data2$nonw, y = data2$mort)) + geom_point() +
  geom_smooth(method = lm) + ggtitle("Outliers Removed")

library(gridExtra)
gridExtra::grid.arrange(outliers_present, outliers_removed, ncol = 2) 
```
Посмотрим, стало ли лучше без наших ~~отбросов~~ выбросов
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Vibrosy.png)

Смотря на график, кажется, что лучше не стало. Соберём модель снова, уже без выбросов и сопоставим данные.
```{r}
lm.model2<-lm(formula = mort ~ nonw, data = data2)
lm.model2$coefficients
b0 <- lm.model1$coefficient[1]
b1 <- lm.model1$coefficient[2]
x1 <- min(data2$nonw)
x2 <- max(data2$nonw)
x <- seq(from = x1, to = x2, length.out =100)
y <- b0 + b1*x
plot(data2$nonw, data2$mort, main="LR 2: Mortality rate and the size of the nonwhite population (without outliers)", xlab="Nonwhite population", ylab="Mortality", pch = 16, col = 'dimgray')
grid()
lines(x, y, col="orangered", lwd = 5)
summary(aov(mort ~ nonw, data = data2))
summary(lm.model2)
```
```{r}
            Df Sum Sq Mean Sq F value  Pr(>F)    
nonw         1  59901   59901   29.51 1.3e-06 ***
Residuals   55 111647    2030                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Call:
lm(formula = mort ~ nonw, data = data2)

Residuals:
    Min      1Q  Median      3Q     Max 
-99.719 -26.054  -5.158  34.133  90.310 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 892.8715    10.3476  86.288  < 2e-16 ***
nonw          4.0815     0.7514   5.432  1.3e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 45.05 on 55 degrees of freedom
Multiple R-squared:  0.3492,	Adjusted R-squared:  0.3373 
F-statistic: 29.51 on 1 and 55 DF,  p-value: 1.304e-06
```
И действительно, F статистика стала меньше, R^2 стал меньше, стандартная ошибка возросла \
Остатки были сладки 

![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Rplot07.png)

Вместе с тем, можно сказать, что модель стала оптимистичнее. Рассчитаем точечный прогноз для большого значения фактора. Например, nonw = 80 по нашим двум моделям: с выбросами и без
```{r}
nonw_given = 80
mort_pred1 = 887.051909 + 4.489601 * nonw_given
mort_pred2 = 892.8715 + 4.0815 * nonw_given
mort_pred1 - 1246.22
mort_pred2 - 1219.391
```
Теперь модель без выбросов интерпретативно мне нравится больше
Для проверки на гетероскедастичность построим график "Абсолютное значение остатков против предсказанных значений"
```{r}
df1 <- data.frame(
  resid = residuals(lm.model1),
  pred = predict(lm.model1))

df2 <- data.frame(
  resid = residuals(lm.model2),
  pred = predict(lm.model2))



ggplot(df1, aes(pred, abs(resid))) +
  geom_point() +
  geom_smooth()

ggplot(df2, aes(pred, abs(resid)) + 
  geom_point() +
  geom_smooth()
```
Модель с выбросами \
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Rplot08.png)

Модель без выбросов \
![png](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/Rplot09.png)

Ничего подозрительного замечено не было

```{r}
library(lmtest)
dwtest(lm.model1)
```
Проведем тест Дурбина-Уотсона 
На практике применение критерия Дарбина—Уотсона основано на сравнении величины DW с теоретическими значениямиe d_L  d_U для заданного числа наблюдений n, числа независимых переменных модели k и уровня значимости p .

Если DW < d_L, то гипотеза о независимости случайных отклонений отвергается (следовательно, присутствует положительная автокорреляция);
Если  DW > d_U, то гипотеза не отвергается;
Если d_L < DW < d_U, то нет достаточных оснований для принятия решений.
Критические значения DW для k = 1
n    d_L   d_U
55	1,53	1,60
60	1,55	1,62

Проведем тест Дурбина Уотсона на датасете с выбросами - 60 переменных
```{r}
        Durbin-Watson test
        
data:  lm.model1
DW = 1.5556, p-value = 0.0396
alternative hypothesis: true autocorrelation is greater than 0
```
Мы стоим на пороге автокорреляиции!  И попали ровно в значение d_L
Без выбросов - 57 переменных
```{r}
dwtest(lm.model2)

       Durbin-Watson test
       
data:  lm.model2
DW = 1.754, p-value = 0.1739
alternative hypothesis: true autocorrelation is greater than 0
```
Без остатков мы уже видим меньше признаков автокорреляции
Однако, мы не можем опираться на данные этого теста, так как \
а) наша выборка слишком мала \
б) могут присутствовать автокорреляции второго и более высших порядков


the end
