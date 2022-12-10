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
```{r}
```
