# Linear_regression_SPbU - Линейные регрессии в рамках учебного курса
### [Данные для скачивания](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/mortality.txt)

##  Discussion:

##    The death rate is to be represented as a function of other variables.

###    There are 60 rows of data.  The data includes (расшифровка переменных):

      I,   the index;  
      A1,  the average annual precipitation; - prec
      A2,  the average January temperature;  - jant
      A3,  the average July temperature; -  jult 
      A4,  the size of the population older than 65;  - ovr65
      A5,  the number of members per household;  - popn 
      A6,  the number of years of schooling for persons over 22; - educ 
      A7,  the number of households with fully equipped kitchens; - hous 
      A8,  the population per square mile; - dens
      A9,  the size of the nonwhite population;  - nonw
      A10, the number of office workers; -  wwdrk 
      A11, the number of families with an income less than $3000; - poor 
      A12, the hydrocarbon pollution index;-  hc 
      A13, the nitric oxide pollution index; - nox 
      A14, the sulfur dioxide pollution index;-  so2 
      A15, the degree of atmospheric moisture.-  humid 
      B,   the death rate.-  mort

##    We seek a model of the form:

      B =  A1 *  X1 +  A2 *  X2 +  A3 *  X3 +  A4 *  X4 +  A5 *  X5
        +  A6 *  X6 +  A7 *  X7 +  A8 *  X8 +  A9 *  X9 + A10 * X10
       + A11 * X11 + A12 * X12 + A13 * X13 + A14 * X14 + A15 * X15
       
## [Часть 1. Overview датасета и ручной выбор переменных](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/1-Linear_overview.md) 
## [Часть 2. Fit модели методом MSE](https://github.com/VMVoron/Linear_regression_SPbU/blob/main/2-MSE.md)
     
> Reference:

>> Richard Gunst, Robert Mason,
>> Regression Analysis and Its Applications: a data-oriented approach,
>> Dekker, 1980, pages 370-371.
>> ISBN: 0824769937.

>> Gary McDonald, Richard Schwing,
>> Instabilities of regression estimates relating air pollution to mortality,
>> Technometrics,
>> Volume 15, Number 3, pages 463-482, 1973.

>> Helmut Spaeth,
>> Mathematical Algorithms for Linear Regression,
>> Academic Press, 1991,
>> ISBN 0-12-656460-4.

>> Andrew Bruce, Peter Bruce
>> Practical Statistics for Data Scientists
>> © 2017 
>> ISBN 9781491952962 

>> А. Б. Шипунов, Е. М. Балдин, П. А. Волкова,А. И. Коробейников, С. А. Назарова,С. В. Петров, В. Г. Суфиянов
>> Наглядная статистика Используем R!
>> Москва, 2014
>> ISBN 9785970600948

