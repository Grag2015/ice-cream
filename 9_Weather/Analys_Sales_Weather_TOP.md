---
output: html_document
---
# Многофакторная линейная регрессионная модель зависимости продаж мороженного от погоды




## Подготовка данных
включаем нужные библиотеки

```r
library("dplyr")
library("ggplot2")
library("lattice")
library("xtable")
library("car")
```

Загрузка обработанных данных

```r
retail <- dget(file = "./retailtidy")
weather <- dget(file = "./weathertidy")
```

Объединение данных о продажах и погоде

```r
retweat <- merge(x=retail, y=weather, by = "date", all.x = T, all.y = F)
```

Выборка данных для бренда ТОП

```r
retweattop <- retweat[retweat$BRAND=="ТОП",]
```
в дальнейшем работаем с датасетом `retweattop`

## Разведочный анализ
предварительный анализ показал, что характер зависимости продаж от погоды 
значительно различается для различных времен года. смотрите для примера характер
зависимости продаж от температуры воздуха


```r
xyplot(saleskg~tem|season, data = retweattop, layout = c(1, 4), scales 
            = "free", breaks = 50)
```

```
## Error in limits.and.aspect(default.prepanel, prepanel = prepanel, have.xlim = have.xlim, : need at least one panel
```

поэтому было принято решение строить отдельные модели для различных времен года

## 1. Бренд: ТОП, сезон: ЛЕТО


```r
tmpseasonstr <- "summer"
tmpseason <- retweattop[retweattop$season==tmpseasonstr,]
```

## Подбор модели
Используем функцию `step` для автоподбора (берем только числовые переменные)

```r
model_full <- lm(saleskg ~ I(tem^2)+tem*clowdly*winds*humidity*ppp,
                 data = tmpseason[,c("saleskg", "tem", "clowdly", "winds", "humidity", "ppp")]) 
```

```
## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...): 0 (non-NA) cases
```

```r
model_null <- lm(saleskg ~ 1, data = tmpseason[,c("saleskg", "tem", "clowdly", "winds", "humidity", "ppp")])
```

```
## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...): 0 (non-NA) cases
```

```r
ideal_model <- step(object = model_full, scope = list(lower = model_null, upper = model_full), direction = 'backward')
```

```
## Error in terms(object): object 'model_full' not found
```

Результат для наилучшей модели `ideal-model`

```
## Error in summary(model): object 'ideal_model' not found
```

```
## Error in summary(ideal_model): object 'ideal_model' not found
```

```
## Error in print(xt, type = "html"): object 'xt' not found
```

             

##  
Мы видим значимые различия найденной модели `ideal_model` по сравнению с 
моделью с максимальным количеством факторов `model_full` 

```r
xt <- anova(model_full, ideal_model)
```

```
## Error in anova(model_full, ideal_model): object 'model_full' not found
```


```
## Error in xtable(xt): object 'xt' not found
```

##
Но ручной подбор позволяет упростить модель, сохранив при этом значение коэффициента 
детерминации `Adjusted R-squared` и значимость коэффициентов

```r
fit <- lm(saleskg^(1/3) ~ I(tem^2)+tem*humidity*ppp, data = tmpseason) 
```

```
## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...): 0 (non-NA) cases
```

```
## Error in summary(model): object 'fit' not found
```

```
## Error in summary(fit): object 'fit' not found
```

```
## Error in print(xt, type = "html"): object 'xt' not found
```

## Проверка модели

1.1 нормальность остатков (тест Шапиро-Вилка)

```r
test1 <- shapiro.test(fit$residuals)
```

```
## Error in stopifnot(is.numeric(x)): object 'fit' not found
```

```
## Error in ifelse(test1$p.value < 0.05, "Остатки не распределены нормально", : object 'test1' not found
```

1.2 визуальная оценка нормальности остатков (QQ-plot)

```
## Error in qqPlot(fit, labels = row.names(tmpseason), id.method = "identify", : object 'fit' not found
```

2. независимость остатков (тест Дарбина-Уотсона)

```r
test2 <- durbinWatsonTest(fit)
```

```
## Error in durbinWatsonTest(fit): object 'fit' not found
```

```
## Error in ifelse(test2$p < 0.05, "Есть зависимость", "Нет зависимости"): object 'test2' not found
```

3.1 гомоскедастичность остатков

```r
test3 <- ncvTest(fit) 
```

```
## Error in ncvTest(fit): object 'fit' not found
```

```
## Error in ifelse(test3$p < 0.05, "Гомоскедастичность НЕ выполняется", : object 'test3' not found
```

3.2 Визуальная оценка гомоскедастичности

```r
spreadLevelPlot(fit)
```

```
## Error in spreadLevelPlot(fit): object 'fit' not found
```

##  
**Вывод:** можно сказать что модель соответствует требованиям, те небольшие 
отклонения от нормальности можно нивелировать при подгонке модели.


## Подгонка модели
Этап подгонки подели подробно не расписывал. Если кратко выбросы находил с помощью
теста Бонферони, расчета метрики Кука для поиска влиятельных значений, а также 
визуально на графиках, но как правило удалял немного - около 4-7 наблюдений.


```
## Error in eval(expr, envir, enclos): object 'fit' not found
```


## Визуализация результатов
Кривая зависимости продаж от температуры при различных комбинациях давления и влажности



```
## Error in `[.data.frame`(base, names(rows)): undefined columns selected
```

##  
**Вывод:** На данном графике хочется отметить (см. верхний), что при повышенном 
давлении продажи выше, если влажность выше.
В целом, попытка посмотреть на найденную зависимость при различных срезах 
предикторов наводит на мысль, что переменные Влажность и Давление не так явно 
влияют на продажи, но при этом значительно усложняют модель и ее интерпретацию.


Далее, рассмотрим еще модель для бренда ТОП для весны, будем смотреть зависимость
только от температуры.

## 1. Бренд: ТОП, сезон: ВЕСНА


```r
tmpseasonstr <- "spring"
tmpseason <- retweattop[retweattop$season==tmpseasonstr,]
```

## Подбор модели
Здесь подбирал коэффициенты вручную

```r
fit <- lm(saleskg^(1/3) ~ tem+I(tem^2), data = tmpseason) 
```

```
## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...): 0 (non-NA) cases
```

```
## Error in summary(model): object 'fit' not found
```

```
## Error in summary(fit): object 'fit' not found
```

```
## Error in print(xt, type = "html"): object 'xt' not found
```

##
как видим модель гораздо более полно описывает продажи, чем предыдущая, доля 
объясненной дисперсии в весенней модели гораздо выше - 0.66 против 0.37 летом. 
Но это особенность сезона - по всем брендам летом влияние погоды (а точнее я 
смотрел температуру) не так значительно как весной или осенью. Зимой метео 
условия вообще мало влияют на продажи.

## Проверка модели

1.1 нормальность остатков (тест Шапиро-Вилка)

```r
test1 <- shapiro.test(fit$residuals)
```

```
## Error in stopifnot(is.numeric(x)): object 'fit' not found
```

```
## Error in ifelse(test1$p.value < 0.05, "Остатки не распределены нормально", : object 'test1' not found
```

1.2 визуальная оценка нормальности остатков (QQ-plot)

```
## Error in qqPlot(fit, labels = row.names(tmpseason), id.method = "identify", : object 'fit' not found
```

2. независимость остатков (тест Дарбина-Уотсона)

```r
test2 <- durbinWatsonTest(fit)
```

```
## Error in durbinWatsonTest(fit): object 'fit' not found
```

```
## Error in ifelse(test2$p < 0.05, "Есть зависимость", "Нет зависимости"): object 'test2' not found
```

3.1 гомоскедастичность остатков

```r
test3 <- ncvTest(fit) 
```

```
## Error in ncvTest(fit): object 'fit' not found
```

```
## Error in ifelse(test3$p < 0.05, "Гомоскедастичность НЕ выполняется", : object 'test3' not found
```

3.2 Визуальная оценка гомоскедастичности

```r
spreadLevelPlot(fit)
```

```
## Error in spreadLevelPlot(fit): object 'fit' not found
```

## 
**Вывод:** есть значительные отклонения от требования, подгонка модели не помогла
полностью привести модель в соответствие с требованиями.


## Подгонка модели
Этап подгонки подели подробно не расписывал.

## Визуализация результатов
Кривая зависимости продаж от температуры


```r
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="grey", size=4,  alpha = 1/2) + 
  geom_line(aes(x = tem, y = (fit$fitted.values)^3), col = 'blue', lwd=1)+
  geom_line(aes(x = tem, y = (fit$fitted.values)^3+1.6*sd((fit$residuals)^3)), col = 'red',  linetype = 3)+
  geom_line(aes(x = tem, y = (fit$fitted.values)^3-1.6*sd((fit$residuals)^3)), col = 'red',  linetype = 3)
```

```
## Error in eval(expr, envir, enclos): object 'fit' not found
```

## Общий вывод
В целом можно сказать, что погода значительно влияет на продажи, при этом летом
это влияние не так заметно, гораздо заметнее связь в межсезонье, зимой связь 
совсем слабая. Также хочется отметить, что основным фактором с подавляющим 
перевесом является температура воздуха, остальные факторы влияют гораздо 
менее значимо, но усложняют модель.


