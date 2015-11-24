---
output: html_document
---
# Подготовка данных о погоде


включаем нужные библиотеки

```r
library("dplyr")
library("lubridate")
library("ggplot2")
library("lattice")
library("car")
```

## об источнике данных
*источник данных* - [pogoda.by](http://pogoda.by/zip/index.php?Year=2015#csv)  
*дата загрузки*: 15/10/2015  
*локация*: данные скачаны для города Минска (см. строку в таблице по ссылке с 
параметром "№ ВМО" = 26850)  
*период* янв2006-сен2015г  
*описание набора файлов* Каждый файл содержит данные по одной метеостанции за 
месяц. Дискретность метеонаблюдений 3 часа. В файле каждая запись (одна строка) 
— это один срок метеонаблюдений. Метеопараметры в строке разделяются точкой с 
запятой. Отделение целой части числа от дробной осуществляется точкой.  
*описание данных (по столбцам)*:   
1. число месяца;  
2. срок наблюдений [часы], UTC;  
3. T - температура воздуха на высоте 2 м [Цельсия];  
4. dd - направление ветра на высоте 10 м [румбы, откуда дует];    
5. FF - средняя скорость ветра на высоте 10 м [м/с];  
6. ww - код погоды в срок наблюдения [CL - без явлений, BR - дымка, FG - туман, 
RA - дождь SHRA - ливневый дождь, SNRA - снег с дождем, SN - снег, SHSN - 
ливневый снег, TS - гроза, DZ - морось, FZ - гололед, HL - град];  
7. N - количество облаков [0 - ясно, 1 - малооблачно (1-2 балла), 3 - небольшая 
облачность (3-4), 6 - переменная (5-7 баллов), 7 - обл. с прояснениями (8-9), 9 
- сплошная (10 баллов)];  
8. vv - метеорологическая дальность видимости [км];  
9. U - относительная влажность воздуха [%];   
10. PPP - атмосферное давление на уровне моря [гПа].  

## Загрузка данных
Загружаем первый файл

```r
test <- read.csv('./Raw data/26850_2006-01.csv', sep = ';', header = F)
head(test)
```

```
##   V1 V2   V3    V4 V5 V6 V7 V8 V9  V10
## 1  1  0 -4.3 Южный  3 CL  3 10 84 1016
## 2  1  3 -4.7   Ю-В  3 CL  1 10 86 1017
## 3  1  6 -4.8   Ю-В  2 CL  3 10 83 1018
## 4  1  9 -3.3 Южный  3 CL  9 10 81 1020
## 5  1 12 -1.1 Южный  3 CL  6 10 72 1020
## 6  1 15 -1.5 Южный  3 CL  6 10 68 1021
```

Готовим dataframe `weather` для хранения данных из всех файлов, добавляем туда 
первый загруженный файл и добавляем 2 дополнительных столбца `year` и `month` 
для записи года и месяца наблюдений (эти данные берем из имени файла)

```r
files <- list.files(path = "./Raw data/", pattern = ".*csv")
weather <- test
weather <- weather[1,1:10]
weather <- cbind(weather, "year", "month")
weather <- weather[FALSE,]
```

Скачиваем все файлы и парсим год и месяц из имени файла

```r
for (i in 1:length(files)) {
  tyear <- gsub(pattern = "([0-9]{5}_)|(-\\d{1,2}\\.csv)", x=files[i], replacement = "")
  tmonth <- gsub(pattern = "([0-9]{5}_[0-9]{4}-(0)?)|(.csv)", x=files[i], replacement = "")
  temp <- read.csv(paste0("./Raw data/",files[i]), sep = ";", header = F)
  temp <- cbind(temp, "year", "month")
  temp[,11] <- rep(tyear, len = nrow(temp))
  temp[,12] <- rep(tmonth, len = nrow(temp))
  weather <- rbind(weather,temp)
}
```

Переименовываем столбцы

```r
names(weather) <- c("day", "hour", "tem", "windd", "winds", "rainy", "clowdly","vv", "humidity", "ppp", "year", "month")
```
И таким образом имеем датафрейм

```r
head(weather)
```

```
##   day hour  tem windd winds rainy clowdly vv humidity  ppp year month
## 1   1    0 -4.3 Южный     3    CL       3 10       84 1016 2006     1
## 2   1    3 -4.7   Ю-В     3    CL       1 10       86 1017 2006     1
## 3   1    6 -4.8   Ю-В     2    CL       3 10       83 1018 2006     1
## 4   1    9 -3.3 Южный     3    CL       9 10       81 1020 2006     1
## 5   1   12 -1.1 Южный     3    CL       6 10       72 1020 2006     1
## 6   1   15 -1.5 Южный     3    CL       6 10       68 1021 2006     1
```

##Анализ пропущенных и недопустимых значений

функция `checkna` вычисляет количество пропущенных значений для каждого столбца.
Как видим, параметр `clowdly` имеет 545 значений NA, остальные - 4-8 NA-значений.

```r
checkna(weather)
```

```
##      day     hour      tem    windd    winds    rainy  clowdly       vv 
##        0        0        0        0        0        0      545        8 
## humidity      ppp     year    month 
##        4        4        0        0
```
в направлении ветра `windd` есть невалидные значения с "?", также есть 4917 пустых
значений

```r
table(weather$windd, useNA = "ifany")
```

```
## 
##           Восточный  Западный       С-В       С-З  Северный       Ю-В 
##      4917      1814      3241      2519      3870      1629      3070 
##       Ю-З     Южный        ?-      ????   ???????  ???????? 
##      3865      3344       104         7        55        11
```
значения с "?" заменим на NA

```r
is.na(weather$windd[grepl("\\?", weather$windd)]) <- T
```
зададим уровни фактора для `windd` (и сделаем упорядоченным для дальн.
группировки), все значения вне уровней получат значение NA (в том числе 4917 
пустых значений)

```r
weather$windd <- factor(x = weather$windd, levels = c("Восточный", "Западный", 
                "С-В", "С-З", "Северный", "Ю-В", "Ю-З", "Южный"),ordered = T)
```
В параметре скорость ветра `winds` есть недопустимые значения. 

```r
table(weather$winds)
```

```
## 
##     1     2     3     4     5 Штиль     6     7    21     8          99 
##  9151  8206  4235  1487   329  4903    79     7     1     2     1     1 
##    60    57    30    11   ???     9 
##     1     1     1     1    39     1
```
"Штиль" - заменим на 0, зн-я выше 21 (их всего 7) заменим на NA, значения "???" 
(39 шт) заменим на NA

```r
weather$winds <- as.character(weather$winds)
weather$winds[grepl("Штиль", weather$winds)] <- "0"
is.na(weather$winds[grepl("\\?", weather$winds)]) <- T
is.na(weather$winds[weather$winds %in% c("21", "30", "57", "60", "99")]) <- T
```
И преобразуем `winds` в числовой формат

```r
weather$winds <- as.numeric(weather$winds)
```
В значениях влажности воздуха `humidity` есть выбросы

```r
head(weather$humidity[order(weather$humidity, decreasing = T)], n = 20)
```

```
##  [1] 680 174 155 148 144 136 135 133 131 127 120 119 117 115 113 110 107
## [18] 107 106 105
```
значение равное 680 заменим на NA, другие значения большие 100 заменим на 100, 
(учитывая, что отн.влажность выше 100% возможно только при специальных условиях)

```r
is.na(weather[which(weather$humidity==680,arr.ind = T),"humidity"]) <- T
weather$humidity[weather$humidity>100] <- 100
```

## Агрегирование данных
Так как дискретность метеонаблюдений составляет 3 часа, то данные надо усреднить - 
группировать по параметру day+year+month
при этом рассчитаем следующие агрегированные показатели:  
 * температура - средняя за 9,12,15,18-часовые даннные  
 * облачность - среднее за 9,12,15,18-часовые даннные   
 * скорость ветра - средняя за 9,12,15,18-часовые даннные  
 * направление ветра - "максимальное" за 9,12,15,18-часовые даннные  
 * влажность - средняя за 9,12,15,18-часовые даннные  
 * давление - среднее за 9,12,15,18-часовые даннные  

```r
t1 <- subset(weather, weather$hour>=9 & weather$hour<=18)
t2 <- group_by(t1, day, month, year)
weather2 <- summarise(t2, tem=mean(tem, na.rm = T), clowdly=mean(clowdly, na.rm = T), 
                      winds=mean(winds, na.rm=T), windd=max(windd, na.rm=T),
                      humidity=mean(humidity, na.rm=T), ppp=mean(ppp, na.rm=T))
```

*замечание: было 39 предупреждений, о том, что в группах для windd не было* 
*отличных от NA значений "no non-missing arguments to max; returning -Inf" -*
*уточнил: это значит будут NA*

проверка NA после группировки и расчета агрегированных показателей

```r
temp <- checkna(weather2)
```
хороший результат: 

```
##      day    month     year      tem  clowdly    winds    windd humidity 
##        0        0        0        0       11        0       39        0 
##      ppp 
##        0
```


## Добавление дополнительных параметров
для облачности `clowdly` добавляем факторный вариант `clowdlyf` с 4-мя уровнями - ясно, средне, облачно, пасмурно

```r
weather2$clowdlyf <- as.character(weather2$clowdly)
weather2$clowdlyf[(weather2$clowdly<=3) & !is.na(weather2$clowdly)]<- 'ясно'
weather2$clowdlyf[(weather2$clowdly > 3 & weather2$clowdly <= 7) & 
                    !is.na(weather2$clowdly)]<- 'средне'
weather2$clowdlyf[(weather2$clowdly > 7 & weather2$clowdly < 9) & 
                    !is.na(weather2$clowdly)]<- 'облачно'
weather2$clowdlyf[(weather2$clowdly >= 9) & !is.na(weather2$clowdly)]<- 'пасмурно'
weather2$clowdlyf <- factor(weather2$clowdlyf, levels = 
                              c("пасмурно", "облачно", "средне", "ясно"), ordered = T)
```

Добавляем дату (используя параметры День, Месяц, год) и удаляем столбцы День, Месяц,
Год.


```r
weather2$date <- weather2$day
for (i in 1:nrow(weather2)) {
  t1 <- weather2[i,1]; if (nchar(t1)==1) {t1 <- paste0("0",t1)}
  t2 <- weather2[i,2]; if (nchar(t2)==1) {t2 <- paste0("0",t2)} 
  weather2[i,"date"] <- paste0(weather2[i,3],"-", t2,"-",t1)
}
weather2$date <- as.Date(weather2$date)
weather2 <- weather2[,-c(1:3)]
```

Добавляем факторную переменную сезон (зима, лето, весна, осень)

```r
weather2$season <- as.factor(season(weather2$date))
```

**Таким образом**, данные о погоде готовы для дальнейшего анализа. Сохраняем их 
в переменную `weathertidy`

```r
dput(x = weather2, file = "./weathertidy")
```


