getwd()
setwd("D:/Grag/R/R-studio/Ice-creame")
test <- read.csv("./_Raw data/погода/26850_2006-01.csv", sep = ";", header = F)

# источник данных - http://pogoda.by/zip/index.php?Year=2015#csv
# описание данных 
# число месяца
# срок наблюдений [часы], UTC
# T Ч температура воздуха на высоте 2 м [∞—];
# dd Ч направление ветра на высоте 10 м [румбы, откуда дует];
# FF Ч средняя скорость ветра на высоте 10 м [м/с];
# ww Ч код погоды в срок наблюдения [CL Ч без явлений, BR Ч дымка, FG Ч туман, RA Ч дождь SHRA Ч ливневый дождь, SNRA Ч снег с дождем, SN Ч снег, SHSN Ч ливневый снег, TS Ч гроза, DZ Ч морось, FZ Ч гололед, HL Ч град];
# N Ч количество облаков [0 Ч ясно, 1 Ч малооблачно (1-2 балла), 3 Ч небольшая облачность (3-4), 6 Ч переменная (5-7 баллов), 7 Ч обл. с прояснениями (8-9), 9 Ч сплошная (10 баллов)];
# vv Ч метеорологическая дальность видимости [км];
# U Ч относительная влажность воздуха [%]; 
# PPP Ч атмосферное давление на уровне моря [гѕа].


# подготовка перед парсингом файлов
files <- list.files(path = "./_Raw data/погода/", pattern = ".*csv")
weather <- test
weather <- weather[1,1:10]
weather <- cbind(weather, "year", "month")
weather <- weather[FALSE,]

# скачиваем все файлы и парсим год и месяц из имени файла
for (i in 1:length(files)) {
  tyear <- gsub(pattern = "([0-9]{5}_)|(-\\d{1,2}\\.csv)", x=files[i], replacement = "")
  tmonth <- gsub(pattern = "([0-9]{5}_[0-9]{4}-(0)?)|(.csv)", x=files[i], replacement = "")
  temp <- read.csv(paste0("./_Raw data/Погода/",files[i]), sep = ";", header = F)
  temp <- cbind(temp, "year", "month")
  temp[,11] <- rep(tyear, len = nrow(temp))
  temp[,12] <- rep(tmonth, len = nrow(temp))
  weather <- rbind(weather,temp)
}
# переименовываем столбцы
names(weather) <- c("day", "hour", "tem", "windd", "winds", "rainy", "clowdly","vv", "humidity", "ppp", "year", "month")

# анализ пропущенных и неформатных значений
checkna(weather) # clowdly - 545, остальные по мелочи 4-8 строк
table(weather$windd, useNA = "ifany")
## в направлении ветра есть странные значения с "?", но их немного и мы заменим их на NA
## также есть 4917 пустых значений, их также зам. на NA
is.na(weather$windd[grepl("\\?", weather$windd)]) <- T
# зададим новые уровни фактора (и сделаем упорядоченным для дальн.группировки), 
# все что вне уровней получат значения NA
weather$windd <- factor(x = weather$windd, levels = c("Восточный", "Западный", "С-В", "С-З", "Северный", "Ю-В", "Ю-З", "Южный"),ordered = T)

table(weather$winds) # есть неформ. "Штиль" - заменим на 0, зн-я выше 21 (их всего 7) заменим на
## NA, значения "???" (39 шт) заменим на NA
weather$winds <- as.character(weather$winds)
weather$winds[grepl("Штиль", weather$winds)] <- "0"
is.na(weather$winds[grepl("\\?", weather$winds)]) <- T
is.na(weather$winds[weather$winds %in% c("21", "30", "57", "60", "99")]) <- T
## преобразуем winds в числовой
weather$winds <- as.numeric(weather$winds)


# данные надо усреднить - группировать по параметру day+year+month
# темп - средняя за 9,12,15,18-часовые даннные
# облачность - среднее за 9,12,15,18-часовые даннные 

t1 <- subset(weather, weather$hour>=9 & weather$hour<=18)
t2 <- group_by(t1, day, month, year)
weather2 <- summarise(t2, tem=mean(tem, na.rm = T), clowdly=mean(clowdly, na.rm = T), 
                      winds=mean(winds, na.rm=T), windd=max(windd, na.rm=T),
                      humidity=mean(humidity, na.rm=T), ppp=mean(ppp, na.rm=T))
# было 39 предупреждений, о том, что в группах для windd не было отличных от NA значений
#  "no non-missing arguments to max; returning -Inf" - уточнил: это значит будут NA

# проверка NA
checkna(weather2)
# хороший результат:
# day    month     year      tem  clowdly    winds    windd humidity      ppp 
# 0        0        0        0       11        0       39        0        0

weather2 <- as.data.frame(weather2)

# обработка облачности
# для clowdly надо оставить 3 варианта - ясно (0,1), средне (3,6), облачно (7,9)

weather2$clowdlyf <- as.character(weather2$clowdly)

# with(tmp, clowdly <- as.character(clowdly)) - почему то не сработала with !!!!!

weather2$clowdlyf[(weather2$clowdly<=3) & !is.na(weather2$clowdly)]<- 'ясно'
weather2$clowdlyf[(weather2$clowdly > 3 & weather2$clowdly <= 7) & !is.na(weather2$clowdly)]<- 'средне'
weather2$clowdlyf[(weather2$clowdly > 7 & weather2$clowdly < 9) & !is.na(weather2$clowdly)]<- 'облачно'
weather2$clowdlyf[(weather2$clowdly >= 9) & !is.na(weather2$clowdly)]<- 'пасмурно'

weather2$clowdlyf <- factor(weather2$clowdlyf, levels = c("пасмурно", "облачно", "средне", "ясно"),
                            ordered = T)
# для проверки статья о количестве ясных дней в РБ
# http://www.aif.by/questions/item/23047-bel.html

# добавление даты
weather2$date <- weather2$day

for (i in 1:nrow(weather2)) {
  t1 <- weather2[i,1]; if (nchar(t1)==1) {t1 <- paste0("0",t1)}
  t2 <- weather2[i,2]; if (nchar(t2)==1) {t2 <- paste0("0",t2)} 
  weather2[i,"date"] <- paste0(weather2[i,3],"-", t2,"-",t1)
}
weather2$date <- as.Date(weather2$date)



# Разведочный анализ ------------------------------------------------------

weather$season <- as.factor(season(weather$date))
weathertmp <- weather[weather$year %in% c("2013", "2014", "2015"),]
weathertmp$year <- as.factor(weathertmp$year)
densityplot(~tem | year+season, data = weathertmp, layout = c(3, 4), scales = "free", breaks = 50)

ggplot(data = weather2, aes(x = tem, y=humidity)) + geom_point() # темп. и влажность есть корр
ggplot(data = weather2, aes(x = windd, y=tem)) + geom_boxplot()
ggplot(data = weather2, aes(x = tem, y=ppp)) + geom_point()
ggplot(data = weather2, aes(x = humidity, y=ppp)) + geom_point()


# матрица попарных корреляций
scatterplotMatrix(weather2[,c(4:9)])

# сохранение финальных таблиц на диске -------------------------
dput(x = weather, file = "./dump/weather")
dput(x = weather2, file = "./dump/weather2")
# чтение финальных таблиц с диска
weather <- dget(file = "./dump/weather")
weather2 <- dget(file = "./dump/weather2")


