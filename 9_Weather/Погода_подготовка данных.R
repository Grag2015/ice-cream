getwd()
setwd("D:/Grag/R/R-studio/Ice-creame")
test <- read.csv("./Raw data/ѕогода/26850_2006-01.csv", sep = ";", header = F)

# »сточник данных - http://pogoda.by/zip/index.php?Year=2015#csv
# описание данных 
# „исло мес¤ца
# —рок наблюдений [часы], UTC
# T Ч температура воздуха на высоте 2 м [∞—];
# dd Ч направление ветра на высоте 10 м [румбы, откуда дует];
# FF Ч средн¤¤ скорость ветра на высоте 10 м [м/с];
# ww Ч код погоды в срок наблюдени¤ [CL Ч без ¤влений, BR Ч дымка, FG Ч туман, RA Ч дождь SHRA Ч ливневый дождь, SNRA Ч снег с дождЄм, SN Ч снег, SHSN Ч ливневый снег, TS Ч гроза, DZ Ч морось, FZ Ч гололЄд, HL Ч град];
# N Ч количество облаков [0 Ч ¤сно, 1 Ч малооблачно (1-2 балла), 3 Ч небольша¤ облачность (3-4), 6 Ч переменна¤ (5-7 баллов), 7 Ч обл. с про¤снени¤ми (8-9), 9 Ч сплошна¤ (10 баллов)];
# vv Ч метеорологическа¤ дальность видимости [км];
# U Ч относительна¤ влажность воздуха [%]; 
# PPP Ч атмосферное давление на уровне мор¤ [гѕа].


# подготовка перед парсингом файлов
files <- list.files(path = "./Raw data/ѕогода/", pattern = ".*csv")
weather <- test
weather <- weather[1,1:10]
weather <- cbind(weather, "year", "month")
weather <- weather[FALSE,]

# скачиваем все файлы и парсим год и мес¤ц из имени файла
for (i in 1:length(files)) {
  tyear <- gsub(pattern = "([0-9]{5}_)|(-\\d{1,2}\\.csv)", x=files[i], replacement = "")
  tmonth <- gsub(pattern = "([0-9]{5}_[0-9]{4}-(0)?)|(.csv)", x=files[i], replacement = "")
  temp <- read.csv(paste0("./Raw data/ѕогода/",files[i]), sep = ";", header = F)
  temp <- cbind(temp, "year", "month")
  temp[,11] <- rep(tyear, len = nrow(temp))
  temp[,12] <- rep(tmonth, len = nrow(temp))
  weather <- rbind(weather,temp)
}
# переименовываем столбцы
names(weather) <- c("day", "hour", "tem", "windd", "winds", "rainy", "clowdly","vv", "humidity", "ppp", "year", "month")



# данные надо усреднить - группировать по параметру day+year+month
# темп - средн¤¤ за 9,12,15,18-часовые даннные
# облачность - среднее за 9,12,15,18-часовые даннные 

t1 <- subset(weather, weather$hour>=9 & weather$hour<=18)
t2 <- group_by(t1, day, month, year)
weather2 <- summarise(t2, mean(tem, na.rm = T), mean(clowdly, na.rm = T))
names(weather2)[4:5] <- c("tem", "clowdly")
# t2$clowdlysrc <- as.numeric(t2$clowdlysrc)
# sum(is.na(weather2$clowdly))

# обработка облачности

# weather2$clowdly <- round(weather2$clowdly)
# дл¤ clowdly надо оставить 3 варианта - ¤сно (0,1), средне (3,6), облачно (7,9)

weather2 <- cbind(weather2, weather2$clowdly)
names(weather2)[6] <- "clowdlyf"

# with(tmp, clowdly <- as.character(clowdly)) - почему то не сработала with !!!!!
# weather$clowdly <- as.character(weather$clowdly)
# weather$clowdlysrc <- as.character(weather$clowdlysrc)

weather2$clowdlyf[(weather2$clowdly<=3) & !is.na(weather2$clowdly)]<- '¤сно'
weather2$clowdlyf[(weather2$clowdly > 3 & weather2$clowdly <= 7) & !is.na(weather2$clowdly)]<- 'средне'
weather2$clowdlyf[(weather2$clowdly > 7) & !is.na(weather2$clowdly)]<- 'облачно'

# добавление даты
weather2 <- cbind(weather2, weather2$day)
names(weather2)[7] <- "date"
for (i in 1:nrow(weather2)) {
  t1 <- weather2[i,1]; if (nchar(t1)==1) {t1 <- paste0("0",t1)}
  t2 <- weather2[i,2]; if (nchar(t2)==1) {t2 <- paste0("0",t2)} 
  weather2[i,7] <- paste0(weather2[i,3],"-", t2,"-",t1)
}
weather2$date <- as.Date(weather2$date)
plot(x = weather2$date, y = weather2$tem)

#
