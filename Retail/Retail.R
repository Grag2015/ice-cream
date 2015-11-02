# исходный файл морож 13-15.xlsx был преобразован в excel (удалены промежуточные итоги и лишние строки и столбцы)
# см. морож 13-15._tidy_step1.xlsx, далее все 5 листов были экспортированы в файлы csv


# 1. Загрузка и предобработка данных  ------------------------------------------

# загрузка 1
tmp1 <- read.csv("./Raw data/Retail/25янв 13 - 12авг13.csv", header = T, dec = ',', sep = ";")
# обработка дат
newnames <- paste0(substr(names(tmp1[,3:ncol(tmp1)]),2,6),'.',"2013")
names(tmp1) <- c(names(tmp1[,1:2]),newnames)
names(tmp1)[2] <- "Наименование.товара1"

# загрузка 2
tmp2 <- read.csv("./Raw data/Retail/13авг13 - 28фев14.csv", header = T, dec = ',', sep = ";")
# обработка дат

ny <- which(names(tmp2)=="X01.01.01.01142.й.период", arr.ind = T)
newnames <- paste0(substr(names(tmp2[,3:(ny-1)]),2,6),'.',"2013")
newnames2 <- paste0(substr(names(tmp2[,ny:ncol(tmp2)]),2,6),'.',"2014")
names(tmp2) <- c(names(tmp2[,1:2]),newnames, newnames2)
names(tmp2)[2] <- "Наименование.товара2"

# загрузка 3
tmp3 <- read.csv("./Raw data/Retail/01мар14 - 16сен14.csv", header = T, dec = ',', sep = ";")
# обработка дат
newnames <- paste0(substr(names(tmp3[,3:ncol(tmp3)]),2,6),'.',"2014")
names(tmp3) <- c(names(tmp3[,1:2]),newnames)
names(tmp3)[2] <- "Наименование.товара3"

# загрузка 4
tmp4 <- read.csv("./Raw data/Retail/17сен14- 04апр15.csv", header = T, dec = ',', sep = ";")
# обработка дат

ny <- which(names(tmp4)=="X01.01.01.01107.й.период", arr.ind = T)
newnames <- paste0(substr(names(tmp4[,3:(ny-1)]),2,6),'.',"2014")
newnames2 <- paste0(substr(names(tmp4[,ny:ncol(tmp2)]),2,6),'.',"2015")
names(tmp4) <- c(names(tmp4[,1:2]),newnames, newnames2)
names(tmp4)[2] <- "Наименование.товара4"

# загрузка 5
tmp5 <- read.csv("./Raw data/Retail/05апр15 - 20окт15.csv", header = T, dec = ',', sep = ";")
# обработка дат
newnames <- paste0(substr(names(tmp5[,3:ncol(tmp5)]),2,6),'.',"2015")
names(tmp5) <- c(names(tmp5[,1:2]),newnames)
names(tmp5)[2] <- "Наименование.товара5"

# объединяем все таблицы по ключу "код.товара"
temp <- merge(tmp1, tmp2, by="Код.товара", all = T)
temp <- merge(temp, tmp3, by="Код.товара", all = T)
temp <- merge(temp, tmp4, by="Код.товара", all = T)
temp <- merge(temp, tmp5, by="Код.товара", all = T)

names(temp)
tt <- grepl(x=names(temp),pattern = "Наименование.товара")
head(temp[,tt])
temp <- cbind(temp[,tt], temp[,!tt])

# 2. Размещает даты в одной колонке: retail ------------------------------------------

tt <- melt(data = temp, id = 1:6, measure = 7:ncol(temp))
#table(tmp5$Код.товара)

# преобразуем дату в класс Date
names(tt)[7] <- "date"
tt$date <- as.Date(tt$date, "%d.%m.%Y")
retail <- tt
# проверка контрольной суммы -  17 198 386,70   
sum(retail$value,na.rm = T)

# 3. Из Наименования товара надо вытянуть массу: retail$pack + retail$saleskg ----------------------

# table(retail[1,1])
# 1-й парсинг
temp <- stri_extract_first(str = retail[,1], regex = "(\\d+(,\\d+)?( )?(к|К)?(г|Г))|(\\d+,\\d+)|(\\d+/\\d+(,\\d+))")
# поиск пропущенных
tt <- temp[temp==""]
table(tt,useNA="ifany")
sum(is.na(retail[,1]))
table(as.character(retail[is.na(temp),1]))
# удаляем буквы к и г и заменяем , на . и переводим в гр. все

# table(temp)
kg <- grepl("(кг|,)", temp)
tmp <- sub(pattern = "(кг|г|Г|\\d{1}/)", temp, replacement = "")
tmp <- sub(",", ".", tmp)
table(tmp)
temp <- as.numeric(tmp)*ifelse(kg,1000,1)

# проверка
nrow(retail)==length(temp)
# добавляем в ритейл новую переменную
retail$pack <- temp

# добавляем продажи в кг.
retail$saleskg <- retail$value*retail$pack/1000

# финальное преобразование типов столбцов
retail[,1] <- as.character(retail[,1])
retail[,2] <- as.character(retail[,2])
retail[,3] <- as.character(retail[,3])
retail[,4] <- as.character(retail[,4])
retail[,5] <- as.character(retail[,5])
retail[,6] <- as.character(retail[,6])

# смотрим есть ли по выбывшим в первом периоде вернувшиеся в следующих
i <- 2 # перебор от 2 до 5
table(as.character(retail[grepl("ыбыл",retail[,1]),i]))
# результат - нет вернувшихся


# 4. Подключение справочника index отгрузки: retail$INDEX ----------------------------------

temp <- read.csv("./Raw data/Retail/Справочник к мороженому.csv", header = T, sep = ";") 
temp$НОМЕНКЛАТУРА <- as.character(temp$НОМЕНКЛАТУРА)
tmp <- merge(x=retail, y=temp, by.x = "Наименование.товара1", by.y = 'НОМЕНКЛАТУРА', all.x = T)
# проверка
table(tmp$INDEX, useNA = "ifany")
table(tmp[is.na(tmp$INDEX),1])
sum(tmp$value, na.rm = T)

retail <- tmp

# 5. Группировка по дате и предвар.анализ: retaildaily ------------------------------------
temp <- group_by(.data = retail, date)
tt <- summarise(temp, value=sum(value, na.rm = T), saleskg= sum(saleskg, na.rm = T))

# добавление факторов для сгруп. по дате данных
summary(tt$saleskg)
hist(tt$saleskg, xlim = c(0,4000), breaks = 40)  # опять похоже на ф-распределение - асимметр.
hist(log(tt$saleskg), breaks = 40) # похоже на 2 нормальных, видимо нужен разбивающий их фактор
tt$year <- factor(year(tt$date))
tt$days <- yday(tt$date)
tt$wdays <- factor(weekdays(tt$date))
tt$wend <- factor(ifelse(tt$wdays=="суббота" | tt$wdays=="воскресенье", "yes", "no"))
tt$season <- season(tt$date)
retaildaily <- tt
# проверка контрольной суммы -  17 198 386,70  
sum(retaildayli$value)
# сохранение финальных таблиц на диске
dput(x = retaildaily, file = "./dump/retaildaily")
# чтение финальных таблиц с диска
retaildayli <- dget(file = "./dump/retaildaily")


# 6. Добавление справочника ITEM и факторов оттуда ------------------------

tt <- subbrand

# сохранение финальных таблиц на диске -------------------------
dput(x = retail, file = "./dump/retail")
# чтение финальных таблиц с диска
retail <- dget(file = "./dump/retail")

# Мои функции -------------------------------------------------------------

# на входе вектор дат, на выходе вектор сезонов фактор
season <- function(x){
  y <- month(x) # month(x) from package  IDateTime
  out <- character(length = length(x))
  out[y>2 & y<6] <- "spring"
  out[y>5 & y<9] <- "summer"
  out[y>8 & y<12] <- "autumn"
  out[y>11 | y<3] <- "winter"
  out
}


tt 