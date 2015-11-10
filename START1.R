# для каждого проекта создаем свою рабочую папку
wd_old <- getwd()
dir_new <- "Ice-creame" # введите имя рабочей папки
wd_new <- paste0("D:/Grag/R/R-studio/", dir_new)
setwd("D:/Grag/R/R-studio/Ice-cream")
# создаем папку для сырых данных 
if (!file.exists("Raw_dir")) {
  dir.create("Raw_dir")
}
# очистка памяти   ----------------------------
ls()
rm(list = ls())

# загрузка сырых данных

# предварительное изучение данных (без изменений) ---------------------
str(plants) 
summary(plants)
head()
table(plants$Active_Growth_Period) # for factors

plot(cars)
boxplot(formula = mpg ~ cyl, data = mtcars)
hist(mtcars$mpg) #  best used by just passing in a single vector

# сохранение промежуточных таблиц на диске
dump(c("yukki_kg", "yukki_dt"), "./dump/otgr_sales.R")
# чтение промежуточных таблиц с диска
source("./dump/weather.R")


# подключение библиотек -------------------------
library('stringr')
library('ggplot2')
library('xlsx')
# для ф-и GET и др.
library(httr)
# для  парсинга HTML и XML
library(XML)
# для функции group_by, select, summarize
library("dplyr")
# для функции melt
library("data.table")
# для 2-й графической системы
library(lattice)
# HDF5
library(rhdf5)
# для регулярок
library(stringi)

# для проверки регресс. модели
library(car)
library(gvlma)

# новое ----------------------
# ИЗУЧИТЬ !!! The sqldf package allows for execution of SQL commands on R data frames.
# примеры запросов
sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")

# очистка данных (это важный момент, должен быть скрипт перехода Raw -> Tidy)
# также на выходе должен быть файл "Code book" (руками) "Study design"+преобразования+"Code book"
# Here your tidy datasets are
# смотри пример в работе по GaCD


# Мои функции -------------------------------------------------------------

# проверка наличия NA-значений в датафрейме, на выходе вектор с кол-м значений в кажд.стобце
checkna <- function(x){
  sapply(x, function(e) sum(is.na(e)))
}

# Выбросами будем считать те наблюдения, которые отклоняются от 1 или 3 квартиля больше чем на 1,5 *  IQR, 
# где  IQR  - межквартильный размах.
# На вход функция получает числовой вектор x. Функция должна возвращать модифицированный вектор x 
# с удаленными выбросами.
# доп. параметр log, если log=F - возвращает вектор значений без выбросов, 
# иначе - логический вектор: F - невыброс, T - выброс

outliers.rm <- function(x, log=F){
  # put your code here  
  # IQR(x) - рассчитывает межквартильный размах вектора x
  # quantile(x, probs = c(0.25, 0.75)) - # рассчитывает первый и третий квартиль вектора x 
  mn <- quantile(x, probs = c(0.25, 0.75))[1]-abs(1.5*IQR(x))
  mx <- quantile(x, probs = c(0.25, 0.75))[2]+abs(1.5*IQR(x))
  for (i in 1:length(x)) {
    if (x[i]>mx | x[i]<mn) {
      x[i] <- mx
    }#else
  }
  ifelse(log, rez <- !x<mx, rez <- x[x<mx])
  rez
}

# на входе вектор дат, на выходе вектор сезонов фактор

season <- function(x){
  y <- month(x) # month(x) from package  lubridate
  out <- character(length = length(x))
  out[y>2 & y<6] <- "spring"
  out[y>5 & y<9] <- "summer"
  out[y>8 & y<12] <- "autumn"
  out[y>11 | y<3] <- "winter"
  out
}
