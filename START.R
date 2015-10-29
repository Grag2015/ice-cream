# дл€ каждого проекта создаем свою рабочую папку
wd_old <- getwd()
dir_new <- "Ice-creame" # введите им€ рабочей папки
wd_new <- paste0("D:/Grag/R/R-studio/", dir_new)
setwd("D:/Grag/R/R-studio/Ice-creame")
# создаем папку дл€ сырых данных 
if (!file.exists("Raw_dir")) {
  dir.create("Raw_dir")
}
# очистка пам€ти 
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
# дл€ ф-и GET и др.
library(httr)
# дл€  парсинга HTML и XML
library(XML)
# дл€ функции group_by, select, summarize
library("dplyr")
# дл€ функции melt
library("data.table")
# дл€ 2-й графической системы
library(lattice)
# HDF5
library(rhdf5)
# дл€ регул€рок
library(stringi)

# новое ----------------------
# »«”„»“№ !!! The sqldf package allows for execution of SQL commands on R data frames.
# примеры запросов
sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")

# очистка данных (это важный момент, должен быть скрипт перехода Raw -> Tidy)
# также на выходе должен быть файл "Code book" (руками) "Study design"+преобразовани€+"Code book"
# Here your tidy datasets are

