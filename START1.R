# ��� ������� ������� ������� ���� ������� �����
wd_old <- getwd()
dir_new <- "Ice-creame" # ������� ��� ������� �����
wd_new <- paste0("D:/Grag/R/R-studio/", dir_new)
setwd("D:/Grag/R/R-studio/Ice-creame")
# ������� ����� ��� ����� ������ 
if (!file.exists("Raw_dir")) {
  dir.create("Raw_dir")
}
# ������� ������   ----------------------------
ls()
rm(list = ls())

# �������� ����� ������

# ��������������� �������� ������ (��� ���������) ---------------------
str(plants)
summary(plants)
head()
table(plants$Active_Growth_Period) # for factors

plot(cars)
boxplot(formula = mpg ~ cyl, data = mtcars)
hist(mtcars$mpg) #  best used by just passing in a single vector

# ���������� ������������� ������ �� �����
dump(c("yukki_kg", "yukki_dt"), "./dump/otgr_sales.R")
# ������ ������������� ������ � �����
source("./dump/weather.R")


# ����������� ��������� -------------------------
library('stringr')
library('ggplot2')
library('xlsx')
# ��� �-� GET � ��.
library(httr)
# ���  �������� HTML � XML
library(XML)
# ��� ������� group_by, select, summarize
library("dplyr")
# ��� ������� melt
library("data.table")
# ��� 2-� ����������� �������
library(lattice)
# HDF5
library(rhdf5)
# ��� ���������
library(stringi)

# ����� ----------------------
# ������� !!! The sqldf package allows for execution of SQL commands on R data frames.
# ������� ��������
sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")

# ������� ������ (��� ������ ������, ������ ���� ������ �������� Raw -> Tidy)
# ����� �� ������ ������ ���� ���� "Code book" (������) "Study design"+��������������+"Code book"
# Here your tidy datasets are
# ������ ������ � ������ �� GaCD


