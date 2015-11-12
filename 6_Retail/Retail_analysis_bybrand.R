# берем только импульсное, группируем по брендам и датам
temp <- retail[retail$impuls=="ИМПУЛЬСНОЕ",]
# контрольная сумма
sum(temp$saleskg) #1167130

tt <- group_by(temp, date, BRAND)
tt2 <- summarise(tt, value=sum(value), saleskg=sum(saleskg))

# контрольная сумма
sum(tt2$saleskg) #1167130 - ок
retaildaily_b <- data.frame(tt2)

# добавляем сезон, температуру и облачность
retaildaily_b$season <- as.factor(season(retaildaily_b$date))
temp <- merge(x=retaildaily_b, y=weather[,4:7], by = "date", all.x = T, all.y = F)
retaildaily_b <- temp
checkna(retaildaily_b) # по 20 NA в каждом бренде - это из новых значений продаж
# пока их выкину, потом добавлю недостающие температуры
retaildaily_b <- retaildaily_b[!is.na(retaildaily_b$clowdly),]
sum(retaildaily_b$saleskg) #1159474 - ок

# добавляем ветер (скор и направл), влажность, давление из перем. weather2
temp <- merge(x=retaildaily_b, y=weather2[,c("winds", "windd", "humidity", "ppp", "date")], 
              by.x = "date", by.y = "date", all.x = T, all.y = F)
sum(retaildaily_b$saleskg)==sum(temp$saleskg)
retaildaily_b <- temp

# визуальный осмотр ----------------
temp <- retaildaily_b[retaildaily_b$saleskg<500,]
ggplot(temp, aes(x=saleskg, col=BRAND)) + geom_density() +facet_grid(season~., scales = "free")


ggplot(retaildaily_b, aes(x=tem, y=saleskg, col=BRAND))+geom_point()+facet_grid(season~., scales = "free")
# хорошо видно как расслаиваются диаг.расс по цвету - каждый бренд занимает свой уровень
# есть шанс построить хорошую модель отдельно по каждому бренду

# Soletto -----------------------------------------------------------------

soletto <- retaildaily_b[retaildaily_b$BRAND=="Soletto",]


# Soletto лето ------------------------------------------------------------

summer1 <- soletto[soletto$season=="summer",]
fit <- lm(saleskg ~ tem, data = summer1) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = summer1, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# модель для лета стала лучше
# Multiple R-squared:  0.5205,	Adjusted R-squared:  0.5187 
# Heteroscedasticity   0.1034993 - ОК
# Шапиро - 0.01619734 - почти ОК

# Soletto весна ------------------------------------------------------------

spring1 <- soletto[soletto$season=="spring",]
fit <- lm(saleskg ~ tem+I(tem^2), data = spring1) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = spring1, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# модель для весны солетто осталась примерно на уровне общей модели
# Multiple R-squared:  0.6576,	Adjusted R-squared:  0.6551 
# Heteroscedasticity   0.1358 - ОК
# Шапиро - 3.125317e-06 - не ок

# Soletto осень ------------------------------------------------------------

autumn1 <- soletto[soletto$season=="autumn",]
fit <- lm(saleskg ~ tem+I(tem^3), data = autumn1) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = autumn1, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# модель для осени солетто осталась примерно на уровне общей модели
# Multiple R-squared:  0.6813,	Adjusted R-squared:  0.6783  
# Heteroscedasticity   6.082e-01 - ОК
# Шапиро - 5.663426e-15 - не ок

sd(fit$residuals)/mean(autumn1$saleskg)

# Soletto зима  ------------------------------------------------------------

tmpseasonstr <- "winter"

tmpseason <- soletto[soletto$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem+I(tem^2)+I(tem^3), data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(aes( col=factor(clowdlyf))) + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# модель для зимы солетто осталась примерно на уровне общей модели ??
# Multiple R-squared:  0.1141,	Adjusted R-squared:  0.1014  
# Heteroscedasticity   8.377e-02 - ОК
# Шапиро - 3.54837e-09 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ТОП -----------------------------------------------------------------

top <- retaildaily_b[retaildaily_b$BRAND=="ТОП",]

# ТОП лето  ------------------------------------------------------------

tmpseasonstr <- "summer"

tmpseason <- top[top$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem, data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# модель для ТОП не очень вышла (надо видимо разбивать на подбренды)
# Multiple R-squared:  0.3199,	Adjusted R-squared:  0.3174   
# Heteroscedasticity   5.040e-03 - не ОК
# Шапиро - 9.4637e-08 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ТОП весна  ------------------------------------------------------------

tmpseasonstr <- "spring"

tmpseason <- top[top$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem+I(tem^3), data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# лучше чем лето ТОП, но хуже весны солетто
# Multiple R-squared:  0.5765,	Adjusted R-squared:  0.5734   
# Heteroscedasticity   0.05465 - ОК
# Шапиро - 1.626576e-15 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ТОП осень  ------------------------------------------------------------

tmpseasonstr <- "autumn"

tmpseason <- top[top$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem+I(tem^3), data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# как и весна
# Multiple R-squared:  0.5689,	Adjusted R-squared:  0.5647    
# Heteroscedasticity   3.415e-01 - ОК
# Шапиро - 7.486981e-08 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ТОП зима  ------------------------------------------------------------

tmpseasonstr <- "winter"

tmpseason <- top[top$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem, data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# зима у всех плохо смотрится
# Multiple R-squared:  0.1016,	Adjusted R-squared:  0.09737    
# Heteroscedasticity   8.435e-0 - ОК
# Шапиро - 3.316069e-05 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ЮККИ -----------------------------------------------------------------
yukki <- retaildaily_b[retaildaily_b$BRAND=="ЮККИ",]

# ЮККИ лето  ------------------------------------------------------------

tmpseasonstr <- "summer"

tmpseason <- yukki[yukki$season==tmpseasonstr,]
fit <- lm(I(saleskg^(0.3)) ~ tem, data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# нормальность зависимой переменной
shapiro.test(sqrt(tmpseason$saleskg))$p.value
par(mfrow=c(2,1))
hist(tmpseason$saleskg, breaks = 50)
hist(sqrt(tmpseason$saleskg), breaks = 50)
library(car)
summary(powerTransform(tmpseason$saleskg))

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="grey", size=4,  alpha = 1/2) + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)+
  geom_line(aes(x = tem, y = fit$fitted.values+1000), col = 'red',  linetype = 3)+
  geom_line(aes(x = tem, y = fit$fitted.values-1000), col = 'red',  linetype = 3)

# проверка остатков на нормальность
residuals <- outliers.rm(fit$residuals)
ifelse(shapiro.test(residuals)$p.value < 0.05, shapiro.test(residuals)$p.value, "Yes")
hist(residuals, breaks = 50)

# корректировка зависимой перем. для вып.требов. гетероскедаст.
spreadLevelPlot(fit)

# проверка доли выбросов
sum(outliers.rm(fit$residuals, log = T))/length(fit$residuals)

# 95% интервал для нормальных остатков
tt <- t.test(residuals)
tt$conf.int
summary(residuals)
confint(fit)
sd(residuals)

par(mfrow=c(2,2))
plot(fit)


# вышло среднее между солетто и топ
# Multiple R-squared:  0.4458,	Adjusted R-squared:  0.4438  
# Heteroscedasticity   0.0018676 - почти ОК
# Шапиро - 0.003836674 - почти ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# ЮККИ весна  ------------------------------------------------------------

tmpseasonstr <- "spring"

tmpseason <- yukki[yukki$season==tmpseasonstr,]
fit <- lm(saleskg ~ tem+I(tem^3), data = tmpseason) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = tmpseason, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# вышло среднее между солетто и топ
# Multiple R-squared:  0.6357,	Adjusted R-squared:  0.6331 
# Heteroscedasticity   0.6988 -  ОК
# Шапиро - 7.518991e-15 - не ок

sd(fit$residuals)/mean(tmpseason$saleskg)

# сохранение финальных таблиц на диске -------------------------
dput(x = retaildaily_b, file = "./dump/retaildaily_b")
# чтение финальных таблиц с диска
retaildaily_b <- dget(file = "./dump/retaildaily_b")

# готовил данные для отчета rmd
dput(x = retaildaily_b[,c(1,2,4)], file = "./dump/retailtidy")
