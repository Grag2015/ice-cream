
# разведочный анализ ---------------------------------------

# плотность в разбивке сезон+год
ggplot(retaildaily, aes(x=saleskg, col=impuls)) + geom_density(alpha = 0.5) + facet_grid(season ~ year, scales = "free")

ggplot(retaildaily[retaildaily$season=="spring",], aes(x=saleskg, col=impuls))+ geom_density(alpha = 0.5)

summary(retaildaily[retaildaily$season=="spring",]$saleskg)

# боксплоты по импульсу
ggplot(data = retaildaily, aes(x = impuls, y = saleskg)) + geom_boxplot()
# вывод - семейное и весовое гораздо меньше в кг продается (чем импульсного), их пока выключим из анализа

# далее работаем только с импульсным
retaildaily2 <- retaildaily[retaildaily$impuls=="ИМПУЛЬСНОЕ",]

# summary по сезонам
tapply(c("autumn","winter","summer","spring"),c("autumn","winter","summer","spring"),
function(e) tapply(retaildaily2[retaildaily2$season==e,]$saleskg, 
                   retaildaily2[retaildaily2$season==e,]$year, summary))

# для скалярной функции результат можно получить проще
tapply(retaildaily$saleskg, retaildaily[,c(4,8)], min)

# диагр.расс.  продажи по годам + сезонам 
densityplot(~saleskg | year+season, data = retaildaily2, layout = c(3, 4), scales = "free", breaks = 50)



# дисперсионный анализ ----------------------------------------------------

# ?????? ????????? ?????? ? ?????
retaildaily <- dget(file = "./dump/retaildaily")

retaildaily$season <- factor(retaildaily$season)

# значимая зависимость от сезона :)
fit <- aov(saleskg ~ season, data = retaildaily2)
summary(fit)

TukeyHSD(fit) # сильные межгрупповые различия, наименее различимы осень и зима

# тест на гомогенность дисперсии
bartlett.test(saleskg ~ wend, retaildaily2)
fit <- aov(saleskg ~ wend, data = retaildaily2)
summary(fit)
ggplot(retaildaily, aes(x=wend, y=saleskg)) + geom_boxplot( )

temp <- retaildaily[retaildaily$saleskg<5000,]
ggplot(temp, aes(saleskg, fill = wend ))+
  geom_density(alpha = 0.5) + facet_grid(season ~ ., scales = "free")

densityplot(~saleskg | wend+season, data = temp, layout = c(2, 4), 
            scales = "free", breaks = 50)

# ???????? ??????????? ??? ??????
bartlett.test(saleskg ~ wdays, retaildaily)
fit <- aov(saleskg ~ wdays, data = retaildaily)
summary(fit)
#  ?? ???????? ????????? ?????????? ???????? ??????????? ?????? ????? ?????????? ?????
# ??-??, ??-??, ??-??, ??-??. - ?.?. ??? ??????????? saleskg ~ wdays ??? ????????? ??????? ?????? ? ??.
TukeyHSD(fit)
ggplot(retaildaily, aes(x=wdays, y=saleskg)) + geom_histogram(stat="identity") + facet_grid( season ~ .,  scales = "free")


# ??????? -----------------------------------------------------------------

ggplot(tt, aes(x=days, y=saleskg)) + geom_line( )+  facet_grid( year ~ .)

# ???????? ?? ???? ? ???????? ?? ?????
ggplot(tt, aes(x=days, y=saleskg)) + geom_line(stat="identity") + facet_grid( year ~ .)

# ?? ???? ??????

# ???????? ?????? ????????? ???????????
ggplot(tt, aes(x=saleskg)) + geom_histogram(binwidth=180)+ facet_grid( season ~ .,  scales = "free")


# условия применения дисперсионного анализа -------------------------------------

# нормальность
# гомогенность дисперсий
# 

tapply(retaildaily2$saleskg, retaildaily2$season, min)
checkna(retaildaily2)
summary(retaildaily$saleskg)
min(retaildaily$saleskg)

# добавляю температуру в retaildaily2----------------------------------------------------
length(unique(weather$date))
temp <- merge(x=retaildaily2, y=weather[,c(4:7)], by.x = "date", by.y = "date", all.x = T, all.y = F)
retaildaily2 <- temp

# линейная регрессионная модель

fit <- lm(saleskg ~ tem*season, data = retaildaily2)
summary(fit)

temp <- retaildaily2[!is.na(retaildaily2$clowdlyf),]
fit <- lm(saleskg ~ tem*season*clowdlyf, data = temp)
summary(fit)

ggplot(data = retaildaily2, aes(x=clowdlyf, y=saleskg)) + geom_boxplot() +facet_grid(season~., scales = "free")

fit <- lm(saleskg ~ clowdly*season, data = retaildaily2)
summary(fit)

fit <- lm(saleskg ~ clowdly*season*tem, data = retaildaily2)
summary(fit) #Multiple R-squared:  0.7769,	Adjusted R-squared:  0.7735
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")

fit <- lm(saleskg ~ season*tem, data = retaildaily2)
summary(fit) #Multiple R-squared:  0.7754,	Adjusted R-squared:  0.7738
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")

fit <- lm(saleskg ~ clowdlyf*season*tem, data = retaildaily2)
summary(fit) #Multiple R-squared:  0.7778,	Adjusted R-squared:  0.7722
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")

# автоматический подбор модели
model_full <- lm(saleskg ~ clowdly*tem*season, data = retaildaily2) 
model_null <- lm(saleskg ~ clowdly, data = retaildaily2)
ideal_model <- step(object = model_full, scope = list(lower = model_null, upper = model_full), direction = 'backward')
summary(ideal_model)

# проверка модели 
x <- gvlma(ideal_model)
summary(x)

# визуальный осмотр
ggplot(data = retaildaily2, aes(x=tem, y=saleskg, col=clowdlyf)) + facet_grid(season~., scales = "free") + geom_point()

# визуальный осмотр показал, что предварительно только лето можно описать линейной моделью
# остальные сезоны надо описывать квадратичной зависимостью.
# попробуем построить модель отдельно для каждого сезона


# лето --------------------------------------------------------------------

summer <- retaildaily2[retaildaily2$season=="summer",]
fit <- lm(saleskg ~ tem, data = summer) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = summer, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)

summary(fit$residuals)

# т.о. модель для лета показала
# Multiple R-squared:  0.4415,	Adjusted R-squared:  0.4395 
# при это тест на нормальность остатков (0.002824001) и гетероскедастичность не прошла (0.0012913)

# осень -------------------------------------------------------------------

autumn <- retaildaily2[retaildaily2$season=="autumn" & !is.na(retaildaily2$tem),]
fit <- lm(saleskg ~ I(tem^3)+I(tem^2)+tem, data = autumn) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = autumn, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# Шапиро-тест показывает 1.016055e-07, но гистограмма в целом похожа на нормальную

# весна -------------------------------------------------------------------

spring <- retaildaily2[retaildaily2$season=="spring" & !is.na(retaildaily2$tem),]
fit <- lm(saleskg ~ I(tem^3)+I(tem^2)+tem, data = spring) 
summary(fit)
x <- gvlma(fit)
summary(x)

# визуальный осмотр
ggplot(data = spring, aes(x=tem, y=saleskg))  + geom_point(col="red") + 
  geom_line(aes(x = tem, y = fit$fitted.values), col = 'blue', lwd=1)

# проверка остатков на нормальность
ifelse(shapiro.test(fit$residuals)$p.value < 0.05, shapiro.test(fit$residuals)$p.value, "Yes")
hist(fit$residuals, breaks = 50)
# Шапиро-тест показывает 5.685623e-15, но гистограмма в целом похожа на нормальную

qqnorm(fit$residuals)
qqline(fit$residuals)

# есть связь ясности и температуры, поэтому видимо ясность не сильно увеличивает коэфф.детер. модели
ggplot(data = retaildaily2, aes(x=clowdlyf, y=tem)) + geom_boxplot() +facet_grid(season~., scales = "free")

checkna(autumn)
pairs(autumn[c(3,4,10,11)])
