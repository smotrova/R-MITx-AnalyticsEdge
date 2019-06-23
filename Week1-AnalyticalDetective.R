mvt <- read.csv("./DataFiles/mvtWeek1.csv")

rm(mvtWeek1)

str(mvt)

mvt$ID[which.max(mvt$ID)]
mvt$Beat[which.min(mvt$Beat)]

table(mvt$Arrest)
length(mvt$LocationDescription[mvt$LocationDescription =='ALLEY'])

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
str(DateConvert)
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)

mvt$Date = DateConvert


table(mvt$Month)
table(mvt$Weekday)

table(mvt$Month[mvt$Arrest == 'TRUE'])

hist(mvt$Date, breaks=100)

boxplot(mvt$Date)

date_arrest <- subset(mvt$Date, mvt$Arrest )
str(date_arrest)
boxplot(date_arrest, col = 'cyan')

table(mvt$Year, mvt$Arrest)

# 2007
1212/(1212+13068)

# 2012
550/(550+13542)

sort(table(mvt$LocationDescription))

Top5 = subset(mvt, LocationDescription == 'STREET'| 
                  LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)'|
                  LocationDescription == 'ALLEY'|
                  LocationDescription == 'GAS STATION'|
                  LocationDescription == 'DRIVEWAY - RESIDENTIAL')

str(Top5$LocationDescription)
Top5$LocationDescription = factor(Top5$LocationDescription)

str(Top5$LocationDescription)

table(Top5$Arrest, Top5$LocationDescription)
249/(2059+249)
132/(1543+132)
439/(1672+439)
1603/(13249+1603)
11595/(11595+144969)

table(Top5$Weekday, Top5$LocationDescription)
