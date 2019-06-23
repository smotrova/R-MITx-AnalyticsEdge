CPS <- read.csv("./DataFiles/CPSData.csv")

str(CPS)
summary(CPS)

sort(table(CPS$State))

sort(table(CPS$Citizenship)) 
length(CPS$Citizenship)
(7073+116639)/131302

table(CPS$Race, CPS$Hispanic)
match(NA, CPS$Industry)

is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))


table(CPS$Region, is.na(CPS$MetroAreaCode))
table(CPS$State, is.na(CPS$MetroAreaCode))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = MetroAreaCodes
CountryMap = CountryCodes

str(MetroAreaMap)
str(MetroAreaCodes)

str(CPS)
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)

table(is.na(CPS$MetroArea))

table(CPS$MetroArea)

str(CPS$Hispanic)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race == 'Asian', CPS$MetroArea, mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = T))

str(CPS)
str(CountryMap)
CPS = merge(CPS, CountryMap, by.x = 'CountryOfBirthCode', by.y = 'Code', all.x=TRUE)
str(CPS)
table(is.na(CPS$Country))

sort(table(CPS$Country))

tapply(CPS$Country != 'United States', CPS$MetroArea, mean, na.rm =T)

sort(tapply(CPS$Country == 'India', CPS$MetroArea, sum, na.rm = T))

sort(tapply(CPS$Country == 'Somalia', CPS$MetroArea, sum, na.rm = T))

