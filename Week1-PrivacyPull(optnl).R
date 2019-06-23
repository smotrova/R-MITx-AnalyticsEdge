Poll <- AnonymityPoll
str(Poll)

table(Poll$Smartphone)
summary(Poll$Smartphone)

table(Poll$State, Poll$Region)
table (Poll$Internet.Use, Poll$Smartphone, useNA = 'always')

limited = subset(Poll, Internet.Use == 1 | Smartphone == 1)
summary(limited)

str(limited$Info.On.Internet)
length(limited$Info.On.Internet[limited$Info.On.Internet == 11])

table(Poll$Worry.About.Info)
mean(Poll$Worry.About.Info, na.rm = T)

table(Poll$Anonymity.Possible)
mean(Poll$Anonymity.Possible, na.rm = T)

table(Poll$Privacy.Laws.Effective)
mean(Poll$Privacy.Laws.Effective, na.rm = T)

hist(Poll$Age)

plot(limited$Age, limited$Info.On.Internet)
table(limited$Info.On.Internet, limited$Age)
