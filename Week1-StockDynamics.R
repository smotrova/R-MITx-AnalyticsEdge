IBM = IBMStock
GE = GEStock
ProcterGamble = ProcterGambleStock
CocaCola = CocaColaStock
Boeing = BoeingStock

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(IBM)
summary(IBM)
summary(GE)
summary(CocaCola)
summary(ProcterGamble)
summary(Boeing)

sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, 'l', col = 'red', lwd = 3)
lines(ProcterGamble$Date, ProcterGamble$StockPrice, 'l', col = 'blue', lwd = 3)

# to draw a vertical line at a certain date
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", lwd = 3, col="red", ylim=c(0,210), xlab = 'Date', ylab = 'Stock Price' )
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], 'l', col = 'blue', lwd = 3)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], 'l', col = 'cyan', lwd = 3)
lines(IBM$Date[301:432], IBM$StockPrice[301:432], 'l', col = 'green', lwd = 3)
lines(GE$Date[301:432], GE$StockPrice[301:432], 'l', col = 'magenta', lwd = 3)
abline(v=as.Date(c("1997-10-01")), lwd=2)


tapply(IBM$StockPrice, months(IBM$Date), mean)

tapply(GE$StockPrice, months(GE$Date), mean)
