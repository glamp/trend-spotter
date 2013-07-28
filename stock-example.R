
# data from: http://pages.swcp.com/stocks/
data <- read.csv("./sp500hst.txt", h=T)
head(data)

data.small <- subset(data, Ticker %in% c("GOOG", "AAPL", "GE"))[,c("Date", "Ticker", "Close")]
data.small <- data[,c("Date", "Ticker", "Close")]
data.small <- dcast(data.small, Date ~ Ticker)
# data.small$Date <- as.Date(data.small$Date, format="%Y%m%d")
summary(data.small$Date)
head(data.small)

# get rid of tickers that are largely null
pct.nas <- sapply(data.small, function(x) sum(is.na(x)) / length(x))
data.small <- data.small[,pct.nas > .01]

# looking at the data...
head(data.small)
# tickers we have available...
colnames(data.small)
data.lng <- compare.lines(data.small[,-1], "DELL")

p <- ggplot(aes(x=x, y=value),
            data=subset(data.lng, variable %in% levels(data.lng$variable)[1:12]))
p + geom_point() +
  geom_line() +
  facet_wrap(~variable, scales="free")

p + stat_smooth(span=.5, se=F) +
  facet_wrap(~variable, scales="free")

