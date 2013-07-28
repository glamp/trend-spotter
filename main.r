library(plyr)
library(reshape2)
library(ggplot2)
library(zoo)

normalize <- function(x) {
    x <- na.fill(x, "extend")
    (x - min(x)) / (max(x) - min(x))
}

calc.similarity <- function(df, comp.var) {
  y <- df[,comp.var]
  results <- ldply(df, function(x) {
    y <- normalize(y)
    x <- normalize(x)
    z <- t(cor(x, y))
    z
  })
  names(results) <- c("variable", "distance")
  results[order(results$distance, decreasing=T),]
}


# generate some fake data
df <- data.frame(
  sapply(1:10, function(i) {
    cumsum(runif(10, -1, 1))
  })
)


compare.lines <- function(df, comp.var, format="long") {
  # compare each line to each other line
  df.cmp <- calc.similarity(df, comp.var)
  # add a dummy column for an x dimension (this would be time)
  df$x <- 1:nrow(df)
  # get into long format
  df.lng = melt(df, id="x")
  # re-order the variables based on their distance from the line we're investigating
  df.lng$variable <- factor(df.lng$variable, levels=df.cmp$variable)
  if (format=="long") {
    df.lng 
  } else {
    df
  }
}
p <- ggplot(aes(x=x, y=value), data=df.lng)
p + geom_point() + geom_line() + facet_wrap(~variable, scales="free")

p <- ggplot(aes(x=x, y=value),
            data=subset(df.lng, variable %in% head(df.cmp$variable, 5)))
p + geom_point() + geom_line() + facet_wrap(~variable, ncol=1, scales="free")

p <- ggplot(aes(x=x, y=value, colour=variable),
            data=subset(df.lng, variable %in% head(df.cmp$var, 3)))
p + geom_point() + geom_line()

