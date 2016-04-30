library(dplyr)
library(lubridate)
if(!exists("dat")) {
  dat <- read.table(unz("activity.zip", "activity.csv"), sep=",", header=TRUE, stringsAsFactors = FALSE)
}

anyValueIsNa <- is.na(dat$steps) | is.na(dat$date) | is.na(dat$interval)
totalNas <- sum(anyValueIsNa)
dat$dayOfWeek <- wday(dat$date)
meanIntervalByWeekday <- tapply(dat$steps, interaction(dat$interval, dat$dayOfWeek), mean, na.rm = TRUE)

typicalDay <- data.frame(steps = meanIntervalByWeekday, intervalDay = names(meanIntervalByWeekday))
typicalDay <- mutate(typicalDay, intervalDay = as.character(intervalDay))


imputeValues <- function(df) {
  intervalDayInput <- paste(as.character(df$interval), as.character(df$dayOfWeek), sep =  ".")
  df$intervalDay = intervalDayInput
  print(head(unique(df$intervalDay)))
  df <- df %>% left_join(typicalDay, c("intervalDay" = "intervalDay"))
  
  df <- rename(df, steps = steps.x, imputedValue = steps.y)
  
  ixesToUpdate <- which(is.na(df$steps))
  
  df$steps[ixesToUpdate] <- df$imputedValue[ixesToUpdate]
  print(head(df))
  df
}
withImputed <- dat
withImputed <- imputeValues(withImputed)

