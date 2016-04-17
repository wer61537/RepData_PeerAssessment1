#clean up objects
rm(list = ls(all = TRUE))
opts_chunk$set(echo = TRUE, results = 'hold')

library(knitr)
library(data.table)
library(ggplot2) 
source("getData.R")

df.activity<-getData()

#verify dataframe is populated
if(!length(df.activity[1,]) > 0L){
  message("Empty dataset so exiting script ....")
  Stop
}

df.activity$date <- as.Date(df.activity$date, format = "%Y-%m-%d")
df.activity$interval <- as.factor(df.activity$interval)



str(df.activity)
head(df.activity)
tail(df.activity)

df.dailysteps <- aggregate(
                    steps ~ date, 
                    df.activity, 
                    sum
                    )
head(df.dailysteps)

p<-ggplot(df.dailysteps, aes(x = steps)) + 
  geom_histogram(binwidth = 1000,fill="seagreen2") + 
  labs(title="Steps Taken Each Day", 
       x = "Steps Each Day", y = "Frequency") + theme_bw()
print(p)

meanDailySteps   <- mean(df.dailysteps$steps, na.rm=TRUE)
medianDailySteps <- median(df.dailysteps$steps, na.rm=TRUE)

df.intervalsteps <- aggregate(
                      df.activity$steps, 
                      by = list(interval = df.activity$interval),
                      FUN=mean, na.rm=TRUE
                      )
#convert to integers for ggplotting
df.intervalsteps$interval <- 
  as.integer(levels(df.intervalsteps$interval)[df.intervalsteps$interval])
colnames(df.intervalsteps) <- c("interval", "avgsteps")
head(df.intervalsteps)

p<-ggplot(df.intervalsteps, aes(x=interval, y=avgsteps)) +   
  geom_line(size=1 ) +  
  labs(title="All Days Average Number of Steps \nby Interval", x="Interval", y="Interval Step Average\n") +  
  theme_bw()
print(p)

intervalMax <- df.intervalsteps[which.max(df.intervalsteps$avgsteps),]
print(intervalMax)

missing.count<- sum(is.na(df.activity$steps))

df.imputed <- df.activity %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
#str(df.imputed)

#check for missing values
if(sum(is.na(df.imputed$steps))!=0){
  message("Some missing values still exist.")
  Stop
}

df.dailystepsImp <- aggregate(
  steps ~ date, 
  df.imputed, 
  sum
  )
str(df.dailystepsImp)
#colnames(fill_steps_per_day) <- c("date","steps")

##plotting the histogram
p<-ggplot(df.dailystepsImp, aes(x = steps)) + 
  geom_histogram(binwidth = 1000,fill="seagreen2") + 
  labs(title="Steps Taken Each Day\n with Imputed Means for NA's", 
       x = "Steps Each Day", y = "Frequency") + theme_bw()
print(p)

stepsImp.mean   <- mean(df.dailystepsImp$steps, na.rm=TRUE)
stepsImp.median <- median(df.dailystepsImp$steps, na.rm=TRUE)

steps.mean   <- mean(df.dailysteps$steps, na.rm=TRUE)
steps.median <- median(df.dailysteps$steps, na.rm=TRUE)

stepsImp.mean
stepsImp.median

steps.mean
steps.median
summary(df.dailysteps$steps)
summary(df.dailystepsImp$steps)
sd(df.dailysteps$steps, na.rm=T)
sd(df.dailystepsImp$steps, na.rm=T)


#use imputed and calculate steps by interval
head(df.imputed)
#convert interval back to numeric for plotting
df.imputed$interval =as.numeric(df.imputed$interval)
df.imputed$day <- ifelse(as.POSIXlt(as.Date(df.imputed$date))$wday%%6 == 
                                  0, "Weekend", "Weekday")
df.imputed$day <- factor(df.imputed$day, levels = c("Weekday", "Weekend"))
#calc steps by interval and day
df.intervalstepsImp <- aggregate(steps ~ interval + day, df.imputed, mean)
head(df.intervalstepsImp)

#write.csv(df.intervalstepsImp, file = "intervalstepsImp.csv")

p<-ggplot(df.intervalstepsImp, aes(x=interval, y=steps, color= day)) + 
  geom_line(color="seagreen2") + 
  facet_wrap(~ day, nrow=2, ncol=1) +
  labs(x="Interval", y="Average Steps\n") +
  theme_bw()

p<-p+ggtitle("Average Steps by Interval \n by Weekday/Weekend\n for Imputed Data")
print(p)


