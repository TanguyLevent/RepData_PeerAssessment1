activity <- read.csv("activity.csv", na.strings = "NA")
total_NA <- sum(is.na(activity))
activity <- na.omit(activity)

total_steps <- activity %>% group_by(date) %>% summarise(steps = sum(steps))
hist(total_steps$steps,  col = c("#3399FF"),xlab = "Total steps", main = "Total steps by day")
#barplot(total_steps$steps, names = total_steps$date ,ylab = "Steps", xlab = "Days", main = "Total steps by day", col = c("#3399FF"))

mean <-  total_steps %>%  summarise(steps = round(mean(steps),2))
median <-  total_steps %>% summarise(steps = median(steps))

mean_5min <-  activity %>% group_by(interval) %>% summarise(steps = round(mean(steps),2))
plot(mean_5min$interval, mean_5min$steps, type="l",xlab = "5-minute interval", ylab = "Number of steps",main = "Average steps of 5 min interval",col = c("#FF6666"))

max_interval <- mean_5min[mean_5min$steps ==  max(mean_5min$steps),]

#change format date

activity$date <- as.POSIXlt(activity$date, format = "%Y-%m-%d")
activity$Day_Category <- weekdays(activity$date)
activity$Day_Category <- ifelse(activity$Day_Category %in% c("Samedi", "Dimanche"), "weekend", "weekday")

activity2 <- select(activity , -(date))
wkday <- activity2 %>% group_by(Day_Category,interval) %>% summarise(steps = round(mean(steps),2))

ggplot(wkday, aes(factor(interval), steps, fill=Day_Category)) + 
        geom_bar(stat="identity") + 
        facet_grid(Day_Category~.) + 
        scale_fill_brewer(palette="Pastel1")

