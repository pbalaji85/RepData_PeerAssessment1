## url to download the activity data
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'

## downloading the file
download.file(url, destfile = 'activity_data.zip', method='curl')

## check to see if the file exists, if not, unzip it
if(!file.exists('activity.csv')) {
        unzip('activity_data.zip')
}

## Read-in the file. I'm also specifying the class of each column (to deal with the dates)
ActivityFile <- read_csv('activity.csv',col_types = list(col_integer(),
                        col_date(format = '%Y-%m-%d'),col_integer()))

## Calculate the total number of steps taken every day
q1 <- ActivityFile %>% group_by(date) %>% 
        summarise(sm = sum(steps, na.rm=T))

## Plot a histogram of the total number of steps taken per day
ggplot(data = q1, aes(sm)) +
        geom_histogram(fill='orange', binwidth = 250, stat='bin', na.rm = T) +
        labs(x = 'Total Steps per Day', y = 'Count') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

## Calculate the medan and median number of steps taken each day.
q2 <- ActivityFile %>% group_by(date) %>% 
        summarise(mn = mean(steps, na.rm=T), md = median(steps, na.rm = T))
## Let's take a quick look at the mean number of steps taken each day
## Since the numbers are quite large, I will report the mean and median
## number of steps taken each day using plots

## Plotting the mean number of steps taken each day
p1 <- ggplot(data = q2, aes(x = date, y= mn)) +
        geom_line( color = 'green', na.rm = T) +
        labs(x = 'Days', y = 'Steps per day (Mean)') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

## Plotting the mean number of steps taken each day
p2 <- ggplot(data = q2, aes(x = date, y= md)) +
        geom_line( color = 'blue', na.rm = T) +
        labs(x = 'Days', y = 'Steps per day (Median)') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))
require(gridExtra)
grid.arrange(p1, p2, ncol=2)

q3 <- ActivityFile %>% group_by(interval) %>% 
        summarize(mn = mean(steps,na.rm = T)) %>%
        mutate(new.int = interval/100)

margin(t=1,r=0.5,b=0.5,r=1)        
p3 <- ggplot(data = q3, aes(x = new.int, y= mn)) +
        geom_line( color = 'brown', na.rm = T) +
        labs(x = 'Hours in the day', y = 'Mean number of steps') +
        scale_x_continuous(limits = c(0,24), breaks = seq(0,24,3)) +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

## Total number of missing values in the dataset
q4_a <- sum(is.na(ActivityFile)) 

cat('The number of missing values in this dataset is:',q4_a)

## Let's write a simple function that will impute missing values 
## by rounding the vales of the mean number of steps for that interval
impute.miss.val <- function(df) {
        ## How many unique intervals in the dataset
        uni_inter <- unique(df$interval)
        ## Let's iterate through each interval
        for (i in 1:length(uni_inter)) {
                # Get values for the current interval
                cur.inter <- df$steps[df$interval==uni_inter[i]]
                # Replace NAs with the mean for that interval
                cur.inter[is.na(cur.inter)] <- round(mean(cur.inter, na.rm = T))
                ## Assign the filled in values back to the DF
                df$steps[df$interval==uni_inter[i]] <- cur.inter
        }
return(df)
}
## Let's fill in the missing values using the impute.miss.val function
New.ActivityFile <- impute.miss.val(ActivityFile)

## Calculate the total number of steps taken every day
q4_b <- New.ActivityFile %>% group_by(date) %>% 
        summarise(total = sum(steps, na.rm=T))

## Histogram of the total number of steps taken each day
ggplot(data = q4_b, aes(total)) +
        geom_histogram(fill='dark red', binwidth = 250, stat='bin', na.rm = T) +
        labs(x = 'Total Steps per Day', y = 'Count') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

## By imputing the missing values, we can see that the counts of number of days
## with 0 steps decreases and the number of days with 10,000 steps increases
q4_c <- New.ActivityFile %>% group_by(date) %>% 
        summarise(mn = mean(steps, na.rm=T), md = median(steps, na.rm = T))

p4a <- ggplot(data = q4_c, aes(x = date, y= mn)) +
        geom_line( color = 'green', na.rm = T) +
        labs(x = 'Days', y = 'Steps per day (Mean)') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

## Plotting the mean number of steps taken each day
p4b <- ggplot(data = q4_c, aes(x = date, y= md)) +
        geom_line( color = 'blue', na.rm = T) +
        labs(x = 'Days', y = 'Steps per day (Median)') +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))
require(gridExtra)
grid.arrange(p4a, p4b, ncol=2)

## Number of steps Weekdays versus Weekends
wkday <- c('Monday', 'Tuesday','Wednesday','Thursday','Friday')

q5 <- New.ActivityFile %>% 
        mutate(wekday = ifelse(weekdays(date) %in% wkday, 'Weekday', 'Weekend')) %>%
        group_by(wekday,interval) %>% summarize(mn = mean(steps)) %>%
        mutate(new.int = (interval/100))

ggplot(data = q5, aes(x = new.int, y= mn)) +
        geom_line( color = 'blue', na.rm = T) +
        facet_grid(wekday ~.) +
        labs(x = 'Hours in the day', y = 'Steps per day (Mean)') +
        scale_x_continuous(limits=c(0,24), breaks = seq(0,24,3)) +
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_rect(color = "black", size = 1, fill = NA),
              panel.background = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size = 16, family="Arial"),
              axis.text.x=element_text(colour="black", size = 14),
              axis.text.y=element_text(colour="black", size = 14))

