library(googlesheets)
library(ggplot2)
library(tidyr)
library(dplyr)

all <- gs_ls()
sheet <- gs_title(x = "History of Ice on Lake Mendota - http://www.aos.wisc.edu/~sco/lakes/Mendota-ice.html")

data <- gs_read(sheet)
head(data)
str(data)

data$closed_date <- as.Date(paste(data$CLOSED, data$`START YEAR`), format = '%d %b %Y')
data$closed_date_stand <- data$closed_date - as.Date(paste('01 Nov', data$`START YEAR`), format = '%d %b %Y')
data$closed_date_stand[data$closed_date_stand < 0] <- data$closed_date_stand[data$closed_date_stand < 0] + 365

data$opened_date <- as.Date(paste(data$OPENED, data$`END YEAR`), format = '%d %b %Y')
data$opened_date_stand <- data$opened_date - as.Date(paste('01 Nov', data$`END YEAR`), format = '%d %b %Y')
data$opened_date_stand[data$opened_date_stand < 0] <- data$opened_date_stand[data$opened_date_stand < 0] + 365

plot_dat <- select(data, year = `START YEAR`, days_of_ice = DAYS, ice_on = closed_date_stand, ice_off = opened_date_stand) %>%
  gather(key = 'metric', value = 'day', ice_on, ice_off) %>%
  mutate(day = as.numeric(day))

ggplot(plot_dat, aes(x = day, y = year)) +
  geom_line(aes(group = year, color = as.numeric(days_of_ice)), size = 1.6) +
  scale_x_continuous(breaks = c(30, 61, 92, 120, 151, 181), labels = c('Dec 1', 'Jan 1', 'Feb 1', 'March 1', 'April 1', 'May 1'))+
  theme_dark() +
  scale_color_gradient2(high = 'blue', mid = 'white', low = 'red', midpoint = median(as.numeric(plot_dat$days_of_ice), na.rm = T))+
  theme(panel.grid = element_blank())

data_merge <- filter(data, !(DAYS %in% '-')) %>%
  arrange(as.numeric(DAYS)) %>%
  mutate(order = 1:nrow(.)) %>%
  select('START YEAR', order)
  
data_temp <- data
#data_temp$DAYS[which(data_temp$DAYS %in% '-')] <- data_temp$DAYS[which(data_temp$DAYS %in% '-') + 1]
data_temp$DAYS <- as.numeric(data_temp$DAYS)

plot_dat_ordered <- left_join(data, data_merge) %>%
  select(year = `START YEAR`, days_of_ice = DAYS, ice_on = closed_date_stand, 
         ice_off = opened_date_stand, order) %>%
  mutate(ice_on = as.numeric(ice_on), ice_off = as.numeric(ice_off), days_of_ice = as.numeric(days_of_ice))
  #gather(key = 'metric', value = 'day', ice_on, ice_off) %>%
  #mutate(day = as.numeric(day))

ggplot(plot_dat_ordered) +
  geom_segment(aes(y = order, yend = order, x = ice_on, xend = ice_off, color = year), size = 1.6) +
  scale_color_gradient2(low = 'blue', mid = 'gray', high = 'red', midpoint=1938)

# years where there was ice breakup
year_count <- group_by(plot_dat_ordered, year) %>%
  summarize(year_count = n())
# When in history?
plot_dat_history <- filter(plot_dat_ordered, !(is.na(days_of_ice)))
plot_dat_history <- mutate(plot_dat_history, days_cat = cut(days_of_ice, breaks = c(0, 30, 60, 90, 120, Inf), labels = c("< 1 month", "1-2 months", "2-3 months", "3-4 months", "> 4 months"))) %>%
  left_join(year_count)

ggplot(plot_dat_history, aes(x = year, y = days_cat)) +
  geom_point(size = 5, shape = '|', aes(color = factor(year_count))) +
  scale_color_manual(values = c('darkgray', 'red'), guide = F) +
  #scale_shape_manual(values = c('|', '/'), guide = F) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.ticks = element_blank()) +
  labs(x = '', y = '', color = '', title = 'The disappearing ice season',
       subtitle = 'Prior to 1900, 4 months of ice on Lake Mendota was relatively common (19 of 45 years on record).
But the last time 4 months of ice was recorded on Lake Mendota was 1977, and now something new is happening.
In 1982, Lake Mendota had less than 2 months of ice, an event that had never occured before and has now been recorded 5 times.')

early <- filter(plot_dat_history, year < 1936)
late <- filter(plot_dat_history, year >=1936)

plot_dat_history_long <- mutate(plot_dat_history, period = ifelse(year < 1936, 'first half', 'second half')) %>%
  gather(key = 'metric', value = 'day', ice_on, ice_off)

ggplot(plot_dat_history_long) +
  geom_histogram(aes(x = ))
