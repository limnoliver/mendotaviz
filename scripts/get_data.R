library(googlesheets)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

all <- gs_ls()
sheet <- gs_title(x = "History of Ice on Lake Mendota - http://www.aos.wisc.edu/~sco/lakes/Mendota-ice.html")

data <- gs_read(sheet)
head(data)
str(data)

# create dates
data$closed_date <- as.Date(paste(data$CLOSED, data$`START YEAR`), format = '%d %b %Y')

# make dates relative to Nov 1st to get ice on/off on linear scale
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

subtitle_1 <- 'Prior to 1900, 4 months of ice on Lake Mendota was relatively common (19 of 45 years on record).
But the last time 4 months of ice was recorded on Lake Mendota was 1977, and now something new is happening.
In 1982, Lake Mendota had less than 2 months of ice, an event that had never occured before and has now been recorded 5 times. 
Red lines indicate years with multiple freeze/thaw dates'

subtitle_1 <- paste0(strwrap(subtitle_1, 80), sep="", collapse="\n")

p <- ggplot(plot_dat_history, aes(x = year, y = days_cat)) +
  geom_point(size = 5, shape = '|', aes(color = factor(year_count))) +
  scale_color_manual(values = c('darkgray', 'red'), guide = F) +
  #scale_shape_manual(values = c('|', '!'), guide = F) +
  theme_classic() +
  theme(panel.border = element_blank(), axis.ticks = element_blank()) +
  labs(x = '', y = '', color = '', title = 'The disappearing ice season',
       subtitle = subtitle_1)


plot_dat_history_long <- mutate(plot_dat_history, period = case_when(year < 1905 ~ '1855-1904',
                                                                     year >= 1905 & year < 1968 ~ '1905-1967',
                                                                     year >= 1968 ~ '1968-2017')) %>%
  gather(key = 'metric', value = 'day', ice_on, ice_off)

plot_dat_history_long <- mutate(plot_dat_history_long, 
                                metric_group = case_when(
                                  period == '1855-1904' & metric == 'ice_on' ~ '1855-1904 ice on',
                                  period == '1905-1967' & metric == 'ice_on' ~ '1905-1967 ice on',
                                  period == '1968-2017' & metric == 'ice_on' ~ '1968-2017 ice on',
                                  period == '1855-1904' & metric == 'ice_off' ~ '1855-1904 ice off',
                                  period == '1905-1967' & metric == 'ice_off' ~ '1905-1967 ice off',
                                  period == '1968-2017' & metric == 'ice_off' ~ '1968-2017 ice off'
                                ))

subtitle_2 <- 'Both, it seems, when we compare median freeze and thaw dates of the last to first 50 years
of the record. Lake Mendota froze 10 days later and thawed 11.5 days earlier.'

subtitle_2 <- paste0(strwrap(subtitle_2, 80), sep="", collapse="\n")

p1 <- ggplot(plot_dat_history_long, aes(x = day)) +
  geom_density(stat = 'density', aes(fill = metric_group, color = metric_group), alpha = 0.5) +
  scale_fill_manual(values = c('blue', 'blue','gray', 'gray', 'red', 'red'), guide = F) +
  scale_color_manual(values = c('black', 'black', NA, NA, 'black', 'black'), guide = F) +
  geom_segment(aes(x = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1855-1904 ice on']),
               xend = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1855-1904 ice on']),
               y = -Inf, yend = 0.04), color = 'blue', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1968-2017 ice on']),
                   xend = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1968-2017 ice on']),
                   y = -Inf, yend = 0.04), color = 'red', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1855-1904 ice off']),
                   xend = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1855-1904 ice off']),
                   y = -Inf, yend = 0.04), color = 'blue', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1968-2017 ice off']),
                   xend = median(plot_dat_history_long$day[plot_dat_history_long$metric_group == '1968-2017 ice off']),
                   y = -Inf, yend = 0.04), color = 'red', linetype = 2) +
  theme_classic() +
  scale_x_continuous(breaks = c(30, 61, 92, 120, 151, 181), labels = c('Dec 1', 'Jan 1', 'Feb 1', 'March 1', 'April 1', 'May 1')) +
  labs(title = "Later winters or earlier springs?",
       subtitle = subtitle_2, x = '') +
  coord_cartesian(ylim = c(0, 0.05)) +
  geom_text(aes(x = 50, y = 0.048, label = 'Ice On'), size = 5) +
  geom_text(aes(x = 155, y = 0.048, label = 'Ice Off'), size = 5) 

p2 <- ggplot(plot_dat_history_long, aes(x = day)) +
  geom_density(stat = 'density', aes(fill = period, color = period), alpha = 0.5) +
  scale_fill_manual(values = c('blue', 'gray', 'red')) +
  scale_color_manual(values = c('black', NA, 'black')) +
  geom_vline(aes(linetype = 'median ice on/off', xintercept = 50)) +
  scale_linetype_manual(values = 2) +
  labs(linetype = '', fill = '', color = '') +
  theme_classic() +
  theme(legend.margin = margin(-0.7,0,0,0, unit="cm"))

library(cowplot)

plegend <- get_legend(p2)

p1all <- ggdraw() +
  draw_plot(p1, 0, 0, 1, 1) +
  draw_plot(plegend, 0.47, 0.4, 0.1, 0.1)
  
p1all
  
