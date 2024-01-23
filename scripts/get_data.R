library(googlesheets4)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)

data <- read_sheet('https://docs.google.com/spreadsheets/d/1uls2NIoxZqB_r64RZSHkW5v9XUBnOglKHSiNElduZzg/edit?usp=sharing', col_types = 'nnccn')

# filter any incomplete years
data <- filter(data, !is.na(OPENED))
# create dates
data$closed_date <- as.Date(paste(data$CLOSED, data$`START YEAR`), format = '%d %b %Y')

# make dates relative to Nov 1st to get ice on/off on linear scale
data$closed_date_stand <- data$closed_date - as.Date(paste('01 Nov', data$`START YEAR`), format = '%d %b %Y')
data$closed_date_stand[data$closed_date_stand < 0] <- data$closed_date_stand[data$closed_date_stand < 0] + 365

data$opened_date <- as.Date(paste(data$OPENED, data$`END YEAR`), format = '%d %b %Y')
data$opened_date_stand <- data$opened_date - as.Date(paste('01 Nov', data$`END YEAR`), format = '%d %b %Y')
data$opened_date_stand[data$opened_date_stand < 0] <- data$opened_date_stand[data$opened_date_stand < 0] + 365

# clean up data for plotting
# including correcting for observations that have NA for days
# which was to indicate multiple freeze/thaw dates for a given year

data_merge <- filter(data, !is.na(DAYS)) %>%
  arrange(as.numeric(DAYS)) %>%
  mutate(order = 1:nrow(.)) %>%
  select('START YEAR', order)

plot_dat <- left_join(data, data_merge) %>%
  select(year = `START YEAR`, days_of_ice = DAYS, ice_on = closed_date_stand, 
         ice_off = opened_date_stand, order) %>%
  mutate(ice_on = as.numeric(ice_on), ice_off = as.numeric(ice_off), days_of_ice = as.numeric(days_of_ice))

# mark years where there were multiple freeze dates
# by summing obs per year
year_count <- group_by(plot_dat, year) %>%
  summarize(year_count = n())

plot_dat <- filter(plot_dat, !(is.na(days_of_ice)))
plot_dat <- mutate(plot_dat, days_cat = cut(days_of_ice, breaks = c(0, 30, 60, 90, 120, Inf), labels = c("< 1 month", "1-2 months", "2-3 months", "3-4 months", "> 4 months"))) %>%
  left_join(year_count)

subtitle_1 <- 'Prior to 1900, 4 months of ice cover on Lake Mendota was relatively common (19 of 45 years on record).
But the last time 4 months of ice was recorded on Lake Mendota was 1977.
In 1982, Lake Mendota had less than 2 months of ice, an event that had never occured before and has now been recorded 5 times. Red lines indicate years with multiple freeze/thaw dates.'

subtitle_1 <- paste0(strwrap(subtitle_1, 150), sep="", collapse="\n")

p <- ggplot(plot_dat, aes(x = year, y = days_cat)) +
  geom_point(size = 5, shape = '|', aes(color = factor(year_count))) +
  scale_color_manual(values = c('darkgray', 'red'), guide = F) +
  #scale_shape_manual(values = c('|', '!'), guide = F) +
  theme_bw() +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        plot.title = element_text(face = 'bold')) +
  labs(x = '', y = 'Ice duration', color = '', title = 'The shrinking ice season',
       subtitle = subtitle_1)

# make long dataset, create categories for years
plot_dat_long <- mutate(plot_dat, 
                        period = case_when(
                          year < 1906 ~ '1855-1905',
                          year >= 1906 & year < 1972 ~ '1906-1971',
                          year >= 1972 ~ '1972-2022')) %>%
  tidyr::pivot_longer(c(ice_on, ice_off), names_to = 'metric', values_to = 'day')

# create groups for period + metric
plot_dat_long <- mutate(plot_dat_long, 
                                metric_group = case_when(
                                  period == '1855-1905' & metric == 'ice_on' ~ '1855-1905 ice on',
                                  period == '1906-1971' & metric == 'ice_on' ~ '1906-1971 ice on',
                                  period == '1972-2022' & metric == 'ice_on' ~ '1972-2022 ice on',
                                  period == '1855-1905' & metric == 'ice_off' ~ '1855-1905 ice off',
                                  period == '1906-1971' & metric == 'ice_off' ~ '1906-1971 ice off',
                                  period == '1972-2022' & metric == 'ice_off' ~ '1972-2022 ice off'
                                ))

# test how much later/earier ice on/ice off was over test periods
ice_on_early <- median(filter(plot_dat_long, metric_group %in% '1855-1905 ice on') %>% pull(day))
ice_on_late <- median(filter(plot_dat_long, metric_group %in% '1972-2022 ice on') %>% pull(day))
ice_off_early <- median(filter(plot_dat_long, metric_group %in% '1855-1905 ice off') %>% pull(day))
ice_off_late <- median(filter(plot_dat_long, metric_group %in% '1972-2022 ice off') %>% pull(day))

subtitle_2 <- 'Both later freeze and earlier thaw dates appear to be contributing to the shorter ice season when comparing median dates of the first and last 50 years of the record. Lake Mendota froze 10.5 days later and thawed 13 days earlier.'

subtitle_2 <- paste0(strwrap(subtitle_2, 150), sep="", collapse="\n")

# overlapping histogram plot
p1 <- ggplot(plot_dat_long, aes(x = day)) +
  geom_density(stat = 'density', aes(fill = metric_group, color = metric_group), alpha = 0.5) +
  scale_fill_manual(values = c('blue', 'blue','gray', 'gray', 'red', 'red'), guide = F) +
  scale_color_manual(values = c('black', 'black', NA, NA, 'black', 'black'), guide = F) +
  geom_segment(aes(x = median(plot_dat_long$day[plot_dat_long$metric_group == '1855-1905 ice on']),
               xend = median(plot_dat_long$day[plot_dat_long$metric_group == '1855-1905 ice on']),
               y = -Inf, yend = 0.04), color = 'blue', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_long$day[plot_dat_long$metric_group == '1972-2022 ice on']),
                   xend = median(plot_dat_long$day[plot_dat_long$metric_group == '1972-2022 ice on']),
                   y = -Inf, yend = 0.04), color = 'red', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_long$day[plot_dat_long$metric_group == '1855-1905 ice off']),
                   xend = median(plot_dat_long$day[plot_dat_long$metric_group == '1855-1905 ice off']),
                   y = -Inf, yend = 0.04), color = 'blue', linetype = 2) +
  geom_segment(aes(x = median(plot_dat_long$day[plot_dat_long$metric_group == '1972-2022 ice off']),
                   xend = median(plot_dat_long$day[plot_dat_long$metric_group == '1972-2022 ice off']),
                   y = -Inf, yend = 0.04), color = 'red', linetype = 2) +
  theme_classic() +
  scale_x_continuous(breaks = c(30, 61, 92, 120, 151, 181), labels = c('Dec 1', 'Jan 1', 'Feb 1', 'March 1', 'April 1', 'May 1')) +
  labs(title = "Later winters and earlier springs",
       subtitle = subtitle_2, y = 'Density', x = 'Figure by Sam Oliver') +
  coord_cartesian(ylim = c(0, 0.05)) +
  geom_text(aes(x = 50, y = 0.048, label = 'Freeze'), size = 4) +
  geom_text(aes(x = 155, y = 0.048, label = 'Thaw'), size = 4) +
  theme(plot.title = element_text(face = 'bold'),
        axis.title.x = element_text(hjust = 1, vjust = -0.5, face = 'italic', size = 10))

# pull simplified legend from another plot
p2 <- ggplot(plot_dat_long, aes(x = day)) +
  geom_density(stat = 'density', aes(fill = period, color = period), alpha = 0.5) +
  scale_fill_manual(values = c('blue', 'gray', 'red')) +
  scale_color_manual(values = c('black', NA, 'black')) +
  geom_vline(aes(linetype = 'median freeze/thaw', xintercept = 50)) +
  scale_linetype_manual(values = 2) +
  labs(linetype = '', fill = '', color = '', y = 'Density') +
  theme_classic() +
  theme(legend.margin = margin(-0.7,0,0,0, unit="cm"))

plegend <- get_legend(p2)

# hacky way to align plots
p1all <- ggdraw() +
  draw_plot(p, 0, 0.5, 1, 0.5) +
  draw_plot(p1, 0.03, 0, 0.95, 0.5) +
  draw_plot(plegend, 0.57, 0.2, 0.005, 0.005)

ggsave('mendota_viz.png', p1all, height = 7.2, width = 12.8)
