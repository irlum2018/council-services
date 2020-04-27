library("tidyverse")
library("tibbletime")

#functions
data_for_time <- function(data, start, stop, date_format) {
  result <- data %>%
    filter((as.Date(start, date_format) < date_received) &
             (date_received < as.Date(stop, date_format))) %>%
    arrange(date_received)
  return(result)
}

requests_category_service_desc_suburb <- function(year) {
  data <- requests_data_2015
  if (year == '2016')
  {
    data <- requests_data_2016
  }
  result <- data %>%
    count(category, service_desc, suburb)
  return(result)
}

median_requests_time_category_service_desc_suburb <-
  function(year) {
    data <- requests_data_2015
    if (year == '2016')
    {
      data <- requests_data_2016
    }
    result <- data %>%
      group_by(suburb, service_desc, category) %>%
      summarise(mdn = median(days_to_complete))
    return(result)
  }

requests_data <- read_csv("data/requests_data_rt.csv")
colnames(requests_data)
unique(requests_data$request_status)
str(requests_data)

glimpse(requests_data)

range(requests_data$date_received)

requests_data_2014 <-
  data_for_time(requests_data, "01/01/2012", "01/01/2015", "%m/%d/%Y")
range(requests_data_2014$date_received)
unique(requests_data_2014$request_status)

#data for the same period as 2016
requests_data_2015 <-
  data_for_time(requests_data, "12/31/2014", "10/16/2015", "%m/%d/%Y")
range(requests_data_2015$date_received)
unique(requests_data_2015$request_status)

requests_data_2016 <-
  data_for_time(requests_data, "12/31/2015", "10/16/2016", "%m/%d/%Y")
range(requests_data_2016$date_received)
unique(requests_data_2016$request_status)
unique(requests_data_2016$service_desc)

#Q1.	What is the demand for public services of Melbourne City council?

#percentage of public service requests grouped by category
percent_request_by_category <-
  requests_data %>%   count(category) %>%
  mutate(percentage = (n * 100) / sum(n))

#plot the percentage of public service requests grouped by category
percent_request_by_category %>%
  ggplot(aes(category, percentage, color = "pink")) +
  geom_bar(position = 'dodge', stat = 'identity')

#percentage of public service requests grouped by category in 2015
percent_request_by_category_2015 <- requests_data_2015  %>%
  count(category) %>%
  mutate(percentage = (n * 100) / sum(n))

percent_request_by_category_2015 <-
  percent_request_by_category_2015 %>%
  mutate(perc_2015 = percentage, n_2015 = n) %>%
  select(-n, -percentage)

#percentage of public service requests grouped by category in 2016
percent_request_by_category_2016 <- requests_data_2016  %>%
  count(category) %>%
  mutate(percentage = (n * 100) / sum(n))

percent_request_by_category_2016 <-
  percent_request_by_category_2016 %>%
  mutate(perc_2016 = percentage, n_2016 = n) %>%
  select(-n, -percentage)

percent_request_by_category_2015_16 <-
  percent_request_by_category_2015 %>%
  inner_join(percent_request_by_category_2016)


#plot the total number of public service requests grouped by category
percent_request_by_category_2015_16 %>%
  ggplot(aes(x = category, y = value, color = variable)) +
  geom_point(aes(y = n_2015, col = "2015", stroke = 5)) +
  geom_point(aes(y = n_2016, col = "2016", stroke = 4)) +
  labs(
    title = "Comparison of Number of Requests per Category from 1 Jan to 15 Oct for 2015 and 2016",
    y = "Number of Requests",
    x = "Category of Service",
    color = "Numbers for",
    fontsize.labels = c(10, 0)
  )

#plot the % number of public service requests grouped by category
percent_request_by_category_2015_16 %>%
  ggplot(aes(x = category, y = value, color = variable)) +
  geom_point(aes(y = perc_2015, col = "2015", stroke = 5)) +
  geom_point(aes(y = perc_2016, col = "2016", stroke = 4)) +
  labs(
    title = "Comparison of Proportion of Requests  per Category from 1 Jan to 15 Oct for 2015 and 2016",
    y = "Number of Requests (%)",
    x = "Category of Service",
    color = "Numbers for",
    fontsize.labels = c(10, 0)
  )

#get the proportion of number of requests per service description for 2015
requests_category_service_desc_proportion_2015 <-
  requests_data_2015  %>%
  count(category, service_desc) %>%
  arrange(desc(n)) %>%
  mutate(percentage = (n * 100) / sum(n))

requests_category_service_desc_proportion_2015
#plot the graph
requests_category_service_desc_proportion_2015 %>%
  ggplot(aes(x = service_desc, y = percentage, fill = category)) +
  geom_col() +
  coord_flip() +
  ylab("Proportion in %") +
  xlab("Service Description") +
  scale_fill_brewer(palette = "Spectral")

#get the proportion of number of requests per service description for 2016
requests_category_service_desc_proportion_2016 <-
  requests_data_2016 %>%
  count(category, service_desc) %>%
  arrange(desc(n)) %>%
  mutate(percentage = (n * 100) / sum(n))
requests_category_service_desc_proportion_2016

#plot the graph
requests_category_service_desc_proportion_2016 %>%
  ggplot(aes(x = service_desc, y = percentage, fill = category)) +
  geom_col() +
  coord_flip() +
  ylab("Proportion in %") +
  xlab("Service Description") +
  scale_fill_brewer(palette = "Spectral")

#plot the tree map
#install.packages("treemap")
library(treemap)
requests_category_service_desc_proportion_2015 %>%
  treemap(
    index = c("category", "service_desc"),
    vSize = "n",
    vColor = "category",
    type = "categorical",
    title = "Number of Requests by Categories and Description from 1 Jan to 15 Oct 2015",
    fontsize.labels = c(0, 10),
    fontface.labels = c(1, 2),
    position.legend = "bottom",
    legend.justification = c(0, 0)
  )

requests_category_service_desc_proportion_2016 %>%
  treemap(
    index = c("category", "service_desc"),
    vSize = "n",
    vColor = "category",
    type = "categorical",
    title = "Number of Requests by Categories and Description from 1 Jan to 15 Oct 2016",
    fontsize.labels = c(0, 10),
    fontface.labels = c(1, 2),
    position.legend = "bottom",
    legend.justification = c(0, 0)
  )

#Q2. What are the complaints patterns and trends for  different suburbs?
#number of requests by date_received by suburb by category

#plot the percentage of public service requests grouped by category
#stacked by suburb for 2015-2016
requests_data  %>%
  ggplot(aes(x = category, fill = suburb)) +
  geom_bar(aes(y = (..count..) / sum(..count..)))

#plot the percentage of public service requests grouped by category
#stacked by suburb for 2016
requests_data_2016  %>%
  ggplot(aes(x = category, fill = suburb)) +
  geom_bar(aes(y = (..count..) / sum(..count..)))

#plot the percentage of public service requests grouped by category for each suburb 2015-2016

requests_data  %>%
  ggplot(aes(category, fill = suburb)) +
  geom_histogram(stat = "count") +
  scale_x_discrete(
    name = 'category',
    labels = c(
      'Asset\n maintenance',
      'Graffiti',
      'Parking ',
      'Parks\n  & Trees',
      'Roads\n& Traffic',
      'Waste,\n Street Cleaning\n& Litter'
    )
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 7,
    ncol = 2,
    scales = 'free_y',
    shrink = TRUE
  )

#plot the percentage of public service requests grouped by category for each suburb 2016

requests_data_2016  %>%
  ggplot(aes(category, fill = suburb)) +
  geom_histogram(stat = "density") +
  scale_x_discrete(
    name = 'category',
    labels = c(
      'Asset\n maintenance',
      'Graffiti',
      'Parking ',
      'Parks\n  & Trees',
      'Roads\n& Traffic',
      'Waste,\n Street Cleaning\n& Litter'
    )
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 7,
    ncol = 2,
    scales = 'free_y',
    shrink = TRUE
  )
#visualise data 2016 as bubble plot, to reveal clusters
#suburb - x, category - y, numer of requests - size of the bubble
library(ggplot2)
library(viridis)

suburbs <-
  c(
    'Carlton',
    'Carlton\nNorth',
    'Docklands',
    'East\nMelbourne',
    '\nKensington',
    'Melbourne',
    'North\nMelbourne',
    'Parkville',
    'Port\nMelbourne',
    'South\nYarra',
    'Southbank',
    'West\nMelbourne'
  )
unique(requests_data$category)
requests_data_2016  %>%
  count(category, service_desc, suburb) %>%
  arrange(desc(n)) %>%
  ggplot(aes(
    x = suburb,
    y = service_desc,
    size = n,
    fill = category
  ),
  show.legend = TRUE) +
  geom_point(alpha = 0.5, shape = 21) +
  scale_size(range = c(.1, 24), name = "Number of Requests") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme(legend.text = element_text(
    colour = "black",
    size = 8,
    face = "bold"
  )) +
  #scale_fill_manual(values = cbp1)+
  scale_x_discrete(name = 'Suburbs',
                   labels = suburbs) +
  labs(title =
         "Number of Requests per Service Description for Each Suburb from 1 Jan to 15 Oct 2016") +
  theme(legend.justification = c(0, 0),
        legend.position = "bottom") +
  ylab("Service Description") +
  xlab("Suburb")

#top suburbs with service description “Missed Bin Collection”
requests_data_2016  %>%
  count(category, service_desc, suburb) %>%
  arrange(desc(n)) %>%
  filter(service_desc == "Missed Bin Collection") %>%
  mutate(percentage = (n * 100) / sum(n))

#top suburbs with service description "Graffiti Removal"
requests_data_2016  %>%
  count(category, service_desc, suburb) %>%
  arrange(desc(n)) %>%
  filter(category == "Graffiti") %>%
  mutate(percentage = (n * 100) / sum(n))

#top suburbs with service description "Parks and Trees"
requests_data_2016  %>%
  count(category, service_desc, suburb) %>%
  arrange(desc(n)) %>%
  filter(category == "Parks and Trees") %>%
  mutate(percentage = (n * 100) / sum(n))

#public service request per category, suburb
requests_data_category_suburb_2016 <- requests_data_2016  %>%
  count(category, suburb) %>%
  arrange(desc(n)) %>%
  mutate(percentage = (n * 100) / sum(n))

#requests for Waste, Street Cleaning and Litter
requests_data_category_suburb_2016  %>%
  filter(category == "Waste, Street Cleaning and Litter")

#requests for Graffiti
requests_data_category_suburb_2016  %>%
  filter(category == "Graffiti")

#requests for Parks and Trees
requests_data_2016  %>%
  filter(category == "Parks and Trees")

#Distribution of requests by category of services monthly for suburbs
#create column date_received_month
request_temp <- requests_data %>%
  group_by(suburb, category, date_received) %>%
  select(date_received, suburb, category) %>%
  filter(((
    date_received > as.Date("12/31/2014", "%m/%d/%Y")
  ) &&
    (
      date_received < as.Date("10/16/2015", "%m/%d/%Y")
    )) ||
    ((
      date_received > as.Date("12/31/2015", "%m/%d/%Y")
    ) &&
      (
        date_received < as.Date("10/16/2016", "%m/%d/%Y")
      ))) %>%
  summarise(n = n())

#ungroup
request_temp %>%
  ungroup()
#organise data by years 2015, 2016
range(request_temp$date_received)

request_temp$date_received_year <-
  as.Date(cut(request_temp$date_received, breaks = "year"))

request_temp$date_received_month <-
  as.Date(cut(request_temp$date_received, breaks = "month"))

request_temp  %>%
  ggplot(aes(date_received_month, n, color = category, fill = category)) +
  stat_summary(fun = sum, #adds up all observations for the month
               geom = "bar") +
  facet_wrap(
    ~ suburb,
    nrow = 7,
    ncol = 2,
    scales = 'free_y',
    shrink = TRUE
  )

#detailed trend for Carlton North
request_temp  %>%
  filter(suburb == "Carlton North")  %>%
  ggplot(aes(date_received_month, n, color = category, fill = category)) +
  stat_summary(fun = sum, #adds up all observations for the month
               geom = "bar")

#detailed trend for Port Melbourne
request_temp  %>%
  filter(suburb == "Port Melbourne")  %>%
  ggplot(aes(date_received_month, n, color = category, fill = category)) +
  stat_summary(fun = sum, #adds up all observations for the month
               geom = "bar")

#detailed trend for Melbourne
request_temp  %>%
  filter(suburb == "Melbourne")  %>%
  ggplot(aes(date_received_month, n, color = category, fill = category)) +
  stat_summary(fun = sum, #adds up all observations for the month
               geom = "bar")

#detailed trend for Carlton
request_temp  %>%
  filter(suburb == "Carlton")  %>%
  ggplot(aes(date_received_month, n, color = category, fill = category)) +
  stat_summary(fun = sum, #adds up all observations for the month
               geom = "bar")


#Q3.How Melbourne City Council deals with public service requests?
#1. How does the time to service requests (date_completed) vary by category and by locality? This could be useful in determining which request types may require additional resources.
head(requests_data) %>%
  select(date_received, days_to_complete, suburb, category)

#median request time for 2015
median_requests_time_suburb_category_2015 <- requests_data_2015 %>%
  group_by(suburb, service_desc, category) %>%
  summarise(mdn = median(days_to_complete))

#ungroup
requests_data_2015 %>%
  ungroup()

#median request time for 2016
median_requests_time_suburb_category_2016 <- requests_data_2016 %>%
  group_by(suburb, service_desc, category) %>%
  summarise(mdn = median(days_to_complete))

#ungroup
requests_data_2016 %>%
  ungroup()


#bubble chart to visualise
#visualise the plot as bubble plot, to reveal clusters
#suburb - x, category - y, numer of requests - size of the bubble
#library(ggplot2)


#plot median value for request completion time for 2015
suburbs <-
  c(
    'Carlton',
    'Carlton\nNorth',
    'Docklands',
    'East\nMelbourne',
    '\nKensington',
    'Melbourne',
    'North\nMelbourne',
    'Parkville',
    'Port\nMelbourne',
    'South\nYarra',
    'Southbank',
    'West\nMelbourne'
  )

median_requests_time_suburb_category_2015 %>%
  ggplot(aes(
    x = suburb,
    y = service_desc,
    size = mdn,
    fill = category
  ),
  show.legend = TRUE) +
  geom_point(alpha = 0.5, shape = 21) +
  scale_size(range = c(.1, 24), name = "Completion Days") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme(legend.text = element_text(
    colour = "black",
    size = 8,
    face = "bold"
  )) +
  scale_x_discrete(name = 'Suburbs',
                   labels = suburbs) +
  labs(title =
         "Median Request Completion Time (in days) for Requests from 1 Jan to 15 Oct 2015") +
  theme(legend.justification = c(0, 0),
        legend.position = "bottom") +
  ylab("Service Description") +
  xlab("Suburb")

#plot median value for request completion time for 2016

median_requests_time_suburb_category_2016 %>%
  ggplot(aes(
    x = suburb,
    y = service_desc,
    size = mdn,
    fill = category
  ),
  show.legend = TRUE) +
  geom_point(alpha = 0.5, shape = 21) +
  scale_size(range = c(.1, 24), name = "Completion Days") +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme(legend.text = element_text(
    colour = "black",
    size = 8,
    face = "bold"
  )) +
  scale_x_discrete(name = 'Suburbs',
                   labels = suburbs) +
  labs(title =
         "Median Request Completion Time (in days) for Requests from 1 Jan to 15 Oct 2016") +
  theme(legend.justification = c(0, 0),
        legend.position = "bottom") +
  ylab("Service Description") +
  xlab("Suburb")

requests_data_2015 %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 1),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete per Suburb from 1 Jan to 15 Oct 2015")

#plot the distribution of number of days to complete per suburb for for "Waste, Street Cleaning and Litter” category in 2015
requests_data_2015 %>%
  filter(category == "Waste, Street Cleaning and Litter") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 1),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Waste, Street Cleaning and Litter Requests per Suburb from 1 Jan to 15 Oct 2015")

#plot the distribution of number of days to complete per suburb for for "Waste, Street Cleaning and Litter” category in 2016
requests_data_2016 %>%
  filter(category == "Waste, Street Cleaning and Litter") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 1),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Waste, Street Cleaning and Litter Requests per Suburb from 1 Jan to 15 Oct 2016")

#plot the distribution of number of days to complete per suburb for for "Graffiti” category in 2015
requests_data_2015 %>%
  filter(category == "Graffiti") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 8),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Graffiti Requests per Suburb from 1 Jan to 15 Oct 2015")

#plot the distribution of number of days to complete per suburb for for "Graffiti” category in 2016
requests_data_2016 %>%
  filter(category == "Graffiti") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 5),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Graffiti Requests per Suburb from 1 Jan to 15 Oct 2016")

#plot the distribution of number of days to complete per suburb for for "Parks and Trees” category in 2015
requests_data_2015 %>%
  filter(category == "Parks and Trees") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 5),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Parks and Trees Requests per Suburb from 1 Jan to 15 Oct 2015")


#plot the distribution of number of days to complete per suburb for for "Parks and Trees” category in 2016
requests_data_2016 %>%
  filter(category == "Parks and Trees") %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  geom_vline(aes(xintercept = median(days_to_complete)), col = 'black', size =
               1) +
  geom_text(
    aes(label = "median", y = 3, x = 5),
    vjust = -1,
    col = 'black',
    size = 3
  ) +
  facet_wrap(
    ~ suburb,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  ) +
  labs(title = "Frequencies of Numbers of Days to Complete Parks and Trees Requests per Suburb from 1 Jan to 15 Oct 2016")

#average completion day for "Waste, Street Cleaning and Litter”
median_requests_time_suburb_category_2016 %>%
  filter(category == "Waste, Street Cleaning and Litter")

#average completion day for “Graffiti”
median_requests_time_suburb_category_2016 %>%
  filter(category == "Graffiti")

#average completion day for “Parks and Trees”
median_requests_time_suburb_category_2016 %>%
  filter(category == "Parks and Trees")

#2. How does the time to service requests (date_completed) vary with the number of requests for different services and different localities?
#plot response time versus number of requests
merge_data <-
  median_requests_time_category_service_desc_suburb("2016") %>%
  inner_join(requests_category_service_desc_suburb("2016"))

pairs(merge_data[, 4:5], pch = 19)

#plot it:
library("plotly")
good.shapes = c(1:25, 33:127)
pl <- merge_data %>%
  ggplot(aes(
    n,
    mdn,
    shape = suburb,
    color = category,
    text = paste(
      suburb,
      "\n",
      service_desc,
      "\nMedian Time:",
      mdn,
      " days",
      "\nNumber of Requests:",
      n
    )
  )) +
  scale_shape_manual(values = good.shapes[1:13]) +
  geom_point(size = 5) +
  scale_x_continuous(trans = 'log10') +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  theme(
    panel.background = element_rect (fill = 'grey'),
    panel.grid.major = element_line (colour = 'grey90', size = 0.20),
    panel.grid.minor = element_line (colour = 'grey90', size = 0.10),
    axis.title = element_text (size = 12, face = "bold"),
    text = element_text (size = 14, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(colour = "black", size =
                                 12),
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text (size = 12, angle = 90),
    legend.justification = c(1, 0)
  ) +
  labs(
    title = "Response Time versus Number of Requests from 1 Jan to 15 Oct 2016",
    y = "Median Response Time (in days)",
    x = "Number of Requests",
    color = "Numbers for",
    shape = "Service Description:",
    font.main = 20,
    width = "auto",
    height = "auto",
    res = 72,
    scrollable = TRUE
  )

ggplotly(pl, tooltip = "text")
