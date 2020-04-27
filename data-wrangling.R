library("tidyverse")
# read the data
requests_data_rt <- read_csv("data-raw/requests_data_rt.csv")
# have a look at the data
glimpse(requests_data_rt)

# change col names to lower case
colnames(requests_data_rt) <-
  tolower(make.names(colnames(requests_data_rt)))
colnames(requests_data_rt)
str(requests_data_rt)


#1 inspect request_status
status_na = is.na(requests_data_rt$request_status)
requests_data_rt[requests_data_rt$request_status == status_na, ]
sum(is.na(requests_data_rt$request_status))

# change request_status values to lower case
requests_data_rt$request_status <-
  tolower(requests_data_rt$request_status)

# unique values
unique(requests_data_rt$request_status)

# histogram of request_status
requests_data_rt  %>%
  count(request_status) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(request_status, perc)) +
  geom_bar(position = 'dodge',
           stat = 'identity',
           fill = "red")

# distribution of days_to_complete across request_status
requests_data_rt  %>%
  group_by(request_status) %>%
  select(request_status, days_to_complete) %>%
  summarise(
    min = min(days_to_complete),
    max = max(days_to_complete),
    median = median(days_to_complete),
    sd = sd(days_to_complete)
  )

# ungroup
requests_data_rt %>%
  ungroup()

#2 inspect category
sum(is.na(requests_data_rt$category))

# unique values
unique(requests_data_rt$category)
# trim spaces at the beginning and the end
requests_data_rt$category <- str_trim(requests_data_rt$category)
# convert to titles
requests_data_rt[requests_data_rt$category == "Asset maintenance",]$category <-
  str_to_title("Asset maintenance")

# check
unique(requests_data_rt$category)

#3 inspect service_desc
sum(is.na(requests_data_rt$service_desc))

# unique values
unique(requests_data_rt$service_desc)

# trim spaces at the beginning and the end
requests_data_rt$service_desc <-
  str_trim(requests_data_rt$service_desc)

# convert to titles
requests_data_rt[requests_data_rt$service_desc == "Waste collection services",]$service_desc <-
  str_to_title("Waste collection services")
requests_data_rt[requests_data_rt$service_desc == "Syringe pick-up services",]$service_desc <-
  str_to_title("Syringe pick-up services")

# check
unique(requests_data_rt$service_desc)

#4 inspect suburb
sum(is.na(requests_data_rt$suburb))

# unique values
unique(requests_data_rt$suburb)

# get rid of NA
requests_data_rt <- requests_data_rt %>%
  filter(!is.na(suburb))

# check
unique(requests_data_rt$suburb)

#5 inspect days_to_complete
sum(is.na(requests_data_rt$days_to_complete))

# unique values
unique(requests_data_rt$days_to_complete)

# calculate stitistics
rng <- range(requests_data_rt$days_to_complete)
md <- median(requests_data_rt$days_to_complete)
min <- min(requests_data_rt$days_to_complete)
max <- max(requests_data_rt$days_to_complete)
bin = md / 2
sd(requests_data_rt$days_to_complete)

# plot histogram
requests_data_rt %>%
  ggplot(aes(days_to_complete)) +
  geom_histogram(
    breaks = seq(min, max, by = bin),
    col = "red",
    fill = "green",
    alpha = .6
  )

requests_data_rt %>%
  group_by(category) %>%
  filter(request_status == 'closed') %>%
  select(category, days_to_complete) %>%
  summarise(
    min = min(days_to_complete),
    max =
      max(days_to_complete),
    median =
      median(days_to_complete),
    sd = sd(days_to_complete)
  )

glimpse(requests_data_rt)

# ungroup
requests_data_rt %>%
  ungroup()
# find outliers - this is according 1.5 IQR rule. In normal distribution it rules out 0.35% of top and bottom values
outliers <- boxplot(requests_data_rt$days_to_complete)$out

glimpse(outliers)

# remove outliers
`%notin%` <- Negate(`%in%`)
requests_data_rt_norm <-
  requests_data_rt[which(requests_data_rt$days_to_complete %notin% outliers), ]

glimpse(requests_data_rt_norm)

range(requests_data_rt_norm$days_to_complete)
# check for outliers
outliers <-
  boxplot(requests_data_rt_norm$days_to_complete, outline = FALSE)$out
outliers

# find the range of requests
range(requests_data_rt_norm$request_status)

requests_data_rt_norm  %>%
  group_by(category) %>%
  select(category, days_to_complete) %>%
  summarise(
    min = min(days_to_complete),
    max =
      max(days_to_complete),
    median = median(days_to_complete),
    sd =
      sd(days_to_complete)
  )

glimpse(requests_data_rt_norm)

# ungroup
requests_data_rt_norm %>%
  ungroup()

p_cat <- requests_data_rt_norm %>%
  filter(request_status == 'closed') %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 5) +
  facet_wrap( ~ category)
p_cat

requests_data_rt_norm  %>%
  group_by(service_desc) %>%
  filter(request_status == 'closed') %>%
  select(category, days_to_complete) %>%
  summarise(
    min = min(days_to_complete),
    max =
      max(days_to_complete),
    median = median(days_to_complete),
    sd =
      sd(days_to_complete)
  )

# ungroup
requests_data_rt_norm %>%
  ungroup()

# plot the distribution of number of days to complete
p_serv_descr <- requests_data_rt_norm %>%
  filter(request_status == 'closed') %>%
  ggplot(aes(days_to_complete, color = category, fill = category)) +
  geom_histogram(alpha = 0.6, binwidth = 1) +
  facet_wrap( ~ suburb)
p_serv_descr

p_suburb <- requests_data_rt_norm %>%
  filter(request_status == 'closed') %>%
  ggplot(aes(days_to_complete, color = suburb, fill
             = suburb)) + geom_histogram(alpha = 0.6, binwidth = 5) +
  facet_wrap(
    ~ service_desc,
    nrow = 9,
    ncol = 3,
    scales = 'free_y',
    shrink = TRUE
  )

p_suburb

#6 inspect date_received
sum(is.na(requests_data_rt_norm$date_received))

library(lubridate)
# unique values
unique(requests_data_rt_norm$date_received)

# change str into dates
requests_data_rt_norm$date_received <-
  as.Date(requests_data_rt_norm$date_received, "%m/%d/%Y")

# find out range of date_received
range(requests_data_rt_norm$date_received)

#7  date_completed
sum(is.na(requests_data_rt_norm$date_completed))
sum(is.na(requests_data_rt_norm[is.na(requests_data_rt_norm$date_completed), "days_to_complete"]))

# de-select date_completed
requests_data_rt_norm <- requests_data_rt_norm %>%
  select(-date_completed)

# check the column names
colnames(requests_data_rt_norm)

requests_data_rt_norm %>%
  group_by(request_status) %>%
  summarise(n())

# ungroup
requests_data_rt_norm %>%
  ungroup()

requests_data_rt_norm %>%
  group_by(suburb) %>%
  summarise(n())

# ungroup
requests_data_rt_norm %>%
  ungroup()

# exclude Flemington and South Wharf, as not enough data points
requests_data_rt_norm <- requests_data_rt_norm %>%
  filter(suburb %notin% c("Flemington", "South Wharf"))

temp_r <- requests_data_rt_norm %>%
  filter (request_status != "closed")
view(temp_r)
range(temp_r$date_received)
range(temp_r$days_to_complete)

sum(is.na(temp_r$days_to_complete))
temp_r <- requests_data_rt_norm %>%
  filter (request_status != "closed")

#write clean data set  for requests with resolution time
write_csv(requests_data_rt_norm, "data/requests_data_rt.csv")
