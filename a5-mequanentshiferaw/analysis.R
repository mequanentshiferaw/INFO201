# "date"   "state"      "city"    "address"  "num_killed"  "num_injured" "lat" "long"


# install.packages("dkahle")
# install.packages("ggmap")
# install.packages("ggplot2")
# install.packages("leaflet")
library(ggmap)
library(ggplot2)
library(dplyr)
library(gganimate)
library(maps)
# loading data
shootings_2018 <- read.csv("data/shootings-2018.csv")


# Summary Information

num_shooting <- nrow(shootings_2018) # 340 (occurred of shootings )
num_feature <- ncol(shootings_2018) # 8
# the number of lives lost was 373 (total_killed)
#  total_killed max_killed min_killed mean_killed
# 1          373         17          0        1.10
summary_killed <- summarise(shootings_2018,
  total_killed = sum(num_killed),
  total_shooting_occured = sum(as.numeric(date)),
  max_killed = max(num_killed), min_killed = min(num_killed),
  total_injured = sum(num_injured)
)

# This code shows the cities that were most impacted

summary_most_impacted <- arrange(shootings_2018, -num_killed, -num_injured) %>%
  select(date, state, num_killed, num_injured) %>%
  head()
# shows the number of shooting that happen by states
num_shooting_by_state <- shootings_2018 %>%
  group_by(state) %>%
  count()


# Summary Table

# shows the top 6 of total killed and injured by states
sum_by_state <- shootings_2018 %>%
  group_by(state) %>%
  summarise(total_killed = sum(num_killed), total_injured = sum(num_injured)) %>%
  arrange(-total_killed, -total_injured)
head(sum_by_state)


# An interactive map
library(plotly)

state_data <- shootings_2018 %>% group_by(state) %>% summarise(
  killed = sum(num_killed),
  injured = sum(num_injured)
) %>% select(state, killed, injured)
state_code <- c(
  "AL", "AZ", "AK", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "IL", "IN", "KS",
  "KY", "LA", "MD", "MA", "MI", "MN", "MS", "MO", "NE", "NV", "NJ", "NM", "NY", "NC", "OH",
  "OK", "PA", "SC", "TN", "TX", "UT", "VA", "WA", "WI"
)

state_data <- state_data %>% mutate(state_code = state_code)

state_data$hover <- with(state_data, paste("state:", state_data$state, "<br>",
  "Number of people killed: ", state_data$killed,
  "<br>", "Number of people injured: ", state_data$injured,
  sep = ""
))
# give state boundaries a white border
color_background <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
mapping_usa <- list(
  scope = "usa",
  projection = list(type = " albers usa"),
  showlakes = TRUE,
  lakecolor = toRGB("white")
)

ploting_num_shooting_by_states <- plot_geo(state_data, locationmode = "USA-states") %>%
  add_trace(
    z = ~killed, text = ~hover, locations = ~state_code,
    color = ~killed, colors = "Reds"
  ) %>%
  colorbar(title = "Number of people killed") %>%
  layout(
    title = "2018 US shooting by State",
    geo = mapping_usa
  )

ploting_num_shooting_by_states





# A plot of your choice


dash_plot <- ggplot(
  data = state_data, mapping =
    aes(x = state_code, y = state_data$killed)
) +
  geom_boxplot(alpha = 0)
