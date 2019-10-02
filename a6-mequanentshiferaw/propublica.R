library(dplyr)
library(httr)
library(jsonlite)
library(ggplot2)
source("api-keys.R")

# House of Representatives Charts
member_representatives <- function(state) {
  base_url <- "https://api.propublica.org"
  endpoint <- paste0("/congress/v1/members/house/", state, "/current.json")
  url <- paste0(base_url, endpoint)

  response <- GET(url, add_headers("X-API-Key" = propublica_key))
  repe <- content(response, "text") %>% fromJSON()
  data <- repe$results$gender %>%
    as.data.frame() %>%
    flatten()
  data_party <- repe$results$party %>%
    as.data.frame() %>%
    flatten()
  # ploting Representatives by Gender
  table_f_m <- data.frame(table(data))
  table_f_m$data <- as.character(table_f_m$data)
  table_change_name <- table_f_m %>% mutate(Gender = data) %>% select(Gender, Freq)
  table_change_name[1, 1] <- "Females"
  table_change_name[2, 1] <- "Males"
  # Basic barplot
  plot <- ggplot(data = table_change_name, aes(x = Gender, y = Freq)) +
    geom_bar(stat = "identity")

  # Horizontal bar plot
  bar_horizontal_gender <- plot + coord_flip()


  # polting Representatives by Party:
  num_democrat_and_republican <- data.frame(table(data_party))
  num_democrat_and_republican$data_party <-
    as.character(num_democrat_and_republican$data_party)
  party_change_name <- num_democrat_and_republican %>%
    mutate(Party = data_party) %>%
    select(Party, Freq)
  party_change_name [1, 1] <- "Republican"
  party_change_name[2, 1] <- "Democratic"

  plot_domocrat_and_republica <- ggplot(
    data = party_change_name,
    aes(x = Party, y = Freq)
  ) + geom_bar(stat = "identity")

  bar_horizontal_party <- plot_domocrat_and_republica + coord_flip()

  gender_plot <- bar_horizontal_gender
  plot_party <- bar_horizontal_party
  plots <- list(gender = gender_plot, party = plot_party)
  plots
}
# testing the function for gender and party bar plot
testing_bar_graph <- member_representatives("wa")


#
date_birth <- function(state) {
  base_url <- "https://api.propublica.org"
  representative_end_point <- paste0("/congress/v1/members/house/", state, "/current.json")
  resource_uri <- paste0(base_url, representative_end_point)
  representative_response <- GET(
    resource_uri,
    add_headers("X-API-Key" = propublica_key)
  )
  representative_content <- content(representative_response, "text") %>%
    fromJSON()
  gitting_content <- representative_content$results
}

info_of_representative <- function(id) {
  base_url <- "https://api.propublica.org"
  representative_end_point <- paste0("/congress/v1/members/", id, "/votes.json")
  resource_uri <- paste0(base_url, representative_end_point)
  representative_response <- GET(
    resource_uri,
    add_headers("X-API-Key" = propublica_key)
  )
  representative_content <- content(representative_response, "text") %>% fromJSON()
  # Calculates recent vote agreement
  name_of_representative <- representative_content$results$votes %>%
    as.data.frame() %>%
    flatten() %>%
    select(result, position)

  num_of_Agreement <- name_of_representative %>% filter((result == "Passed" | result == "Agreed to") & position == "Yes" |
    result == "Failed" & position == "No")
  percentage <- 100 *
    (nrow(num_of_Agreement) / nrow(name_of_representative))
}
perecentage_data <- info_of_representative("A000374")


# This get you first & last name , age, and twitter_account

url <- "https://api.propublica.org"
end_point_date <- "/congress/v1/115/house/members.json"
resource_uri <- paste0(url, end_point_date)
representative_response <- GET(
  resource_uri,
  add_headers("X-API-Key" = propublica_key)
)
representative_content <- content(representative_response, "text") %>%
  fromJSON()

today <- as.Date(Sys.Date(), format = "%d/%m/%y")

gitting_content <- representative_content$results$members[[1]] %>%
  select(first_name, last_name, date_of_birth, twitter_account) %>%
  mutate(age = as.double(((today - as.Date(date_of_birth)) / 365)))
chosen <- gitting_content[1, ]
age <- as.integer(chosen$age)
first_name <- chosen$first_name
Last_name <- chosen$last_name
twitter_account <- chosen$twitter_account
