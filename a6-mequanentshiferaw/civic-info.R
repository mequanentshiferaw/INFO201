library(dplyr)
library(httr)
library(jsonlite)

source("api-keys.R")

political_representative <- function(address) {
  base_url <- ("https://www.googleapis.com/civicinfo/v2")
  endpoint <- "/representatives"

  url <- paste0(base_url, endpoint)
  query_parameters <- list(key = civic_key, address = "seattle")
  response <- GET(url, query = query_parameters)
  # convert data to json and flattening data
  representative_info <- content(response, "text") %>% fromJSON()




  officals <- (representative_info$officials)

  offices <- (representative_info$offices)
  # just adding the postion into officals_convert


  num_to_rep <- unlist(lapply(representative_info$offices$officialIndices, length))

  expanded <- offices[rep(row.names(offices), num_to_rep), ]

  officials <- officals %>% mutate(index = row_number() - 1)

  expanded <- expanded %>%
    mutate(index = row_number() - 1) %>%
    rename(Position = name)

  officals$index <- as.integer(rownames(officals)) - 1
  mergedData <- left_join(expanded, officals, by = "index")

  example <- mergedData %>% mutate(photoUrl = paste0("![](", photoUrl, ")"))
  list_of_representative <- example %>%
    select(name, Position, party, emails, phones, photoUrl) %>%
    rename(Name = name, Email = emails, Party = party, Phone = phones, Photo = photoUrl)
  photo_NA <- is.na(list_of_representative)
  list_of_representative[photo_NA] <- "Not available"
  list_of_representative$Email[list_of_representative$Email == "NULL"] <- "Not available"
  list_of_representative$Party[list_of_representative$Party == "Nonpartisan"] <- "NA"
  list_of_representative$Party[list_of_representative$Party == "Democratic Party"] <- "Democratic"
  list_of_representative$Party[list_of_representative$Party == "Republican Party"] <- "Republican"
  list_of_representative$Photo[list_of_representative$Photo == "![](NA)"] <- "Not available"
  list_of_representative
}
