library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

presidential_data<-(html_table(temp[2]))

presidential_data<-as.data.frame(presidential_data)

presidential_data<-presidential_data[3:50,]

presidential_data<-as.data.frame(presidential_data, row.names = presidential_data$Year)

