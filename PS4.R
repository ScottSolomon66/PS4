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

year<-as.numeric(as.character(presidential_data$Year))
turnout<-presidential_data$Turnout
turnout<-as.numeric(sub("%", "", turnout))

turnout_by_year<-cbind(presidential_data$Year, presidential_data$Turnout)

turnout_by_year<-as.matrix(turnout_by_year)

dem_years<-dem_data$Year
dem_turnout<-dem_data$Turnout
dem_turnout<-as.numeric(sub("%", "", dem_turnout))

plot(NULL, NULL,xlab = "Year", 
     ylab = "Turnout in Percent", 
     pch = 20,
     main = "Turnout in Percent by Year",
     xlim = c(1800,2020),
     ylim = c(20, 90))
points(dem_years, dem_turnout, pch = 20, col = 132)


dem_rows<-grep("Dem.", presidential_data$Party)

dem_data<-presidential_data[dem_rows,]



