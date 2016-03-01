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

dem_rows<-grep("Dem.", presidential_data$Party)
dem_data<-presidential_data[dem_rows,]
dem_years<-dem_data$Year
dem_turnout<-dem_data$Turnout
dem_turnout<-as.numeric(sub("%", "", dem_turnout))

rep_rows<-grep("Rep.", presidential_data$Party)
rep_data<-presidential_data[rep_rows,]
rep_years<-rep_data$Year
rep_turnout<-rep_data$Turnout
rep_turnout<-as.numeric(sub("%","", rep_turnout))

independent_rows<-c(1, 17, 20)
independent_data<-presidential_data[independent_rows,]
independent_years<-independent_data$Year
independent_turnout<-independent_data$Turnout
independent_turnout<-as.numeric(sub("%", "", independent_turnout))

turnout<-presidential_data$Turnout
turnout<-as.numeric(sub("%", "", turnout))
year<-presidential_data$Year

turnout_regression<-lm(turnout~year)
turnout_coefficients<-turnout_regression$coefficients

dem_rep_rows<-c(dem_rows,rep_rows)


plot(NULL, NULL,
     xlab = "Year", 
     ylab = "Turnout in Percent", 
     pch = 20,
     main = "Turnout in Percent by Year",
     xlim = c(1800,2020),
     ylim = c(20, 90))
points(dem_years, dem_turnout, pch = 20, col = 'blue')
points(rep_years, rep_turnout, pch = 20, col = 'red')
points(independent_years, independent_turnout, pch = 20, col = "green")
par(bg = 'white')
legend(x = 1962, y = 90,
       legend = c("Democrats", "Republicans", "Other"),
       pch = 20,
       col = c('blue', 'red', 'green'),
       bg = 'grey',
       cex = .7,
       title = "Legend",
       pt.cex = 1)
abline(a = turnout_coefficients[1], b = turnout_coefficients[2], lty = 2)
legend(x = 1900, y = 40,
       legend = "Turnout = 250 - .0976 * Year",
       cex = .7)

       
?legend

dem_pop_vote_percent<-dem_data$Popular.vote....
dem_pop_vote_percent<-as.numeric(sub("%", "", dem_pop_vote_percent))

rep_pop_vote_percent<-rep_data$Popular.vote....
rep_pop_vote_percent<-as.numeric(sub("%", "", rep_pop_vote_percent))

independent_vote_percent<-independent_data$Popular.vote....
independent_vote_percent<-as.numeric(sub("%", "", independent_vote_percent))

plot(NULL, NULL,
     xlab = "Year",
     ylab = "Winning Vote Percentage",
     main = "Winning Vote Percentage by Year",
     ylim = c(20,70),
     xlim = c(1820, 2020))
points(dem_years, dem_pop_vote_percent,
       pch = 20,
       col = 'blue')
points(rep_years, rep_pop_vote_percent,
       pch = 20,
       col = 'red')
points(independent_years, independent_vote_percent,
       pch = 20,
       col = 'yellow')
par(bg = 'grey')



plot(dem_years, dem_pop_vote_percent,
     main = "Winning Percent",
     pch = 20,
     col = 'blue')
par(bg = 'white')

dem_data$Popular.vote


