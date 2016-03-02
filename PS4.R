library(rvest)    
wikiURL <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'

## Grab the tables from the page and use the html_table function to extract the tables.
## You need to subset temp to find the data you're interested in (HINT: html_table())

temp <- wikiURL %>% 
  read_html %>%
  html_nodes("table")

##taking the actual data table we want from the website
presidential_data<-(html_table(temp[2]))

presidential_data<-as.data.frame(presidential_data) #turning it into a dataframe

presidential_data<-presidential_data[3:50,] #eliminating the first two rows, which were blank or useless

##now I'm extracting data by party so I can plot each party's data in a different color later

dem_rows<-grep("Dem.", presidential_data$Party) #finding out which rows are democratic wins
dem_data<-presidential_data[dem_rows,] #subsetting the dataframe for just democratic rows
dem_years<-dem_data$Year #creating an object of democratic win years
dem_turnout<-dem_data$Turnout #creating an object of turnout percentege in democratic rows
dem_turnout<-as.numeric(sub("%", "", dem_turnout)) #turning the percenteges into numbers

##repeating this for the republican rows

rep_rows<-grep("Rep.", presidential_data$Party)
rep_data<-presidential_data[rep_rows,]
rep_years<-rep_data$Year
rep_turnout<-rep_data$Turnout
rep_turnout<-as.numeric(sub("%","", rep_turnout))

##repeating this for the other party rows

independent_rows<-c(1, 17, 20)
independent_data<-presidential_data[independent_rows,]
independent_years<-independent_data$Year
independent_turnout<-independent_data$Turnout
independent_turnout<-as.numeric(sub("%", "", independent_turnout))

##extracting the turnout and year data for a regression line

turnout<-presidential_data$Turnout #creating an object for turnout
turnout<-as.numeric(sub("%", "", turnout)) #making the turnout a number
year<-presidential_data$Year #creating an object for years

turnout_regression<-lm(turnout~year) #regression turnout by years
turnout_coefficients<-turnout_regression$coefficients #creating an object for the coefficients

## "Presidential Turnout in Percent by Year" 
## creating the first plot
## this plot displays the turnout percent by year


plot(NULL, NULL, #setting up a blank plot
     #adding axis labels
     xlab = "Year", 
     ylab = "Turnout in Percent", 
     #defining point type
     pch = 20,
     #creating a title
     main = "Presidential Turnout in Percent by Year",
     #setting the axis scales
     xlim = c(1800,2020),
     ylim = c(20, 90))
points(dem_years, dem_turnout, pch = 20, col = 'blue') #adding the democratic points
points(rep_years, rep_turnout, pch = 20, col = 'red') #adding the republican points
points(independent_years, independent_turnout, pch = 20, col = "black") #adding the other points
par(bg = 'white') #making a white background for the chart
#creating a legend for the different colored points
legend(x = 1962, y = 90, #setting the location
       #writing the content for the legend
       legend = c("Democrats", "Republicans", "Other"),
       #making sure the point size and color are coordinated
       pch = 20,
       col = c('blue', 'red', 'black'),
       #creating a grey bakcground
       bg = 'grey',
       #setting the size
       cex = .6,
       #creating the legend title
       title = "Legend",
       pt.cex = 1)
## adding a regression line using the coefficients from the linear regression
abline(a = turnout_coefficients[1], b = turnout_coefficients[2], lty = 2)
## adding a "legend" that displays the regression function
legend(x = 1920, y = 40,
       legend = "Turnout = 250 - .0976 * Year",
       cex = 1.1,
       box.lwd = 0)


## now extracting popular vote percent data for the second chart
dem_pop_vote_percent<-dem_data$Popular.vote.... #making an object for the popular vote percent
dem_pop_vote_percent<-as.numeric(sub("%", "", dem_pop_vote_percent)) #making it a numeric

## doing this for the republicans
rep_pop_vote_percent<-rep_data$Popular.vote....
rep_pop_vote_percent<-as.numeric(sub("%", "", rep_pop_vote_percent))

## doing this for the other data rows
independent_vote_percent<-independent_data$Popular.vote....
independent_vote_percent<-as.numeric(sub("%", "", independent_vote_percent))

## "Winning Presidential Vote Percentage by Years"
## creating the second plot
## this will plot the winning voting percentage by year, colored by party

plot(NULL, NULL, #setting up a blank plot
     ## creating x and y labels
     xlab = "Year",
     ylab = "Winning Vote Percentage",
     ## writing the title
     main = "Winning Presidential Vote Percentage by Year",
     ## setting the x and y limits
     ylim = c(20,70),
     xlim = c(1820, 2020))
## adding the democratic points
points(dem_years, dem_pop_vote_percent,
       pch = 20,
       col = 'blue')
## adding republican points
points(rep_years, rep_pop_vote_percent,
       pch = 20,
       col = 'red')
## adding the other points
points(independent_years, independent_vote_percent,
       pch = 20,
       col = 'black')
## setting the white background
par(bg = 'white')
## creating a legend
legend(x = 1968, y = 40, #sets the location for the legend
       ## creating the content for the legend
       legend = c("Democrats", "Republicans", "Other"),
       ## making sure the point type and color match the chart
       pch = 20,
       col = c('blue', 'red', 'black'),
       #creating the background color
       bg = 'grey',
       #setting the size
       cex = .5,
       #creating the title
       title = "Legend",
       pt.cex = 1)
## adding a line at 50%
abline(a = 50, b = 0,
       lty = 3)
## adding a brief explanation for the line
legend(x = 1820, y = 25,
       legend = "Line represents 50% threshhold, not a regression line",
       cex = 1,
       box.lwd = 0)



