### R: Bikeshare Project submission

##initial set up
# initial 1
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# initial 2
#adding column Gender with NA values to table wash
wash$Gender <- NA

#adding column Birth.Year with NA values to table wash
wash$Birth.Year <- NA

#adding column city to table wash
wash$City <- 'Washington'

#adding column city to table ny
ny$City <- 'NY'

#adding column city to table chi
chi$City <- "Chicago"

#creating consolidated data frame
ny_chi_wash=rbind(ny,chi,wash)

# create a day of the week column
ny_chi_wash$Start.Weekday <- weekdays(as.Date(ny_chi_wash$Start.Time))


# initial 3
library(ggplot2)

### Question 1
## How does the trip duration compare between the three cities?

#creating a Trip Duration boxplot by City
ggplot(aes(x=City, y = Trip.Duration/60), data = ny_chi_wash)+
  geom_boxplot()+
  coord_cartesian(ylim = c(0,25))+
  ggtitle('Trip Duration Boxplot by City')+
  ylab('Trip duration (minutes)')

#getting summary statiscs
by(ny_chi_wash$Trip.Duration/60, ny_chi_wash$City, summary)

#answer:
#The median trip duration does not vary materially between the three cities; Washington has the highest 1st quartile, Mean and third quartile suggesting a higher occurrence of longer trips despite the fact it is the city with the smallest area among the sample:

#Washington: 68 square miles
#Chicago: 227 square miles
#NYC: 469 square miles

###Question 2
##What is the most commom day of the week in each city?

#re-ordering day of the week
ny_chi_wash$Start.Weekday <- factor(ny_chi_wash$Start.Weekday,
                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

#plotting trips started per day of the week
                ggplot(aes(x = Start.Weekday), data = subset(ny_chi_wash, !is.na(Start.Weekday))) +
                  geom_bar() +
                  ggtitle('Trip Start per Day of the Week')+
                  ylab('Count of Trips')+
                  xlab('Trip Started on')+
                  theme(axis.text.x = element_text (angle=90))+
                  facet_wrap(~City)


#Creating a formula to get summary statistics
summ_statistics = function(var_input, by_input) {res = by(var_input, by_input, summary)
    return(res)}
summ_statistics (ny_chi_wash$Start.Weekday, ny_chi_wash$City)

#answer:
#In Chicago, the day of the week with the highest number of trips started is Monday; In New York and Washington it happens on Wednesdays;



###Question 3
##Which Generation account for the highest number of trips?

##Generation as defined by Neil Howe and William Strauss: 2000–: New Silent Generation or Generation Z 1980 to 2000: Millennials or Generation Y 1965 to 1979: Thirteeners or Generation X 1946 to 1964: Baby Boomers 1925 to 1945: the Silent Generation 1900 to 1924: the G.I. Generation

ny = read.csv('new_york_city.csv')

#adding column city to table ny
ny$City <- 'NY'
chi = read.csv('chicago.csv')

#adding column city to table chi
chi$City <- "Chicago"

#creating consolidated data frame for ny and chicago
ny_chi=rbind(ny,chi)

#Using ifelse to assign "Generation" based on Birth.Year
ny_chi$Generation <- ifelse (ny_chi$Birth.Year > 2000, 'The New Silent Generation',
                      ifelse (ny_chi$Birth.Year >= 1980, 'Millennials',
                       ifelse (ny_chi$Birth.Year >= 1965, 'Generation X',
                        ifelse (ny_chi$Birth.Year >= 1946,'Baby Boomers',
                         ifelse (ny_chi$Birth.Year >= 1925, 'The Silent Generation',
                          ifelse (ny_chi$Birth.Year >= 1900, 'G.I. Generation',
                            'No Birth Year available'))))))

#re-ordering Generation (from oldest to newest)
ny_chi$Generation <- factor(ny_chi$Generation,
    levels = c("G.I. Generation", "The Silent Generation", "Baby Boomers", "Generation X", "Millennials", "The New Silent Generation"))

#plotting trips per Generation
ggplot(aes(x = Generation), data = subset(ny_chi, !is.na(Generation))) +
      geom_bar() +
      facet_wrap(~City) +
      ggtitle('Trips Started per Generation')+
      ylab('Count of Trips')+
      xlab('Generation (as defined by Howe and Strauss)')+
      theme(axis.text.x = element_text (angle=90))


#Creating a formula to get summary statistics
summ_statistics = function(var_input, by_input) {res = by(var_input, by_input, summary)
    return(res)}
summ_statistics (ny_chi$Birth.Year, ny_chi$City)

#answer:
# "Millenials" is the generation with the highest number of trips on both Chicago and New York. The summary statiscs show the median and the 3rd Quartile both fall within the Millenials' range (from years 1980 to 2000) and also that the max value is pretty close to the Millenials' end point (2000).
