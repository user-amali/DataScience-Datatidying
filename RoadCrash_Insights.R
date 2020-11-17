## Australian fatal crashes and fatalities 
## This project performs data cleaning, data wrangling, and
## exploratory data analysis to obtain insights about road crashes.

## 1. Fatalities by State given Year as Input
## 2. Fatalities by Month for one or more Year(s)
## 3. Fatalities by Dayweek in a given period to
## see if there are more accidents in any day of the week.
## 4. Fatalities by Age group and Gender.
## 5. Fatalities by Year over Year 

library(dplyr) 
library(ggplot2) 
library(tidyr) 
library(tidyverse)

source('~/RoadCrash_Utility.R')

# 1. Data representation 
# R code used to import fatal crashes and fatalities 
#setwd("~/FDS_Capstone") 
# Read two csv files using read.table function
fatalcrashes <- read.table(file="Data files/bitrearddfatalcrashesjanuary2018.csv",
                           header = TRUE, sep = ",") 
#View(fatalcrashes) 
fatalities <- read.table(file="Data files/bitrearddfatalitiesjanuary2018.csv",
                         header = TRUE, sep = ",")

# Natural inner join is used to join two tables based on
# the Primary Key (Crash.ID). Only rows having matching
# Crash.ID will be joined, and the unmatched rows will not return.
Onedatanatural <- inner_join(fatalcrashes, 
                            select(fatalities, Crash.ID, Road.User,
                                  Gender, Age), 
                             by = ("Crash.ID"),
                             keep = FALSE) 
summary(Onedatanatural)

# Displaying the first ten records of fatalcrashes
head(Onedatanatural, 10)

# View(fatalities) 
# Assigning easy to access and meaningful names to the columns
names(Onedatanatural) <- c("CrashID", "State", "Month", "Year", 
                         "Dayweek", "Time", "CrashType", 
                         "NumberOfFatalities", 
                         "Bus", 
                         "RigidTruck", "ArticulatedTruck", "SpeedLimit",
                         "RoadUser", "Gender", "Age") 

# 2. Type conversion 
str(Onedatanatural)
## 

## Time is a factor with many levels. This will be converted to a character
## vector.
is.ordered(Onedatanatural$Month)

is.ordered(Onedatanatural$Dayweek)
## Month and Dayweek are not ordered factor variables. So we will convert them
## to ordered variables in fatalcrashes and fatalities.

# converting month to ordered factor 
month_name = c("January", "February", "March", "April",
               "May", "June", "July", "August",
               "September", "October", "November", "December") 
Onedatanatural$Month <- factor(as.character(Onedatanatural$Month),
                             levels = month_name,
                             ordered = TRUE)  

summary(Onedatanatural$Month)

# converting dayweek to ordered factor 
day_week = c("Sunday", "Monday", "Tuesday",
             "Wednesday", "Thursday", "Friday", 
             "Saturday") 
Onedatanatural$Dayweek <- factor(as.character(Onedatanatural$Dayweek),
                             levels = day_week, 
                             ordered = TRUE) 

# converting time of factor var to char since the associated date  
# is not available in the dataset
head(Onedatanatural$Time, 100)
Onedatanatural$Time <- as.character(Onedatanatural$Time) 

#3. Missing value imputation 
# BusInvolvement - -9 is a missing value 
# RigidTruckInvolvement - -9 is a missing value or unknown 
# ArticulatedTruckInvolvement - -9 is a missing or unknown value 
# SpeedLimit - -9,900 
# Gender - -9 for unknown and Unspecified 
# Age - -9 for unknown 

summary(Onedatanatural) 
## Looking at the summary data, variables Bus, RigidTruck, Speed,  
# articulated truck, Age and Gender have -9 invalid values. 
# Since articulated truck has a very large number of -9/ unknown values, 
# the -9 values will be changed to "No", but for other features, 
# these rows will be removed to produce clean dataset. 


logical_val = c("Yes", "No") 
Onedatanatural <- Onedatanatural %>% 
                  mutate(RigidTruck = ifelse(RigidTruck == "-9", "No",
                                      as.character(RigidTruck) ) ) 
str(Onedatanatural$RigidTruck)

summary(Onedatanatural$SpeedLimit)
# Summary results show there are some invalid values -9, 900.
# These records will be filtered out since removing them
# didn't delete a large number of rows.
Onedatanatural <- filter(Onedatanatural, 
         (SpeedLimit > 0) & (SpeedLimit < 130 ) ) 

summary(Onedatanatural$Age)
# Summary results on Age show -ve values for Age. So removing
# those records from the dataset.
Onedatanatural <- filter(Onedatanatural, 
                         (Age > 0) )

summary(Onedatanatural)

ggplot(data = Onedatanatural, mapping = aes(Age)) +
  geom_histogram(binwidth = 6, colour = "black", fill = "white")

# Histogram results show the highest number of crashes and accidents
# are in the age group 20 - 40. In general, the Age shows a positive
# skew with a long tail to the right.

# Using this breaks to bin the age into 6 groups for
# easier data visualisation.

Onedatanatural <- Onedatanatural %>%
  mutate(
    Agegroup = case_when(
      between(Age, 0, 20) ~ '0-20',
      between(Age, 20, 40) ~ '20-40',
      between(Age, 40, 60) ~ '40-60',
      between(Age, 60, 80) ~ '60-80',
      between(Age, 80, 100) ~ '80-100',
      between(Age, 100, 120) ~ '100-120'
    )
  )

agegrplevel <- c('0-20', '20-40', '40-60',
                 '60-80', '80-100', '100-120')

Onedatanatural <- mutate(Onedatanatural, 
                         Agegroup = factor(Onedatanatural$Agegroup,
                                           levels = agegrplevel ) )

Onedatanatural <- Onedatanatural %>%
                  filter( (Gender != '-9') &
                         (ArticulatedTruck != '-9') )

## Use of functions

# 4. Data subset selection and/or subsampling 
# filter out only the records for the year 2017, 2016, and 2015 
# from fatalcrashes data 

Year171615 <- filterYeardata(Onedatanatural,
                             c(2017, 2016, 2015) )

summary(Year171615)

# write.table(Year171615, file = "Data files/Year18171615.csv", 
#             append = FALSE, quote = TRUE, sep = ",",  
#             eol = "\n", na = "NA", dec = ".", 
#             row.names = FALSE, col.names = TRUE, 
#             qmethod = c("escape", "double"),  fileEncoding = "") 

# 5. stratified subsampling  
# fatalcrash_grouped <- group_by(Year18171615, Year, Month) 
# fatalcrash_stratified_balanced <- sample_n(fatalcrash_grouped, size = 50, 
#                                            replace = FALSE) 

# 6. Group-based data summarisation
# Perform yearly summarisation of the number of fatalities 
# Here, we are trying to find the number of deaths across all states in each Year

YearbyYear <- Onedatanatural %>%
  group_by(Year) %>%
  summarise(Number_of_rows = n(), No_of_deaths = sum(NumberOfFatalities)) 

# Summary output is a table with a row for each year. Each Year's records
# are grouped and only no_deaths summary is calculated.
# Now, visualising the timeseries plot using ggplot 

ggplot(data = YearbyYear[YearbyYear$Year != 2018,], 
       mapping = aes(x = Year, y = No_of_deaths)) +
  geom_point() +
  geom_line(colour = "red") + 
  labs(title="Number of fatalities across all States 1989-2017",
       xlab = "Year",
       ylab = "Number")  
# As you can see from the chart, the overall trend in fatalities has 
# been decreasing 
# significantly since 1990, 2000 and 2010 and is still trending downwards.

# Number of fatalities summary for each state with the year and month included 
Y2 <- Year171615 %>%   
  group_by(State, Year) %>%
  summarise(Number_of_rows = n(), no_deaths = sum(NumberOfFatalities) ) %>%
  arrange(desc(no_deaths))

ordered_state <- reorder(Y2$State, Y2$no_deaths)

ggplot(data = Y2, 
       mapping = aes(x = ordered_state, y = no_deaths, fill = as.factor(Y2$Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title="Comparison across all States 2015-2017",
       xlab = "Year",
       ylab = "Number")
# NSW tops the list with significantly more deaths from fatal crashes 
# than any other state.
# VIC, QLD are second and third in their total number of deaths 
# from fatal accidents. Finally, ACT is in the last with the 
# least number of deaths resulting from road accidents.

( Weekdays <- Year171615 %>%   
    group_by(Year, Month, Dayweek) %>%  
    summarise(no_deaths = sum(NumberOfFatalities) )
)

(ordered_weekdays <- reorder(Weekdays$Dayweek, Weekdays$no_deaths))

# Visualising Y3 data
ggplot(data = Weekdays, 
       mapping = aes(x = ordered_weekdays, y = no_deaths, fill = as.factor(Year))) +
  geom_bar(stat = "identity") +
  labs(title = "Weekdays comparison for 2015-2017",
       xlab = "Weekdays",
       ylab = "Deaths")

# When all three years are combined, Saturday has the most number of deaths.
# With Sunday and Friday having the next highest numbers, weekends appear
# to have a higher number of crashes than other days.

( Monthly <- Year171615 %>%   
  group_by(Year, Month) %>%  
  summarise(no_deaths = sum(NumberOfFatalities), 
            maxAge = max(Age), minAge = min(Age),    
            no_singleveh = sum(CrashType == 'Single vehicle'),            
            no_Pedestrian = sum(CrashType == 'Pedestrian'),     
            no_multiple = sum(CrashType == 'Multiple vehicle')) )

# Y3 has a row for each unique Year and Month combination. Since there are
# 3 years and 12 months data/ Year, the summarised result 
# has 36 rows, which was grouped by Year and Month.

# Visualising Y3 data
ggplot(data = Monthly, 
       mapping = aes(x = Month, y = no_deaths, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Month-by-Month comparison for 2015-2017",
       xlab = "Month",
       ylab = "Deaths")

# The months February, March, and October have the lowest number of deaths 
# of all months for 2015. However, there is not a single month where you see the
# lowest deaths or a month where you see the highest deaths consecutively 
# in the 3 years. It seems that all months have slightly more or less similar
# proportions of no_deaths.

# Rolling Y3 up to calculate the yearly summary of maximum
# Age, minimum Age, number of single vehicle, Pedestrian, and multiple
# vehicle crashes

Yearly <- 
  summarise(Monthly, 
            no_deaths = sum(no_deaths), 
            maxAge = max(maxAge),
            minAge = min(minAge),    
            no_singleveh = sum(no_singleveh),            
            no_Pedestrian = sum(no_Pedestrian),     
            no_multiple = sum(no_multiple) )



#7. Exploratory visualisation using ggplot2 

# Bar graph by Age 
# Create Age groups
Y5 <- Year171615 %>%  
      group_by(Agegroup) %>%
      summarise(no_of_fatalities = sum(NumberOfFatalities) )

(ordered_agegroup <- reorder(Y5$Agegroup, Y5$no_of_fatalities))

ggplot(data = Y5) +
  geom_bar(mapping = aes(x = ordered_agegroup, 
                       y = no_of_fatalities, 
                       fill = 'red'),
         na.rm = TRUE,
         stat = "identity") +
  labs(title = "Fatalities by Agegroup 2015-2017")
# This chart gives the number of fatalities by agegroup. The 
# agegroup 20-40 has the highest number of deaths(around 1400) from crashes
# in 2015-2017. The next highest group is 40-60 with 1100 deaths.

( Y6 <- Year171615 %>%  
  group_by(Agegroup, Gender) )

ggplot(data = Y6) +
  geom_bar(mapping = aes(x = Agegroup, 
                         fill = Gender),
           na.rm = TRUE,
           position = "fill") +
  geom_abline(slope = 0, intercept = 0.5, color = "green") +
  labs(title = "Fatal accidents by Agegroup and Gender 2015-2017")
# As you can see, the proportion of males is higher than females
# in all age groups involved in fatal road crashes in 2015-2017.

Year171615 %>% 
  filter((Gender != -9) & (RoadUser != 9) & (RoadUser != 'Other')) %>%   
  ggplot(mapping = aes(x = RoadUser, y = Age)) +  
  geom_boxplot(fill = "Pink") + 
  labs(title = "Killed RoadUser type and Range of Age",
       x = "RoadUser", y = "Age") 

# You can observe many insights from these boxplots which show the Age distribution
# separately for each RoadUser.
# The median age of Passenger is much younger than the driver. There
# is also a more number of road users above the median.
# The median age of Pedestrian died from crash is older than the drivers.

Year171615 %>%   
  filter((Gender != -9) & (RoadUser != 9)) %>%   
  ggplot(mapping = aes(x = CrashType, 
                       y = NumberOfFatalities,
                       fill = Gender
                       )) +   
  geom_bar(stat = "identity") + 
  labs(title = "Fatalities by CrashType",   
       x = "CrashType", y = "NumberOfFatalities")   

# Single vehicle and multiple vehicle crashes are about
# the same size. Pedestrian involved crashes are relatively a much
# smaller proportion.

