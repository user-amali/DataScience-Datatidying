# Script functions 

filterYeardata <- function(Onedatanatural, ylist)
{
  Onedatanatural %>% 
    filter(Year %in% ylist )
}