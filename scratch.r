library(tidyverse)
library(urbnmapr)
acc <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/accident.csv", stringsAsFactors = FALSE)
per <- read.csv("https://raw.githubusercontent.com/xdaiISU/ds202materials/master/hwlabs/fars2017/person.csv", stringsAsFactors = FALSE)

df <- left_join(acc, per)


# Question 1

fatal <- df %>% filter(INJ_SEV == 4)

#Question 2

make <- fatal %>% select(STATE,FATALS, MAKE, COUNTY) %>% drop_na() %>% group_by(STATE,MAKE) %>% summarise(totalByMake = sum(FATALS)) %>% 
  arrange(-totalByMake) %>% top_n(1) %>% arrange(STATE)

#Question 3

codes <- read_xlsx("GLC.xlsx") %>% mutate(`State Code` = as.integer(`State Code`), StateCode = `State Code`) %>% select(StateCode, `State Name`) %>% 
  unique

map <- left_join(make,codes, by = c("STATE" = "StateCode")) %>% mutate(region = tolower(`State Name`)) %>% select(-`State Name`)

states <- map_data("state")
statesLab <- states %>% group_by(region) %>% summarize(long=mean(long), lat = mean(lat))

makeLab <- left_join(map,statesLab)


ggplot(states, aes(long,lat)) + geom_path(aes(group = group)) + coord_map() + geom_text(aes(label=MAKE), color='black', data=makeLab)

The most dangerous vehicle is measured by the total number of fatalities associated with that vehicle make. 
The overwhelmingly most dangerous vehichle in many states is listed as 20, which equates to Chevrolet.

# Question 4

The datasets were joined as part of my initial setup. The joined variables were STATE, ST_CASE, VEFORMS, COUNTY, DAY, MONTH, HOUR, MINUTE, 
RUR_URB, FUNC_SYS, HARM_EV, MAN_COLL, and SCH_BUS.

# Question 5

week <- df %>% group_by(DAY_WEEK) %>% summarise(Total = n())

hour <- df %>% group_by(HOUR) %>% summarise(Total = n()) %>% filter(HOUR < 26)

#1- Male, 2- Female 8- Not Reported 9- Unknown
sex <- df %>% group_by(SEX) %>% summarise(Total = n())

#Saturday
ggplot(week, aes(DAY_WEEK, Total, fill = as.factor(DAY_WEEK))) + geom_bar(stat="identity")
#6pm
ggplot(hour, aes(HOUR, Total)) + geom_bar(stat="identity")
#Men
ggplot(sex, aes(SEX, Total, fill = as.factor(SEX))) + geom_bar(stat="identity")

The groups that had the most accidents were Saturdays, 6pm, and Men.

# Question 6

counties <- counties %>% mutate(county_fips = as.integer(county_fips))
county <- fatal %>% select(STATE, COUNTY, FATALS) %>% group_by(STATE, COUNTY) %>% summarise(Total = sum(FATALS)) %>%
  mutate(county_fips = (STATE*1000)+COUNTY)

countydata <- left_join(county, counties, by = "county_fips") 

ggplot(countydata, aes(long, lat, group = group, fill = Total)) + geom_polygon(color = NA) + coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(fill = "Fatalities")

There were many categories in the midwest specifically that had no fatalities. 
Los Angeles California and the surrounding area had by far the highest fatalities.

# Question 7

winter <- fatal %>% select(STATE, MONTH, FATALS) %>% group_by(STATE, MONTH) %>% 
  summarise(Total = sum(FATALS)) %>% filter(MONTH == 1 | MONTH == 2 | MONTH == 12) %>% summarise(winterTotal = sum(Total))

summer <- fatal %>% select(STATE, MONTH, FATALS) %>% group_by(STATE, MONTH) %>% 
  summarise(Total = sum(FATALS)) %>% filter(MONTH == 6 | MONTH == 7 | MONTH == 8) %>% summarise(summerTotal = sum(Total))

months <- full_join(winter, summer, by=("STATE")) %>% mutate(Summer = winterTotal<summerTotal, Summer = as.integer(Summer))

monthsMap <- states %>% left_join(months, by=c("group" = "STATE"))

ggplot(monthsMap, aes(x=long, y= lat, group = group, fill = as.factor(Summer))) + geom_polygon() + coord_map()

There were more states that had more accidents in the summer than in the winter which suggests that summer had more fatal crashes and is therefore 
more dangerous. However this does seem to vary based on state, for example California had more fatalities in winter.
