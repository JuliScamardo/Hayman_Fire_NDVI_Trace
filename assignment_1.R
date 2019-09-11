library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
files <- list.files('data',full.names=T,pattern='.csv')


#Read in individual data files
ndmi <- read_csv(files[1]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndmi')


ndsi <- read_csv(files[2]) %>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndsi')

ndvi <- read_csv(files[3])%>% 
  rename(burned=2,unburned=3) %>%
  mutate(data='ndvi')

# Stack as a tidy dataset
full_long <- rbind(ndvi,ndmi,ndsi) %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
#convert the full_long dataset in to a wide dataset using the 
#function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

## Your code here

#Creating a wide dataset from full_long
full_wide <- spread(full_long, key = "data", value = "value") %>%
  filter_if(is.numeric, all_vars(!is.na(.)))%>%
  mutate(month = month(DateTime)) %>%
  mutate(year = year(DateTime))

#summary to look at full_wide
summary(full_wide)

#Subsetting summer months to compare NDVI and NDMI for green months
summer_only <- filter(full_wide, month %in% c(6,7,8,9))

#Plotting NDMI v. NDVI for comparison
ggplot(summer_only,aes(x = ndmi, y = ndvi, color = site)) +
  xlab('NDMI') +
  ylab('NDVI') +
  ggtitle('NDMI and NDVI Comparison') +
  geom_point() +
  scale_color_economist()+
  theme_few()+
  theme(legend.position=c(0.85,0.8))

#Answer: NDMI and NDVI are positively correlated at both burned
#and unburned sites. 

## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 


## Your code here

#Calculate average NDSI for winter months
ndsi_annual <- select(full_wide, 'site', 'ndsi', 'month', 'year') %>%
  filter(month %in% c(1,2,3,4)) %>%
  group_by(site,year) %>%
  summarize(mean_NDSI=mean(ndsi))

#Calculate average NDVI for summer months
ndvi_annual <- select(full_wide, 'site', 'ndvi', 'month', 'year') %>%
  filter(month %in% c(6,7,8)) %>%
  group_by(site, year) %>%
  summarise(mean_NDVI = mean(ndvi))

#Join NDSI and NDVI
annual_comparison <- inner_join(ndvi_annual, ndsi_annual, by = c('year', 'site'))

#Plot comparison between NDSI and NDVI
ggplot(annual_comparison, aes(x = mean_NDSI, y = mean_NDVI)) + 
  geom_point(colour = 'darkslategray4')+
  xlab('Mean Annual Winter NDSI') + 
  ylab('Mean Annual Summer NDVI') +
  ggtitle('Mean annual NDSI and NDVI comparison')+
  theme_few()

#Answer: There is no correlation between mean annual NDSI (snow) and 
#mean annual NDVI (greenness) 

## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here

#Burned/unburned comparison
ggplot(annual_comparison, aes(x = mean_NDSI, y = mean_NDVI, color = site)) +
  geom_point() + 
  xlab('Mean Annual Winter NDSI') +
  ylab('Mean Annual Summer NDVI') +
  ggtitle('Burned and unburned NDSI and NDVI comparison')+
  scale_color_few()+
  theme_few()+
  theme(legend.position=c(0.85,0.2))


#Pre/Post comparison
annual_pre_post <- annual_comparison %>% 
  mutate(treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn')))


#Scatterplot showing pre- and post- burn NDVI vs. NDSI at 
#burned and unburned sites
annual_pre_post %>%
  ggplot(.,aes(x = mean_NDSI, y = mean_NDVI, color = treatment)) + 
  geom_point() +
  xlab('Mean Annual Winter NDSI') +
  ylab('Mean Annual Summer NDVI') +
  scale_color_solarized()+
  theme_few()+
  theme(legend.position=c(0.85,0.2)) +
  facet_wrap(~site)


#Answer: There is still no correlation between NDSI and NDVI, but NDVI remains
#higher at the unburned site

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

#What is the overall greenest month? 
ndvi_month <- select(full_wide, 'site', 'ndvi', 'month', 'year') %>%
  group_by(month) %>%
  summarise(avg_month_NDVI = mean(ndvi)) %>%
  arrange(desc(avg_month_NDVI)) 
ndvi_month[1,]
#ANSWER: Overall, August is the greenest month

#Does this change in the burned plots after fire?
ndvi_month_pre_post <- select(full_wide, 'site', 'ndvi', 'month', 'year') %>%
  mutate(treatment = cut(year,breaks=c(0,2003,2020),
                         labels=c('pre-burn','post-burn'))) %>%
  filter(site == "burned") %>%
  group_by(month, treatment)%>%
  summarise(avg_month_prepost_ndvi = mean(ndvi)) %>%
  arrange(treatment, desc(avg_month_prepost_ndvi))

ndvi_month_pre_post[1,]
ndvi_month_pre_post[13,]

#ANSWER: August has the highest NDVI both pre- and post- burn, but average NDVI
#is lower post-burn.


##### Question 5 ####
#What month is the snowiest on average?
snowiest <- select(full_wide, 'site', 'ndsi', 'month') %>%
  group_by(month) %>%
  summarise(avg_month_ndsi = mean(ndsi)) %>%
  arrange(desc(avg_month_ndsi))

snowiest[1,]

#ANSWER: The snowiest month on average is January
  
