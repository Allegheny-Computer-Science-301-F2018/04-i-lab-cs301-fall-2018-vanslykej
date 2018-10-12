# Name: Jake Van Slyke
# Date: October 12, 2018
# Pledge: This work is mine unless otherwise cited.

# Run the below only if the library is not already installed.
# install.packages(dslabs)

library(dslabs)
library(dplyr)
library(tidyverse)
data(us_contagious_diseases)

#Question 1.

dat <- filter(us_contagious_diseases, disease == "Measles", state != "Alaska" & state != "Hawaii")
dat <- mutate(dat, per100000rate = ((count*100000)/population) * ((weeks_reporting)/52))

#create variable dat which finds measles in all states but Hawaii and Alaska
#use formula per100000rate 

#Question 2.

data_cali <- filter(dat, disease == "Measles", state == "California")
ggplot(data = data_cali, mapping = aes(x = year, y = per100000rate)) + geom_line() + geom_vline(xintercept = 1965)

#data filtered to give Measles in California
#plot of data for data_cali using per100000rate and by year

#Question 3.

dat_caliFocus <- filter(us_contagious_diseases, state == "California")

dat_caliFocus$yearBlock[dat_caliFocus$year >= 1950] <- "1950’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1960] <- "1960’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1970] <- "1970’s"
dat_caliFocus$yearBlock[dat_caliFocus$year >= 1980] <- "NA"

dat_caliFocus <- filter(dat_caliFocus, yearBlock != "NA")

ggplot(data = dat_caliFocus) + geom_bar(mapping = aes(x = state, y = count, fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

ggplot(data = dat_caliFocus) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#blocking all years except 1950, 60, and 70 in Cali
#filter for all NA in graphs
#bar graph without sqrt transformation
#bar graph with sqrt transformation
#the graph with sqrt transformation seems to have more similar variability throughout years

#Question 4.

dat_Focus <- filter(us_contagious_diseases)

dat_Focus$yearBlock[dat_Focus$year >= 1950] <- "1950's"
dat_Focus$yearBlock[dat_Focus$year >= 1960] <- "1960's"
dat_Focus$yearBlock[dat_Focus$year >= 1970] <- "1970's"
dat_Focus$yearBlock[dat_Focus$year >= 1980] <- "NA"

dat_Focus <- filter(dat_Focus, yearBlock != "NA")

ggplot(data = dat_Focus) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#blocking all years except 1950, 60, and 70 in all states
#filter for all NA in graphs
#bar graph without sqrt transformation
#bar graph with sqrt transformation
#the pattern does not hold for all states as they don't have similar variability

#Question 5.

ggplot(data = us_copy) + geom_tile(mapping = aes(x = state, y = sqrt(count), color = count)) + geom_bar(mapping = aes(x = state, y = sqrt(count), fill = yearBlock), position = "dodge", stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=-0.01))

#bar graph for states using color with geom_tile

#Question 6.

autismData <- read.csv("~/cs301F2018/classDocs/labs/04_lab/04-i-lab-cs301-fall-2018-vanslykej/src/autismData.csv", comment.char="#")

dat_Autism <- filter(autismData)

ggplot(data = dat_Autism) + geom_line(mapping = aes(x = Year.., y = Net.Growth))

#import data into file
#filter data and create graph that charts net growth per year
#Graph shows that autism is indeed increasing, however it is not proven to be
#caused by vaccines. Even though you can point to autism increasing as well as
#vaccines, there is no provable correlation between the two.


