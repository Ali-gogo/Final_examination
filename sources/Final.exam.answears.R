# Final Spring 2020 June 6

# Create your full name to variable 'my_fullname'
my_fullname <- "Alisherkhon Rakhmanov"

# Create your student number to variable 'my_std_no'
my_std_no <- 17012639

# Honor Policy. For this exam, you must work alone. 
# You may not aid or accept aid from other students.

# You may also consult any other non-human resources you want 
# including the course textbook, handouts, your notes, other books, and on-line materials. 


# Part 1: working with data frames 
# The followings are the R code with data frames. 
# You are to transform it into different R code with 
# Note that this repeats the analysis from Part 1, but should be
# performed using `dplyr`
# Hint : Refer exercise1.R and exercise2.R in the Chapter 11(dplyr).
#       And the keys have been already given.

install.packages("devtools")
library("devtools")

install.packages("dplyr")
library("dplyr")

install.packages("treemap")
library("treemap")


# Use the `data()` function to load the "GNI2014" dataset
data("GNI2014")

# You should now have access to the `GNI2014` data frame
# You can use `View()` to inspect it
View(GNI2014)
nrow(GNI2014)

# Description of 'GNI2014'
# Gross national income (per capita) in dollars and 
# population totals per country in 2014.
# What is the GNI per capita?
# the dollar value of a country's final income in a year, 
# divided by its population.

# What is the GNI?
# Gross National Income (GNI) is a measurement of a country's income.


# Select the different continents (continents) of the countries in this data set.
# Save this vector in a variable
continents <- GNI2014$continent

head(continents)

# Use the `unique()` function to determine how many different continents
# are represented by the data set
length(unique(continents))

# (Problem1-1)
# Filter the data set for countries in Asia
GNI2014_Asia <- GNI2014[GNI2014$continent == "Asia", ]

#(Problem1-2) Arrange the Asia countries by GNI
GNI2014_Asia <- GNI2014_Asia[order(GNI2014_Asia$GNI), ]

# (Problem1-3)
# Mutate the GNI per capita data frame to add a column 'GNI_capita'.  
# The GNI per capita is the dollar value of a country's final income in a year, 
# divided by its population

GNI2014_Asia$GNI_capita <- GNI2014_Asia$GNI

GNI2014_Asia$GNI <- (GNI2014_Asia$population * GNI2014_Asia$GNI)

#(Problem1-4)
# Filter the whole GNI2014 data set for Europe that get more
# than 2000 in the GNI per capita.
# Save this new data frame in a variable.
GNI2014_Europ_2000 <- GNI2014[GNI2014$continent == "Europe" & GNI2014$GNI > 2000, ]


#(Problem1-5)
# Of the above GNI2014, what is the ios3 of the GNI2014 with the smallest
# population?
# Hint: filter for the smallest population, then select its iso3.
smallest_population <- GNI2014_Europ_2000$iso3[GNI2014_Europ_2000$population == min(GNI2014_Europ_2000$population)]


#(Problem1-5)
# Write a function that takes a `continent_choice` and a `pop_choice` as parameters,
# and returns the country that gets the most GNI
# with having more than 'pop_choice' in the population.
# You'll need to filter more (and do some selecting)!
continent_GNI_filter <- function(continent_choice, pop_choice) {
  filtered <- GNI2014[GNI2014$continent == continent_choice & GNI2014$population > pop_choice, ]
  filtered[filtered$GNI == max(filtered$GNI), "country"]
}

# What was the largest population of European country with having more than 200000 population?
continent_GNI_filter("Europe", 200000)


# Part 2 : Transform it into your code using the pipe operator

# Install the `"nycflights13"` package. Load (`library()`) the package.
# You'll also need to load `dplyr`
install.packages("nycflights13")
library(nycflights13)
library(dplyr)

# The data frame `flights` should now be accessible to you.
# Use functions to inspect it: how many rows and columns does it have?
# What are the names of the columns?
# Use `??flights` to search for documentation on the data set (for what the
# columns represent)
nrow(flights)
ncol(flights)
colnames(flights)

View(flights)
unique(flights$month)

# Which month had the greatest average departure delay?
# Save this as a data frame `greatest_dep_delay_by_month`

# you will first want to group the data by month.
# since you’re interested in the average departure delay, 
# you will want to summarize those groups to aggregate them.
# Because you can't take the mean() of NA values, you can do this by passing an na.rm=TRUE argument.
# You need to figure out the greatest average departure delay —
# which means you need to find the flights that were delayed (filtering for them).
# Finally, you can choose (select) the month of that group.
# You must use the pipe operator.


#
# Part 3
#
# 2010~2016 Korea(ROK) Tourism Receipts_Expenditures

library(dplyr)

setwd("sources")

tbl1 <- read.csv("2010-16-Table.csv", header = T)

head(tbl1)
colnames(tbl1)

# Plot line graph with different colors.

Tourism_Receipts_2011 <- tbl1 %>% 
  filter(YEAR==2011) %>% 
  select(YEAR, TOURISM.RECEIPTS)

Tourism_Receipts_2016 <- tbl1 %>% 
  filter(YEAR==2016) %>% 
  select(YEAR, TOURISM.RECEIPTS)

month = 1:12
tr_2011 <- Tourism_Receipts_2011$TOURISM.RECEIPTS
tr_2016 <- Tourism_Receipts_2016$TOURISM.RECEIPTS


plot(month,                                # x data
     tr_2011,                                # y data
     main="Tourism Receipts",
     type= "b",                            # Type of a graph 
     lty=1,                                # Type of a line
     col="red",                            # line color          
     xlab="Month ",                        # x axis
     ylab="Receipts",                      # y axis
     #     ylim=c(1, 15)                         # y values(min, max)
)
lines(month,                               # x data
      tr_2016,                               # y data
      type = "b",                          # line type
      col = "blue")                        # line color

# Plot line graph with different colors by using 'ggplot2' and 'dplyr'.
# Libraries
install.packages("babynames")
library("babynames")
library(ggplot2)
library(babynames) # provide the dataset: a dataframe called babynames
library(dplyr)

colnames(babynames)
?babynames

# Keep only 3 names

don2 <- tbl1 %>%
  filter(YEAR %in% c(2011, 2016)) 

# Plot
don2 %>%
  ggplot(aes(x=MONTH, y=TOURISM.RECEIPTS, group=YEAR, color=YEAR)) +
  geom_line()

# Problem : Plot the line graph of 2011, 2012, 2013, 2014, 2015 and 2016 with different colors.


#
# Part 4
#
# Problem : Express the circle proportional to the size of population of North America
# by using the dataset 'treemap'.

library(ggmap)

register_google(key='AIzaSyBClez4HTaT4HL08e487E1NG6T63sBxbDQ')  # Register your google key.
library(treemap)
data(GNI2014)
colnames(GNI2014)

#
# Part 5
#

# Answer the following questions.
# 1. List the three main sections of an R Markdown.

cat("The three main sections are ")

# 2. In an R Markdown, how will commonly we execute R code inline with the rest of text?
# 'r your_R_code', a single backtick(')


#3. Sharing Reports as Websites
# 
# (1) Create the repo - 'Movie_Review' on your github.
#     https://github.com/Ali-gogo/Movie_Review

# (2) Copy 'apikey.R', 'exercise.R' and 'index.Rmd' from 
#     https://github.com/peppibook/Movie_Review
# (3) Execute the 'index.Rmd' (RMarkdown file) by clicking the 'knit' button.
# (4) Push 'apikey.R', 'exercise.R', 'index.Rmd', 'index.html' into your repo - Movie_Review.
# (5) On the web portal page for your repo, click on the “Settings” tab, and scroll down to the section labeled “GitHub Pages.”
# (6) Select the “master branch” option to enable GitHub Pages. (See 18.4 Sharing Reports as Websites in the textbook)
# (7) Visit at https://GITHUB_USERNAME.github.io/Moive_Review


# Thank you! 

