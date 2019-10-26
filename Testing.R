library(dslabs)
data("movielens")
str(movielens)
nrow(movielens)
nlevels(movielens$genres)
index <- order(murders$total)
murders$state[index]
print(max(murders$total))
print(murders$state[which.max(murders$total)])
print(rank(murders$total))
murder_rate = (murders$total/murders$population)*100000
print(murders$state[order(murder_rate,decreasing = TRUE)])
safe <- murder_rate <= 1
region <- murders$region == "West"
index <- safe & region
print(murders$state[index])
abbs <- c("AK","IA","MI")
ind <- match(abbs,murders$abb)
print(murders$state[ind])
# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU")
# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs %in% murders$abb
# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp <- 5/9 * (temp - 32)
# Create a data frame `city_temps` 
city_temps <- data.frame(city = city, temp = temp)
print(city_temps)
# Define an object `x` with the numbers 1 through 100
x <- 1:100
# Compute the sum 
sum(1/x^2)
# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 
# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which( !abbs %in% murders$abb )
# Names of abbreviations in `ind`
abbs[ind]

# Loading data
library(dslabs)
data(murders)

# Loading dplyr
library(dplyr)

# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
murders <- mutate(murders , rate = total / population*100000)

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders,rank(-rate))

# Load dplyr
library(dplyr)

# Use select to only show state names and abbreviations from murders
select(murders,state,abb)

# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter to show the top 5 states with the highest murder rates
filter(murders,rank(-rate) <= 5)

# Use filter to create a new data frame no_south
no_south <- filter(murders, region != "South")
# Use nrow() to calculate the number of rows
nrow(no_south)

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders, region %in% c("Northeast", "West"))
# Number of states (rows) in this category 
nrow(murders_nw)

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders,region %in% c("Northeast","West") & rate < 1)
# Use select to show only the state name, the murder rate and the rank
select(my_states,state,rate,rank)

# Load library
library(dplyr)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# show the result and only include the state, rate, and rank columns, all in one line
filter(murders,region %in% c("Northeast","West") & rate < 1) %>% select(state,rate,rank)

# Loading the libraries
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>%  mutate( rate =  total / population * 100000, rank = rank(-rate)) %>% filter( region %in% c("Northeast","West") & rate < 1) %>% select(state,rate,rank)

# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

population_in_millions <- murders$population*10^6
# Transform population using the log10 transformation and save to object log10_population
log10_population <- log10(population_in_millions)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders <- log10(total_gun_murders)

# Create a scatterplot with the log scale transformed population and murders 
plot(log10_population,log10_total_gun_murders)

# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6


# Create a histogram of this variable
hist(population_in_millions)

# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region,data = murders)