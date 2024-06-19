# Software Carpentry - Programming with R
# https://swcarpentry.github.io/r-novice-inflammation/instructor/index.html

dir.create("r-novice-inflammation")

# Episode 1: Analyzing Patient Data
# Studying inflammation in patients; each row is a patient, each column is a daily inflammation measure

read.csv("r-novice-inflammation/data/inflammation-01.csv", header = FALSE) # there are no headers so it's important to do header = F or else it will create headers from your first row of data and append an X (in this case) to the number 

weight_kg <- 55
2.2 * weight_kg
weight_kg <- 57.5
weight_lb <- 2.2 * weight_kg
weight_lb

dat <- read.csv("r-novice-inflammation/data/inflammation-01.csv", header = FALSE)

head(dat)

class(dat) # data.frame

dim(dat) # 60,40

dat[1,1] # indexing with square brackets, first number is row, and the second number is the column
dat[30,20]

dat[c(1, 3, 5), c(10, 20)] # c will combine the values you give it into one vector or list; 
# this is items from rows 1, 3 and 5, and columns 10 and 20

1:5 # generates the sequence
3:12

dat[c(1:4), c(1:10)] # actually you don't need the c(), you can just do: 
dat[1:4, 1:10] 

dat[5, ]# to select all rows or columns, just leave that empty; all columns from row 5
dat[, 16:18] # columns 16 - 18, all rows
dat[ , ] # you'll get the entire dataframe

# max inflammation for patient 1
patient_1 <- dat[1, ]
max(patient_1)
#instead of doing that...you can do: 
max(dat[1, ])

# minimum inflammation on day 7
min(dat[, 7])
# mean inflammation on day 7
mean(dat[, 7])
# median inflammation on day 7
median(dat[, 7])
# sd of inflammation on day 7
sd(dat[, 7])

# Forcing conversion box - rows vs. columns
mean(dat[1, ]) # returns NA
mean(as.numeric(dat[1, ])) # works

summary(dat[, 1:4]) # That provides summary stats for columns 1-4


?apply # allows you to repeat a function on all the rows (MARGIN = 1) or columns (MARGIN = 2) of a dataframe
# the second argument in apply is MARGIN...1 indicates rows, 2 indicates columns, c(1,2) indicates rows and columns

# average inflammation of each patient (across rows)
avg_patient_inflammation <- apply(dat, 1, mean)
# average inflammation of each day (down columns)
avg_day_inflammation <- apply(dat, 2, mean)

# An alternative solution is rowMeans and colMeans
rowMeans(dat)
colMeans(dat)

# Subsetting character vectors
animal <- c("m", "o", "n", "k", "e", "y")
animal[1:3]
animal[4:6]
animal[-1] # removes "m"
animal[-4] # removes "k"

# Challenges: 
# Calculate the mean inflammation for patients 1 to 5 over the entire 40 days
apply(dat[1:5, ], 1, mean)
# Calculate the mean inflammation for days 1 to 10, across patients
apply(dat[, 1:10], 2, mean)
# Calculate the mean inflammation for every second day, across patients
apply(dat[, seq(2, 40, by = 2)], 2, mean)

# Plotting
plot(avg_day_inflammation) # y axis is the average inflammation level; x axis is the order, or index, of the values in the vector, which in this case corresponds to the 40 days

max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)

min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)
# These plots indicate unlikely results so either mistake in our calculations or something is wonky with the data
# SK: what's the point of suggesting that the data is wrong and then then moving on to the next lesson???


# Episode 2: Creating Functions

fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}
# the assignment operator provides the function name
# the list of arguments are contained within parentheses
# the body of the function (what's executed) is contained within curly braces
# the return statement defines what will be returned by the function; note: in R, it's not necessary to include a return statement

# Calling our function is no different than calling any other function
fahrenheit_to_celsius(32) # freezing point of water
fahrenheit_to_celsius(212) # boiling point of water

# Composing Functions -- this section seems a little unnecessary at this stage
celsius_to_kelvin <- function(temp_C) {
  temp_K <- temp_C + 273.15
  return(temp_K)
}
# Freezing point of water in Kelvin
celsius_to_kelvin(0)

fahrenheit_to_kelvin <- function(temp_F) {
  temp_C <- fahrenheit_to_celsius(temp_F)
  temp_K <- celsius_to_kelvin(temp_C)
  return(temp_K)
}
fahrenheit_to_kelvin(32) # freezing point of water in Kelvin

# can also nest these functions
celsius_to_kelvin(fahrenheit_to_celsius(32))

# Activities (Create a Function, Named Variables and Scope of Variables) - Skip

# Testing, Error Handling, and Documenting
# Once we start putting things in functions, we need to start testing that those functions are working correctly

center <- function(data, midpoint) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

# Creating a small dataset to see if our funciton works like we want
z <- c(0, 0, 0, 0)
z

center(z, 3) # yup, looks good

# Now we're going to use our function on the inflammation data to center day 4 around zero
centered <- center(dat[, 4], 0)
head(centered)

# confirming that it matches the original data
mean(dat[, 4]) # original mean, 1.75
mean(centered) # 0 

sd(dat[, 4]) # 1.06
sd(centered) # 1.06

# difference in sd before and after 
sd(dat[, 4]) - sd(centered) # 0

# Sometimes a small difference can be detected due to rounding
# R has a function for comparing two objects allowing for rounding errors: all.equal()
all.equal(sd(dat[, 4]), sd(centered)) # TRUE

# Error Handling
