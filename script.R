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

# Creating a small dataset to see if our function works like we want
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
# What happens if we have missing data in the data argument we provide to center? 
datNA <- dat
datNA[10, 4] <- NA

center(datNA[ ,4], 0) #NAs......

# We'll revise the function to resolve the NA issue
center <- function(data, midpoint) {
  new_data <- (data - mean(data, na.rm = TRUE)) + midpoint
  return(new_data)
}

center(datNA[, 4], 0) # now works

# What happens if we handed the function a factor or character vector? 
datNA[, 1] <- as.factor(datNA[, 1])
str(datNA[,1])
datNA[, 2] <- as.character(datNA[, 2])
str(datNA[, 2])

center(datNA[, 1], 0) # error: argument is not numeric or logical
center(datNA[, 2], 0)
# Luckily, the errors are informative, but sometimes you need to add in error handling using the warning and stop functions. 

# Documentation
# You can comment your functions but more formal documentation for R functions is written in separate .Rd 
# the roxygen2 package allows R coders to write documentation alongside the function code and then process it into the appropriate .Rd files

# Exercise: Function to create Graphs
# Write a function called analyze that takes a filename as an argument and displays the three graphs produced in the previous lesson

analyze <- function(filename) {
  #inputs the file, creates an object using apply, then plots that object
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

# Now using the function
analyze("r-novice-inflammation/data/inflammation-01.csv")
analyze("r-novice-inflammation/data/inflammation-02.csv")
analyze("r-novice-inflammation/data/inflammation-03.csv")
analyze("r-novice-inflammation/data/inflammation-04.csv")
analyze("r-novice-inflammation/data/inflammation-05.csv") 
analyze("r-novice-inflammation/data/inflammation-11.csv")

# Defining Defaults
# We'll edit the center function to specify the midpoint argument with a default of 0
# This still allows you to pass an argument when you need to, and rely on the default when you don't need to

center <- function(data, midpoint = 0) {
  new_data <- (data - mean(data)) + midpoint
  return(new_data)
}

test_data <- c(0,0,0,0)
center(test_data, 3)

more_data <- 5 + test_data
more_data
center(more_data)

# Example of how the arguments are matched by position or call
display <- function(a = 1, b = 2, c = 3) {
  result <- c(a, b, c)
  names(result) <- c("a", "b", "c")
  return(result)
}

# calling the function without specifying any arguments
display() # provides the names (a,b,c) and 1, 2, 3

display(55) # provides only the first argument; output: 55, 2, 3
display(55, 66) # two arguments; output: 55, 66, 3
display(55, 66, 77)
display(c = 77) # output: 1, 2, 77

?read.csv # this tells us that the function has one argument (file), that doesn't have a default value
# and six others that do have default values

# Episode 3: Analyzing Multiple Data Sets
# How can you do the same thing to multiple data sets? 

# We've created a function (analyze) that creates graphs for a single data set
# And we can use that to analyze other data sets one by one
# But we have dozens of data sets and more on the way so we need to tell the computer how to repeat things

# For Loops
best_practice <- c("Let", "the", "computer", "do", "the", "work")
best_practice

print_words <- function(sentence) {
  for (word in sentence) {
    print(word)
  }
}

print_words(best_practice)
print_words(best_practice[-6]) # everything but the last word

# For Loop Form
for (variable in collection) {
  do thing with variable
}

len <- 0
vowels <- c("a", "e", "i", "o", "u")

for (v in vowels) {
  len <- len + 1
}
len # output is 5
# It’s worth tracing the execution of this little program step by step. 
# Since there are five elements in the vector vowels, the statement inside the loop will be executed five times. 
# The first time around, len is zero (the value assigned to it on line 1) and v is "a". 
# The statement adds 1 to the old value of len, producing 1, and updates len to refer to that new value. 
# The next time around, v is "e" and len is 1, so len is updated to be 2. 
# After three more updates, len is 5; since there is nothing left in the vector vowels for R to process, the loop finishes.

letter <- "z"

for (letter in c("a", "b", "c")) {
  print(letter)
}

letter # now it's "c"

length(vowels) #5
length(letter) #1

# R has a build in function called seq that creates a list of numbers
seq(3)
# Using seq, write a function that prints the first N natural numbers, one per line: 
print_N <- function(N) {
  nseq <- seq(N)
  for (num in nseq) {
    print(num)
  }
}

print_N(10)

# Processing Multiple Files
# We now have almost everything we need to process all our data files. 
# The only thing that’s missing is a function that finds files whose names match a pattern. 
# We do not need to write it ourselves because R already has a function to do this called list.files.

?list.files # Base R, lists the files in a directory/folder
# If we run the function without any arguments, it returns every file in the current working directory
list.files()
# the first argument is the path (default is ., current wd); second argument is the pattern being searched

list.files(path = "r-novice-inflammation/data", pattern = "csv") # finding all the csv files within my data folder

list.files("r-novice-inflammation/data", "inflammation") # pattern match is now for inflammation

# list.files result is a vector of strings, which means we can loop over it to do something with each filename in turn. 
# In our case, the “something” we want is our analyze function.

# Because we have put our data in a separate subdirectory, if we want to access these files using the output of list.files we also need to include the “path” portion of the file name. 
# We can do that by using the argument full.names = TRUE
list.files(path = "r-novice-inflammation/data", pattern = "csv", full.names = TRUE) # this returns the full path name in the output
# instead of "inflammation-04.csv", I get "r-novice-inflammation/data/inflammation-04.csv"

# Analyze function on the first three files in the vector returned by list.files
filenames <- list.files(path = "r-novice-inflammation/data", pattern = "inflammation-[0-9]{2}.csv", full.names = TRUE) # this is using regular expressions! 
filenames <- filenames[1:3]
for (f in filenames) {
  print(f)
  analyze(f)
}

# Write a function called analyze_all that takes a folder path and a filename pattern as its arguments and runs the analyze function for each file whose name matches the pattern
analyze_all <- function(folder = "r-novice-inflammation/data", pattern) {
  filenames <- list.files(path = folder, pattern = "inflammation", full.names = TRUE)
  for (f in filenames) { 
    print(f) # I like adding this because you can see in the console which files are run through the loop
    analyze(f)
    }
} # This only creates the function; remember to also run the function (see below)! 
analyze_all() # Yes! 

# That's the end of Episode 3 (going to do the loop content below before moving on to Episode 4)

# Linked Supplementary Episode: Loops in R
# https://swcarpentry.github.io/r-novice-inflammation/15-supp-loops-in-depth.html

# Vectorized Operations
a <- 1:10
b <- 1:10

res <- numeric(length = length(a))
for (i in seq_along(a)) {
  res[i] <- a[i] + b[i]
} # ugh, what a complicated way to add two vectors
res

res2 <- a + b
res2 # much easier

all.equal(res, res2) #TRUE

# Vector Recycling
# This is important: if you perform an operation on two vectors of unequal length, R will recycle elements of the shorter vector to match the longer vector

a <- 1:10
b <- 1:5
a + b # When R gets to the end of b, it starts again at the beginning of b to add until a is done

# Handy in occasions like this
a <- 1:10
b <- 5 # b is a vector of 1 (the number 5) and it just gets recycled until a is done
a * b

# When the length of the longer object is a multiple of the shorter object length (as in our example above), the recycling occurs silently. When the longer object length is not a multiple of the shorter object length, a warning is given:
a <- 1:10
b <- 1:7
a + b # it does add them (with recycling) 
# but you also receive a warning message: longer object is not a multiple of shorter object length

# for or apply? 
# apply - apply over the margins of an array (rows or columns of a matrix)
# lapply - apply over an object and return list
# sapply - apply over an object and return a simplified object (an array) if possible
# vapply - similar to sapply but you specify the type of object returned by the iterations
# each of these has an argument FUN that takes a function to apply to each element of the object

# Instead of using a loop like earlier, you can use sapply over filesnames with FUN = analyze
sapply(filenames, FUN = analyze) # wow, that's efficient

# Takeaway: personal preference whether to use "for" or one of the apply family functions
# Using an apply function forces you to encapsulate your operations as a function rather than separate calls with for

# Loops in R are Slow
# No, they are not, if you follow these rules: 
# 1. Don't use a loop when a vectorized alternative exists
# 2. Don't grow objects (c, cbind, etc) during the loop
# 3. Allocate an object to hold the results
# Then it provides examples of different loops. I think I prefer the apply family. 
# Final Key Point: Use functions such as apply instead of for loops to operate on the values in a data structure
