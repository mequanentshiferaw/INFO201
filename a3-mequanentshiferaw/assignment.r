# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### DataFrame Manipulation (20 POINTS) ######################

# Create a vector `first_names` with 5 names in it
first_names <- c("Mequnanent", "Mokshe", "Fasil", "Belete", "Mike")

# Create a vector `math_grades` with 5 hypothetical grades (0 - 100)
# in a math course (that correspond to the 5 names above)
math_grades <- c(80, 78, 100, 90, 96)


# Create a vector `spanish_grades` with 5 hypothetical grades (0 - 100)
# in a Spanish course (that correspond to the 5 names above
spanish_grades <- c(40, 60, 80, 93, 85)

# Create a data.frame variable `students` by combining
# the vectors `first_names`, `math_grades`, and `spanish_grades`
students <- data.frame(first_names, math_grades, spanish_grades, stringsAsFactors = FALSE)

# Create a variable `num_students` that contains the
# number of rows in your dataframe `students`
num_students <- nrow(students)

# Create a variable `num_courses` that contains the number of columns
# in your dataframe `students` minus one (b/c of their names)
num_courses <- ncol(students)

# Add a new column `grade_diff` to your dataframe, which is equal to
# `students$math_grades` minus `students$spanish_grades`
students$grade_diff <- students$math_grades - students$spanish_grades

# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
students$better_at_math <- students$math_grades > students$spanish_grades
# Create a variable `num_better_at_math` that is the number
# (i.e., one numeric value) of students better at math
num_better_at_math <- nrow(students[students$better_at_math == TRUE])

# Write your `students` dataframe to a new .csv file inside your data/ directory
# with the filename `grades.csv`. Make sure *not* to write row names.
write.csv(students, "grades.cvs", row.names = FALSE)


########################### Loading R Data (30 points) #########################

# In this section, you'll work with the `Titanic` data set
# Which is built into the R environment. You should be able to `View()` it
# Pay *close attention* to what each column means. Use ?Titanic to read more.
data("Titanic")
View(Titanic)

# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.

is.data.frame(Titanic) # False

# You should convert the `Titanic` variable into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Be sure to **not** treat strings as factors!
Titanic <- data.frame(Titanic, stringsAsFactors = FALSE)


# Create a variable `children` that contains *only* the rows of the data frame
# with information about the children on the Titanic.
children <- Titanic[Titanic$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: remember the `sum()` function!
num_children <- sum(Titanic$Age == "Child")

# Create a variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
# Tip: if you want, you can use multiple statements (lines of code)
# if you find that helpful to create this variable.
most_lost <- Titanic[Titanic$Freq == (max(Titanic$Freq)) & Titanic$Survived == "No", ]
print(most_lost)

# Define a function called `survival_rate()` that takes in a ticket class
# (e.g., "1st", "2nd") as an argument.This function should return the following
# sentence that compares the *total survival rate* of adult men vs.
# "women and children" in that ticketing class. It should read (for example):
# "Of Crew class, 87% of women and children survived and 22% of men survived.".
# The approach you take to generating the sentence to return is up to you.
# A good solution will likely utilize filtering to produce the required data.
total <- sum(Titanic$Freq) # 2201 people
survival_rate <- function(class) {
  women <- subset(Titanic, Titanic$Class == class & Titanic$Sex == "Female")
  adult_men <- subset(Titanic, Titanic$Class == class & Titanic$Sex == "Male")
  children <- subset(Titanic, Titanic$Class == class & Titanic$Sex == "Child")
  # finding the percentage of adult_men, children, and women
  persentage_children <- round(sum(children$Freq) / total * 100)
  persentage_women <- round(sum(women$Freq) / total * 100)
  persentage_men <- round(sum(adult_men$Freq) / total * 100)
  paste("Of Crew class, ", persentage_women + persentage_children, "% of women and children survived and ",
    persentage_men, "% of men survived.",
    sep = ""
  )
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class to your function above.
first_survived <- survival_rate("1st")
second_survived <- survival_rate("2nd")
third_survived <- survival_rate("3rd")
crew_survived <- survival_rate("Crew")
print(crew_survived)

########################### Reading in Data (40 points)#########################
# In this section, we'll read in a .csv file with a tabular row/column layout
# This is like Microsoft Excel or Google Docs, but without the formatting.
# The .csv file we'll be working with has the life expectancy
# for each country in 1960 and 2013. We'll ask real-world questions about the
# data by writing the code that answers our question.
View(life_expectancy)

# Using the `read.csv` function, read the life_expectancy.csv file into
# a variable called `life_expectancy`. Makes sure not to read strings as factors
life_expectancy <- read.csv("data/life_expectancy.csv", stringsAsFactors = FALSE)

# Determine if `life_expectancy` is a data.frame by using
# the is.data.frame function. You may also want to View() it.
is.data.frame(life_expectancy)

# Create a column `life_expectancy$change` that is the change
# in life expectancy from 1960 to 2013
life_expectancy$change <- life_expectancy$le_2013 - life_expectancy$le_1960




# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy
most_improved <- life_expectancy[life_expectancy$change == max(life_expectancy$change), ][1, 1]
# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved fewer than 5 years between 1960 and 2013

num_small_gain <- life_expectancy[life_expectancy$change < 5, ]


# Write a function `country_change()` that takes in a country's name
# as a parameter, and returns it's change in life expectancy from 1960 to 2013
country_change <- function(country) {
  return(life_expectancy[life_expectancy$country == country, 6])
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 2013 in Sweden
sweden_change <- country_change("Sweden")



# Define a function `lowest_life_exp_in_region()` that takes in a **region**
# as an argument, and returns the **name of the country**
# with the lowest life expectancy in 2013 (in that region)
lowest_life_exp_in_region <- function(region) {
  data <- life_expectancy[life_expectancy$region == region, ]
  return(data$country[data$le_2013 == min(data$le_2013)])
}


# Using the function you just wrote, create a variable `lowest_in_south_asia`
# that is the country with the lowest life expectancy in 2013 in South Asia
lowest_in_south_asia <- lowest_life_exp_in_region("South Asia")



# Write a function `bigger_change()` that takes in two country names
# as parameters, and returns a sentence that describes which country experienced
# a larger gain in life expectancy (and by how many years).
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=31.9),
#  whose life expectancy grew by 7.4 years more than Bolivia's (gain=24.5)."
# Make sure to round your numbers to one digit.
bigger_change <- function(first_country, second_country) {
  first <- round(life_expectancy[life_expectancy$country == first_country, 6], digits = 1)
  second <- round(life_expectancy[life_expectancy$country == second_country, 6], digits = 1)
  diff <- first - second
  # sub_diff <- life_expectancy$change = first_country - second_country ||
  # life_expectancy$change = second_country - first_country
  if (diff > 0) {
    paste(
      "The country with the bigger change in life expectancy was ", first_country, "(gain= ", first, "), ",
      "whose life expectancy grew by", diff, "years more than", second_country, "'s (gain=  ", second, ")."
    )
  } else {
    paste(
      "The country with the bigger change in life expectancy was ", second_country, "(gain= ", second, "), ",
      "whose life expectancy grew by", -1 * diff, "years more than", first_country, "'s (gain=  ", first, ")."
    )
  }
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- bigger_change("United States", "France")

# Write your `life_expectancy` data.frame to a new .csv file to your
# data/ directory with the filename `life_expectancy_with_change.csv`.
# Make sure not to write row names.
write.csv(life_expectancy, file = "life_expectancy_with_change.csv", row.names = FALSE)

############################## Challenge (10 points) ###########################
# Create a variable `highest_avg_change` that has the name of the region with
# the highest *average change* in life expectancy between the two time points.
# To do this, you'll need to *compute the average* change across the countries
# in each region, and then compare the averages across regions.
# Feel free to use any library of your choice, or base R functions.
highest_avg_change <- function() {
  reg_list <- unique(sort(life_expectancy$region))
  names(sort(sapply(reg_list, function(x) mean
    (life_expectancy_with_change[life_expectancy_with_change$region == x, ]$change)),
  decreasing = TRUE
  ))[1]
}
# Create a *well labeled* plot (readable title, x-axis, y-axis) showing
# Life expectancy in 1960 v.s. Change in life expectancy
# Programmatically save (i.e., with code, not using the Export button)
# your graph as a .png file in your repo
# Then, in a comment below, *provide an interpretation* of the relationship
# you observe. Feel free to use any library of your choice, or base R functions.
plot(life_expectancy$le_1960, life_expectancy_with_change$change,
  xlab = "life Expectancy in 1960", ylab = "life Expectancy with change", main = " Life Expectancy "
)
# Put your interpretation here!
# some of the point are plot togeter
#  the polt looks like it has a negative slope
# most of the point are in the 50s and up on the x-axis
