## Hands-on Data Science with R
# Chapter 3
set.seed(100)

data(state)

state <- data.frame(state.x77) # Creating a data.frame from the matrix state.x77
head(state)

# View First 3 rows
state[1:3,]

# View First 3 columns
state[,1:3]

# View First 3 rows and 3 columns
state[1:3,1:3]

# Creating a new column
state$State <- row.names(state)

# Finding matches using a boolean operation
state[state$State == "Connecticut",] # Find information on Connecticut
state[state$Population > 1000 & state$Income > 2000,] # Find states with Population > 1000 and Income > 2000

# Saving the state data.frame as a CSV File
write.csv(x=state,file = "state.csv",row.names = F)

# The arguments were as follows:
# x = the name of the data.frame we want to save; file = the file we want to save as; row.names=F means do not include the row names

# We can read the file back to see what it contains

read.csv("state.csv")

# Note the difference - the original state data.frame contained row names whereas the saved CSV file doesn't because of the options we had selected
# Both write.csv and read.csv takes several other options that you can view by running ?write.csv and ?read.csv in the R console respectively


# View the structure of the state data.frame

str(state)

# Apply Family of Functions
# apply

# Usage
# 
# apply(X, MARGIN, FUN, ...)
# Arguments
# 
# X: an array, including a matrix.
# MARGIN: a vector giving the subscripts which the function will be applied over. E.g., for a matrix 1 indicates 
# rows, 2 indicates columns, c(1, 2) indicates rows and columns. Where X has named dimnames, it can be a 
# character vector selecting dimension names.

apply(state[,-ncol(state)], 2, sum) # Sum of all values in the numeric columns
apply(state[,-ncol(state)], 2, mean) # Mean of all values in the numeric columns


## lapply - list apply which is similar to apply, but can be also used for other R object types
## Produces the output as a list
lapply(state[,-ncol(state)], function(x) {list(MIN=min(x), MAX=max(x), MEAN=mean(x))})

# $Population
# $Population$MIN
# [1] 365
# 
# $Population$MAX
# [1] 21198
# 
# $Population$MEAN
# [1] 4246.42


## sapply - simplifies the output (eg., produce a vector instead of a list)
sapply(state[,-ncol(state)], function(x) {list(MIN=min(x), MAX=max(x), MEAN=mean(x))})


# Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area    
# MIN  365        3098   0.5        67.96    1.4    37.8    0      1049    
# MAX  21198      6315   2.8        73.6     15.1   67.3    188    566432  
# MEAN 4246.42    4435.8 1.17       70.8786  7.378  53.108  104.46 70735.88

# Aggregation Functions

## aggregate
## If say, we wanted to find the aggregate values of each "Region" of the US
## We can use aggregate to find the cumulative values on a per region basis
## To find the min, max and mean of each column aggregated by region, we will first
## Add the region value to our data frame

state$Region <- state.region
ncol(state)
aggregate(state[,-c(9,10)], by=list(state$Region), mean, na.rm = T)

# Group.1 Population   Income Illiteracy Life.Exp    Murder  HS.Grad    Frost      Area
# 1     Northeast   5495.111 4570.222   1.000000 71.26444  4.722222 53.96667 132.7778  18141.00
# 2         South   4208.125 4011.938   1.737500 69.70625 10.581250 44.34375  64.6250  54605.12
# 3 North Central   4803.000 4611.083   0.700000 71.76667  5.275000 54.51667 138.8333  62652.00
# 4          West   2915.308 4702.615   1.023077 71.23462  7.215385 62.00000 102.1538 134463.00

## --- Merging Data Frames in R ---
##
## R provides a built-in command, merge in order to join data frames

# Let us create a separate data frame with the geographic location of the center of each state (latitude and longitude)
state2 <- data.frame(State=state.name, Latitude=state.center$y, Longitude=state.center$x)

# The syntax for merge is as follows:

## S3 method for class 'data.frame'
# merge(x, y, by = intersect(names(x), names(y)),
#       by.x = by, by.y = by, all = FALSE, all.x = all, all.y = all,
#       sort = TRUE, suffixes = c(".x",".y"),
#       incomparables = NULL, ...)
# Arguments
# 

# x and y = the data frames we want to merge
# by = the column names by which we want to perform the merge
# Note that since this can be different (if say the same column had different names in different data frames)
# we can use by.x and by.y to specify the corresponding name of the column across the 2 data frames

# all = whether to keep rows that did not match in either the x or y data frame
# Using all=T means keep all rows of both the data frames even if there was no match

merged <- merge(state,state2,by="State",all=T)
merged

# New rows and columns can be added using the rbind and cbind functions
# For eg., if say we create 2 data.frames using a subset of the state dataframe, we can combine them as follows:

# Using cbind to combine 2 separate data.frame of 2 columns each
state0 <- state[,c(1:2)]
state1 <- state[,c(3:4)]

dim(state0)
dim(state1)

state01 <- cbind(state0,state1)
state01

# Population Income Illiteracy Life.Exp
# Alabama              3615   3624        2.1    69.05
# Alaska                365   6315        1.5    69.31
# Arizona              2212   4530        1.8    70.55
# Arkansas             2110   3378        1.9    70.66

# Using rbind to combine 2 separate data.frame of 2 rows each
state0 <- state[c(1:2),]
state1 <- state[c(3:4),]

dim(state0)
dim(state1)

state01 <- rbind(state0,state1)
state01

# Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area    State
# Alabama        3615   3624        2.1    69.05   15.1    41.3    20  50708  Alabama
# Alaska          365   6315        1.5    69.31   11.3    66.7   152 566432   Alaska


## Using tibble and dplyr

# install.packages("tidyverse")
library("tidyverse")

tstate <- as_tibble(state)

# Notice the difference between the output of data.frame. The tibble display contains descriptive information about the variable types as well as the total number of columns and rows.
# You can use the same syntax with tibble as you do for data.frame

# To view the rows and columns, use tibble name [rows,columns]
# For instance, to view the first 2 rows of tstate
tstate[1:2,]

# To view the first 2 columns of tstate
tstate[,1:2] # Or tstate[1:2]

## Note that tibbles when subset using [] always return a data.frame. The same may not always be true for data.frame based subsetting as it can return a vector (which has to be then converted to a data.frame)


# To get a quick view of the table, use glimpse
glimpse(tstate)

# You can also create a tibble on-the-fly, using tribble (transposed table)
mytibble <- tribble(~state, ~capital,
                        "CT", "Hartford",
                        "NY", "Albany")

# To add a row to a tribble, use add_row as follows
mytibble <- mytibble %>% add_row(state="NJ",capital="Trenton")

# View the contents of mytibble to see that a new row for NJ, Trenton has been added
mytibble

# You can similarly, add a column using add_column

# You can convert rownames to columns using rownames_to_column as follows
# Note that a tibble is also a data.frame as seen by running the following command

class(tstate)

# However, it is also possible to convert a tibble to a data.frame using the as.data.frame command from base R
# For eg.,
as.data.frame(tstate)
class(as.data.frame(tstate))

# Note that using as.data.frame or as_data_frame can be quite slow and tibble should be preferred unless it isn't supported with older functions that may strictly require a data.frame

## Beginning dplyr
tstate <- as_tibble(state.x77)
tstate$Region <- state.region

# Using pipes
# One way to find the region with the maximum income would be to aggregate by region and then find the region corresponding to the highest value

step1 <- aggregate(tstate[,-c(9)], by=list(state$Region), mean, na.rm = T)
step1

# Group.1 Population   Income Illiteracy Life Exp    Murder  HS Grad    Frost      Area
# 1     Northeast   5495.111 4570.222   1.000000 71.26444  4.722222 53.96667 132.7778  18141.00
# 2         South   4208.125 4011.938   1.737500 69.70625 10.581250 44.34375  64.6250  54605.12
# 3 North Central   4803.000 4611.083   0.700000 71.76667  5.275000 54.51667 138.8333  62652.00
# 4          West   2915.308 4702.615   1.023077 71.23462  7.215385 62.00000 102.1538 134463.00

step2 <- step1[step1$Income==max(step1$Income),]
step2
# Group.1 Population   Income Illiteracy Life Exp   Murder HS Grad    Frost   Area
# 4    West   2915.308 4702.615   1.023077 71.23462 7.215385      62 102.1538 134463

# This can be greatly simplified using the %>% pipe operator as follows:

tstate %>% group_by(Region) %>% summarise(Income = mean(Income)) %>% filter(Income == max(Income))

# # A tibble: 1 x 2
# Region   Income
# <fctr>    <dbl>
#   1   West 4702.615

# It is also possible to summarise all the column values at once using summarise_all and find the row corresponding to max income as in the prior example
tstate %>% group_by(Region) %>% summarise_all(funs(mean)) %>% filter(Income == max(Income))

# # A tibble: 1 x 9
# Region Population   Income Illiteracy `Life Exp`   Murder `HS Grad`    Frost   Area
# <fctr>      <dbl>    <dbl>      <dbl>      <dbl>    <dbl>     <dbl>    <dbl>  <dbl>
#   1   West   2915.308 4702.615   1.023077   71.23462 7.215385        62 102.1538 134463

# dplyr Verbs
# select

# Add the state name to the tstate dataset
tstate$Name   <- state.name

select(tstate, Income, Frost, Area) # selecting specific columns

# # A tibble: 50 x 3
# Income Frost   Area
# <dbl> <dbl>  <dbl>
#   1   3624    20  50708
# 2   6315   152 566432

select(tstate, Population:Illiteracy) # selecting a range of columns

# # A tibble: 50 x 3
# Population Income Illiteracy
# <dbl>  <dbl>      <dbl>
#   1       3615   3624        2.1
# 2        365   6315        1.5
# 3       2212   4530        1.8

select(tstate, -c(Population:Illiteracy)) # excluding a range of columns

# # A tibble: 50 x 7
# `Life Exp` Murder `HS Grad` Frost   Area    Region        Name
# <dbl>  <dbl>     <dbl> <dbl>  <dbl>    <fctr>       <chr>
#   1      69.05   15.1      41.3    20  50708     South     Alabama
# 2      69.31   11.3      66.7   152 566432      West      Alaska
# 3      70.55    7.8      58.1    15 113417      West     Arizona

rename(tstate, Pop=Population) # renaming specific columns and selecting all columns

# # A tibble: 50 x 10
# Pop Income Illiteracy `Life Exp` Murder `HS Grad` Frost   Area    Region        Name
# <dbl>  <dbl>      <dbl>      <dbl>  <dbl>     <dbl> <dbl>  <dbl>    <fctr>       <chr>
#   1  3615   3624        2.1      69.05   15.1      41.3    20  50708     South     Alabama
# 2   365   6315        1.5      69.31   11.3      66.7   152 566432      West      Alaska
#

# You can also use helper functions such as starts_with, ends_with and others to select only specific columns matching a criteria
select(tstate, starts_with("P"))

# # A tibble: 50 x 1
# Population
# <dbl>
#   1       3615
# 

select(tstate, ends_with("n"))

# # A tibble: 50 x 2
# Population    Region
# <dbl>    <fctr>
#   1       3615     South
# 2        365      West
#

## Filtering with filter
## The verb filter can be used to extract a subset of rows matching the filter criteria

# Filter states with < 1% Illiteracy (i.e., > 99% literacy)
filter(tstate, Illiteracy < 1) # Equivalently -> filter(tstate, (100 - Illiteracy) > 99)

# Filter states with < 1% Illiteracy and Income > the mean Income of all states
# We will apply the AND condition using &

filter(tstate, Illiteracy < 1 & Income > mean(Income))

# This is the same as using , (comma), multiple parameters are treated as AND

identical(filter(tstate, Illiteracy < 1 & Income > mean(Income)),filter(tstate, Illiteracy < 1, Income > mean(Income)))
# [1] TRUE

# Filter states with Income > the mean Income of all states OR HS Graduation Rate > 60%
# We will apply the OR condition using |

filter(tstate, Income > mean(Income) | `HS Grad` > 60)

# Filter for states in the West Region and the above condition (Income > the mean Income of all states OR HS Graduation Rate > 60%)

filter(tstate, (Income > mean(Income) | `HS Grad` > 60) & Region=="West")

# Other related verbs include filter_all, filter_if and filter_at
# An example for each is given below

# Print names of all numeric column
filter_all(tstate, all_vars(class(.)=="numeric"))


# Filter if ALL row values > 1 using all_vars
select_if(tstate, is.numeric) %>% filter_all(all_vars(. > 1)) # When all vars > 1

# Filter if ANY row values > 4000 using any_vars
select_if(tstate, is.numeric) %>% filter_all(any_vars(. > 4000)) # When any vars > 4000

# There are various other ways that filter can be used and more details can be found at online resources for the same.


## Using arrange
# arrange is used to sort datasets as shown below

# Sort the dataset in descending order of Income (high to low)
arrange(tstate, desc(Income))

# Sort by Region and Illiteracy (within Region)
arrange(tstate, Region, desc(Income))

## Mutate
# The verb mutate is used to add new columns to a dataset, most commonly to represent a calculated value

# For instance to find population on a per square mile basis:
mutate(tstate, pop_per_sq_mile = Population/Area)

# If you want to keep only the newly created variable, use transmute

transmute(tstate, pop_per_sq_mile = Population/Area)
# # A tibble: 50 x 1
# pop_per_sq_mile
# <dbl>
#   1    0.0712905261
# 2    0.0006443845
# 3    0.0195032491

## Summarise
# summarise is used to obtain aggregate values, generally over a grouped variable

# The following highlights some of the common operations using summarise
# Generally, summarise is preceded by a group_by operation, i.e., the summary is performed over group-ed variable(s)

# In the example below:
# 1) We sorted the data frame by State Name using arrange
# 2) We applied a group-by using Region, i.e., all resulting values would be aggregated using Region
# 3) We calcuated the values for total rows using n(), the unique states belonging to each region using n_distinct
#   the max & mean literacy using max and mean respectively
# 

tstate %>% arrange(Name) %>% group_by(Region) %>% 
  summarise(total_rows = n(), first_state = first(Name), 
            unique_states = n_distinct(Name), max_literacy = max(100-Illiteracy), 
            mean_literacy = mean(100-Illiteracy, na.rm=T))


## Sampling with sample_n and sample_frac
# Both sample_n and sample_frac allows the user to select random samples (by count or fraction respectively) from the given dataset
# It is primarily a wrapper to gather random samples using sample.int


sample_n(tstate, 10) # To select 10 random rows

# # A tibble: 10 x 10
# Population Income Illiteracy `Life Exp` Murder `HS Grad` Frost   Area        Region        Name
# <dbl>  <dbl>      <dbl>      <dbl>  <dbl>     <dbl> <dbl>  <dbl>        <fctr>       <chr>
#   1       2280   4669        0.6      72.58    4.5      59.9   114  81787 North Central      Kansas
# 2      11197   5107        0.9      70.14   10.3      52.6   127  55748 North Central    Illinois
# 3       1544   4508        0.6      72.60    2.9      59.3   139  76483 North Central    Nebraska

sample_frac(tstate, 0.10) # To select 10% of the rows at random, i.e., 5 rows out of 50

## tidyr
## The last package that deserves mention here is tidyr
## The package provides a few helpful functions to convert wide tables to long and vice-versa. Consequently, it can be used effectively for creating long tabbles and pivoting values based on column data

## Gather allows the creation of key-value pairs by flattenting the columns into a key column with the corresponding measurements in a value column.
## Note that columns can be prevented from being flattemned by simply passing a - sign before the name of the column

# In order to keep only the Region and Name column and convert everything else into a key-value pair, run
gathered <- gather(tstate, "Keys", "Values", -Region, -Name) %>% arrange(Region) # Note taht arrange has been used only for aesthetic purposes
gathered
# # A tibble: 400 x 4
# Region          Name       Keys Values
# <fctr>         <chr>      <chr>  <dbl>
#   1 Northeast   Connecticut Population   3100
# 2 Northeast         Maine Population   1058
# 3 Northeast Massachusetts Population   5814
# 4 Northeast New Hampshire Population    812
# 5 Northeast    New Jersey Population   7333

## spread
## The opposite of gather() is spread() which converts a long table into a wide table by converting the values in the key column into column headers

spread_df <- spread(gathered, "Keys", "Values")
spread_df # Note that this in essence restores the original look alignment of the table

# # A tibble: 50 x 10
# Region          Name  Area Frost `HS Grad` Illiteracy Income `Life Exp` Murder Population
# <fctr>         <chr> <dbl> <dbl>     <dbl>      <dbl>  <dbl>      <dbl>  <dbl>      <dbl>
#   1 Northeast   Connecticut  4862   139      56.0        1.1   5348      72.48    3.1       3100
# 2 Northeast         Maine 30920   161      54.7        0.7   3694      70.39    2.7       1058
# 3 Northeast Massachusetts  7826   103      58.5        1.1   4755      71.83    3.3       5814

## unite
## The unite function is used to merge two or more variables
unite(tstate, "NewColumn", "Region","Name") %>% select(NewColumn)

# # A tibble: 50 x 1
# NewColumn
# <chr>
#   1         South_Alabama
# 2           West_Alaska
# 3          West_Arizona
# 4        South_Arkansas

## Joining tables
# Finally, there are a few helpful functions to merge tibbles
# Various types of joins are supported - for eg., inner_join, left_join, right_join and others
# The main difference among these is related to the which rows are kept from two tibbles in the even there is no match
# An example of a left_join has been provided here for reference. More exhaustive treatment of these join types is avaiable online

astate <- tibble(Name=state.name, Abbr=state.abb)
astate

tstate %>% left_join(astate, by="Name")
# A tibble: 50 x 11
# Population Income Illiteracy `Life Exp` Murder `HS Grad` Frost   Area    Region        Name  Abbr
# <dbl>  <dbl>      <dbl>      <dbl>  <dbl>     <dbl> <dbl>  <dbl>    <fctr>       <chr> <chr>
#   1       3615   3624        2.1      69.05   15.1      41.3    20  50708     South     Alabama    AL
# 2        365   6315        1.5      69.31   11.3      66.7   152 566432      West      Alaska    AK
# 3       2212   4530        1.8      70.55    7.8      58.1    15 113417      West     Arizona    AZ
# 4       2110   3378        1.9      70.66   10.1      39.9    65  51945     South    Arkansas    AR

# Note that the Abbr column has been added on the right ... .

## Introduction to data.table

library(data.table)


dstate <- data.table(state.x77,State=row.names(state.x77))
# Note that State has been added as data.table's do not have row names (uses row indices instead)

dstate


# The general form of data.table operations is as follows:

#   dt[i, j, by]
#   where 
#    •	dt is the name of the data.table;
#    •	i is the condition/rows by which the data.table is being subset;
#    •	j represents the calculations/columns to be produced; and
#    •	by represents the group-by aggregates

# An interesting aspect of data.table is that operations can occur "by reference" without the need for copying the data. In base R, operations such as renaming columns, etc may require copying the entire data.frame. By avoiding such steps and along with several other optimisations, data.table provides an immense improvement in performance over most of the other data manipulation solutions in R today.

# To select the first 3 rows
dstate[3:5]

# To select the rows where Income > 5000
dstate[Income > 5000]

# To select the rows where Income > 5000 and `HS Grad` > 60
dstate[Income > 5000 & `HS Grad` > 60]

# To select the columns Population, Income, Frost and State where Income > 5000
dstate[Income > 5000, list(Population, Income, Frost, State)]

# Note that we can also use the . (dot) notation in order to return the results as a data.table
dstate[Income > 5000, .(Population, Income, Frost, State)]

## As stated before, the j value can be also used in order to perform calculations. For eg., if instead of just returning the individual values for Population, Income and Frost, we wanted to get the mean of each, we can instead use the following:

dstate[Income > 5000, .(Mean_Pop=mean(Population), Mean_Inc=mean(Income), Mean_Frost=mean(Frost))]

## The notation .N can be used in order to get counts as follows:

dstate[Income > 5000, .(Count=.N, Mean_Pop=mean(Population), Mean_Inc=mean(Income), Mean_Frost=mean(Frost))]

## Note that this manner of making selections is different from that in data.frame where variables are quoted. In data.table, we can use the variable names as is.
## There is a way by which we can also refer to the columns/subset it using the data.frame's [row,column] method by using "with"
## For eg., to select the columns, Population, Income and State where Income > 5000 using the data.frame method, we can use the following:

dstate[Income > 5000, c("Population","Income","State"), with=F]

## We can also use : (colon) to select a range of columns. For instace, to select the first 3 rows of all the columns from  HS Grad to State, we can use,

dstate[1:3,`HS Grad`:State]

## Grouping Operations
## In data.table, we can perform grouping operations using the by notation. Let us first add a new column with the Region names to the data.table

## Adding a column
# We can do so using the := notation. This is a notation that is already available in R, but generally not used. It allows the update to happen "in-place". In other words, it avoids making a copy of the dataset in order to add a new column.
dstate[,Region:=state.region]
dstate[1:3] # We can see that the Region column has been added

# We can use := to add multiple columns
## For instance to add the Division and Abbrevation of each state, we can use the following:

dstate[,c("Division","Abb"):=.(state.division, state.abb)]
dstate[1:3] # We can see that the new columns, Division and Abb have been added

## To find the sum of Population grouped by Region, we can use the following:

dstate[,.(Sum_Pop=sum(Population)),by=Region]

## To find the sum of Population grouped by Region and Division, we can use the following:

dstate[,.(Sum_Pop=sum(Population)),by=.(Region,Division)]

## Notice that we always use the . notation when performing such operations. A data.table is in essence a "list of lists", i.e., each column is a list. Hence, in order to group by a column, instead of using the c() notation (which represents vectors), we use . or list()

## If we had to perform an operation across multiple columns, we can use the inbuilt .SD symbol (which stands for Subset of Data). For instance to find the first row corresponding to each region, we can use .SD with head() as follows:

dstate[,head(.SD,1), by=.(Region)]

## We can also modify the .SD symbol to operate on only a fixed set of columns. For eg., to find the maximum of Population and Income grouped by Region using .SD, we can use it in conjunction with lapply() to run the max() function for each column specified in .SD

dstate[,lapply(.SD,max), by=.(Region), .SDcols=c("Population","Income")]

# To perform the same operation as shown above and also add the minimum per Region, we can use:

dstate[,c(lapply(.SD, max), lapply(.SD, min)), by=.(Region), .SDcols=c("Population","Income")]


## Ordering Columns

# We can order the rows by using order() as the j value in the data.table as follows:

dstate[order(Region)]

# We can order using multiple columns by just adding them to order()

dstate[order(Region, State)] # Ascending Region, Ascending State
dstate[order(Region,-State)] # Ascending Region, Descending State

## Joining tables using data.table

# data.table includes a very useful function called setkey() and setkeyv(). There are other set* functions which, when used along with := modifies the object by reference. It is also extremely useful when merging tables as discussed below.

## To query the table by Region, we can set Region as the "key" and query by simply using the valye of the Region

setkey(dstate,Region)

key(dstate) # Check if the table is keyed

# [1] "Region"


dstate[.("West")] # View all entries with Region "West"

# We can also set multiple keys. For instance to set both Region and Division as a key:

setkey(dstate,Region,Division)

key(dstate)
dstate[.("West","Mountain")] # View all entries with Region "West" and Division "Mountain"

# Notice that the keys are being passed as the i value in the corresponding data.table notation as we are subsetting the table. We can use this in conjunction with the j value to return only specific columns

dstate[.("West","Mountain"),.(Mean=mean(Area))] # Find the Mean Area for Region "West" and Division "Mountain"

dstate[.("West"),.(Mean=mean(Area)), by = Division] # Find the Mean Area for Region "West" group-ed by Division

# A couple of addition arguments, mult and nomatch allows the user to specify the behaviour if multiple matches or no matches are found

dstate[.("West","Mountain"),mult="first"]

dstate[.("West",c("Mountain","InvalidEntry")),nomatch=0L] # Skips no matches

# What is the advantage of searching using key by?
# The primary advantage of using keys is that the sorting is based on a binary search. This means that instead of scanning the entire column, it is possible to complete the search in "log-time" which can be extremely powerful in practice. It is also very memory efficient, thus improving the overall performance of the query from a systems/resource needs standpoint.

## Creating new columns in data.table
## New columns can be created in data.table using the := notation.

dstate[,IncomeGreaterThan5000:=Income>5000] # We will add a column IncomeGreaterThan5000 in-place using :=

## Adding multiple columns
dstate[,c("IncomeGreaterThan5000","AreaLessThan5000"):=list(Income>5000,Area<5000)] # We will add a column IncomeGreaterThan5000 in-place using :=

# alternatively, we can also use `:=`

dstate[,`:=`(IncomeGreaterThan5000=Income>5000,AreaLessThan5000=Area<5000)] # We will add a column IncomeGreaterThan5000 in-place using :=

## Deleting a column
## Columns can likewise be deleted using :=NULL as shown below

dstate[,IncomeGreaterThan5000:=NULL]

## We can delete multiple columns as well
dstate[,c("AreaLessThan5000","Abb","Division"):=NULL]

## Pivots on data.table
## Pivot operations on data.table-s can be performed using the melt and cast functions

## The melt functionality
# The data.table::melt function is used to conver a wide table to a long table, i.e., the columns are collapsed into a key-value relationship

# If say, we wanted to conver the dstate table into a long table consisting of a column with State & Region and Income & Area as the values, we could use,

dmelted <- data.table::melt(dstate, id.vars=c("State","Region"), measure.vars=c("Income","Area"))
dmelted
# An easy way to remember this is that id.vars are the columns that will remain constant, in this case, State and Region and measure.vars indicate the columns that will be collapsed as shown below:

#             State        Region variable  value
# 1:    Connecticut     Northeast   Income   5348
# 2:          Maine     Northeast   Income   3694
# 3:  Massachusetts     Northeast   Income   4755
# 4:  New Hampshire     Northeast   Income   4281
# 5:   Rhode Island     Northeast   Income   4558
# 6:        Vermont     Northeast   Income   3907
# 7:     New Jersey     Northeast   Income   5237


## We can convert this back into a "wide" table, simply by using data.table's dcast functionality as follows:

data.table::dcast(dmelted, State + Region ~ variable, value.var="value")
# Read literally, this means, keeping the State and Region columns constant, un-collapse the column named "variable" with values corresponding to the column "value"

## Reading and writing files

# For reading and writing files, for eg., csv files, there is no other functiomn as fast and efficient as fread and fwrite in R as of today. Let us first see how to read and write csv files.

# fwrite: Writing csv files
# Check the help file for fwrite as it includes numerous options that can be passed to the fwrite function.

?fwrite

# The simples way to use fwrite is to pass the name of the data.table or data.frame as the first argument followed by the name of the file you'd like to create, for eg.,
fwrite(dstate, file="dstate.csv") # Save data.table dstate into the csv file dstate.csv

# Note that we can change the separator using the sep argument (eg., sep="|)

# To read back the file, simply use fread(filename)
fread("dstate.csv")

# Similar to fwrite, the function fread can accept several other helpful arguments as can be seen from ?fread

# A special note on reading/writing data with dates and/or times
# Parsing date-time values in any platform can be a tedious operation, not least because of the various formats in which dates and times can be written.
# The package, lubridate, makes it easy for anyone to work with dates and times in R. In addition, data.table also includes several helpful functions in order to work with date/time values.

library(lubridate)

# We can create a sample table with dates and times as follows

tdate <- data.frame(dt=(as.POSIXct("2010-03-18 19:08:10 EDT")) + 1000000 * runif(1000,-100,100), value=sample(LETTERS,1000,T))

head(tdate)

str(tdate)
# Note that we the column dt is already defined as of class POSIXct. However, in general, when reading date times from files, they may instead get read in as strings. We'll create a separate column, called dts where the column dt will be cast to string.

tdate$dts <- as.character(tdate$dt)
str(tdate)

# The package lubridate provides an easy method to translate the character column into dates/times. For instance,

ymd_hms(tdate$dts[1:3]) # The character values in dts are read in as dates/times
# [1] "2012-02-25 01:15:02 UTC" "2007-03-01 15:18:32 UTC" "2011-09-19 13:34:47 UTC"

class(tdate$dts[1:5])
# [1] "character"

class(ymd_hms(tdate$dts[1:5]))
# [1] "POSIXct" "POSIXt" 

# In general, it is possible to interpret any string as a date/time using the ymd and hms notations.
# For example:

ymd(20190420) # Change 20190420 into year 2014, month 04 and date 20
mdy(04202019)
dym(20201904)
dmy(20042020)

# We can verify whether they are equivalent using identical
identical(ymd(20190420),mdy(0420201))

identical(ymd(20190420),dym(20201904))

# Once the data type is interpreted as date/time, we can then use other operations, such as extracting only parts of the date/time values as shown:

tdate$dts[1:3]
# [1] "2012-02-25 01:15:02" "2007-03-01 15:18:32" "2011-09-19 13:34:47"

ymd_hms(tdate$dts[1:3])
# [1] "2012-02-25 01:15:02 UTC" "2007-03-01 15:18:32 UTC" "2011-09-19 13:34:47 UTC"

year(ymd_hms(tdate$dts[1:3]))
# [1] 2012 2007 2011

month(ymd_hms(tdate$dts[1:3]))
# [1] 2 3 9

day((ymd_hms(tdate$dts[1:3])))
# [1] 25  1 19

yday((ymd_hms(tdate$dts[1:3]))) #nth day of the year y
# [1]  56  60 262

wday((ymd_hms(tdate$dts[1:3]))) # Weekday
# [1] 7 5 2

wday((ymd_hms(tdate$dts[1:3])), label=T) # Weekday
# [1] Sat Thu Mon

ymd(20190420) + days(10)
# [1] "2019-04-30"

ymd(20190420) + months(2)
# [1] "2019-06-20"

ymd(20190420) + years(2)
# [1] "2021-04-20"

# Lubridate also includes functions to perform simple date/time calculations such as,

my_birthday <- ymd("20010101")

# The standard difftime object (obtained when subtracting dates/times) can bbe a bbit difficult to interpret
today() - my_birthday
# Time difference of 6299 days

class(today() - my_birthday)
# [1] "difftime"

# Lubridate includes useful functions named d* such as dseconds, ddays, dhours and so on that makes it easier to interpret difftime values, for eg.,
as.duration(today() - my_birthday)
# [1] "544233600s (~17.25 years)"

# Further details can be found at http://r4ds.had.co.nz/dates-and-times.html

# Note that we can also use data.table in order to work on date/time values. Common date/time functions in data.table include,
# IDateTime(x, ...)

# second(x)
# minute(x)
# hour(x)
# yday(x)
# wday(x)
# mday(x)
# week(x)
# isoweek(x)
# month(x)
# quarter(x)
# year(x)

# The feature was still experimental at the time of writing and details can be found at https://www.rdocumentation.org/packages/data.table/versions/1.10.4-2/topics/IDateTime


## Checking data quality
## Packages Used:
## psych, pastecs, dataMaid, daff

# install.packages(c("psych","pastecs","dataMaid","daff"))

state <- data.frame(state.x77)
state$State <- row.names(state)
state


summary(state)

# Population        Income       Illiteracy       Life.Exp         Murder      
# Min.   :  365   Min.   :3098   Min.   :0.500   Min.   :67.96   Min.   : 1.400  
# 1st Qu.: 1080   1st Qu.:3993   1st Qu.:0.625   1st Qu.:70.12   1st Qu.: 4.350  
# Median : 2838   Median :4519   Median :0.950   Median :70.67   Median : 6.850  
# Mean   : 4246   Mean   :4436   Mean   :1.170   Mean   :70.88   Mean   : 7.378  
# 3rd Qu.: 4968   3rd Qu.:4814   3rd Qu.:1.575   3rd Qu.:71.89   3rd Qu.:10.675  
# Max.   :21198   Max.   :6315   Max.   :2.800   Max.   :73.60   Max.   :15.100  
# HS.Grad          Frost             Area           State          
# Min.   :37.80   Min.   :  0.00   Min.   :  1049   Length:50         
# 1st Qu.:48.05   1st Qu.: 66.25   1st Qu.: 36985   Class :character  
# Median :53.25   Median :114.50   Median : 54277   Mode  :character  
# Mean   :53.11   Mean   :104.46   Mean   : 70736                     
# 3rd Qu.:59.15   3rd Qu.:139.75   3rd Qu.: 81162                     
# Max.   :67.30   Max.   :188.00   Max.   :566432  


library(psych)
describe(state)

# vars  n     mean       sd   median  trimmed      mad     min      max     range
# Population    1 50  4246.42  4464.49  2838.50  3384.28  2890.33  365.00  21198.0  20833.00
# Income        2 50  4435.80   614.47  4519.00  4430.07   581.18 3098.00   6315.0   3217.00
# Illiteracy    3 50     1.17     0.61     0.95     1.10     0.52    0.50      2.8      2.30
# Life.Exp      4 50    70.88     1.34    70.67    70.92     1.54   67.96     73.6      5.64
# Murder        5 50     7.38     3.69     6.85     7.30     5.19    1.40     15.1     13.70
# HS.Grad       6 50    53.11     8.08    53.25    53.34     8.60   37.80     67.3     29.50
# Frost         7 50   104.46    51.98   114.50   106.80    53.37    0.00    188.0    188.00
# Area          8 50 70735.88 85327.30 54277.00 56575.72 35144.29 1049.00 566432.0 565383.00
# State*        9 50      NaN       NA       NA      NaN       NA     Inf     -Inf      -Inf
# skew kurtosis       se
# Population  1.92     3.75   631.37
# Income      0.20     0.24    86.90
# Illiteracy  0.82    -0.47     0.09
# Life.Exp   -0.15    -0.67     0.19
# Murder      0.13    -1.21     0.52
# HS.Grad    -0.32    -0.88     1.14
# Frost      -0.37    -0.94     7.35
# Area        4.10    20.39 12067.10
# State*        NA       NA       NA


# You can also use describe.by to get summary information on a per group basis, for eg.,
describe.by(state,state$State)


# Descriptive statistics by group 
# group: Alabama
# vars n     mean sd   median  trimmed mad      min      max range skew kurtosis se
# Population    1 1  3615.00 NA  3615.00  3615.00   0  3615.00  3615.00     0   NA       NA NA
# Income        2 1  3624.00 NA  3624.00  3624.00   0  3624.00  3624.00     0   NA       NA NA
# Illiteracy    3 1     2.10 NA     2.10     2.10   0     2.10     2.10     0   NA       NA NA
# Life.Exp      4 1    69.05 NA    69.05    69.05   0    69.05    69.05     0   NA       NA NA
# Murder        5 1    15.10 NA    15.10    15.10   0    15.10    15.10     0   NA       NA NA
# HS.Grad       6 1    41.30 NA    41.30    41.30   0    41.30    41.30     0   NA       NA NA
# Frost         7 1    20.00 NA    20.00    20.00   0    20.00    20.00     0   NA       NA NA
# Area          8 1 50708.00 NA 50708.00 50708.00   0 50708.00 50708.00     0   NA       NA NA
# State*        9 1      NaN NA       NA      NaN  NA      Inf     -Inf  -Inf   NA       NA NA

library(pastecs)
stat.desc(state)

# > stat.desc(state)
# Population       Income Illiteracy     Life.Exp      Murder      HS.Grad
# nbr.val      5.000000e+01 5.000000e+01 50.0000000 5.000000e+01  50.0000000   50.0000000
# nbr.null     0.000000e+00 0.000000e+00  0.0000000 0.000000e+00   0.0000000    0.0000000
# nbr.na       0.000000e+00 0.000000e+00  0.0000000 0.000000e+00   0.0000000    0.0000000
# min          3.650000e+02 3.098000e+03  0.5000000 6.796000e+01   1.4000000   37.8000000
# max          2.119800e+04 6.315000e+03  2.8000000 7.360000e+01  15.1000000   67.3000000
# range        2.083300e+04 3.217000e+03  2.3000000 5.640000e+00  13.7000000   29.5000000
# sum          2.123210e+05 2.217900e+05 58.5000000 3.543930e+03 368.9000000 2655.4000000
# median       2.838500e+03 4.519000e+03  0.9500000 7.067500e+01   6.8500000   53.2500000
# mean         4.246420e+03 4.435800e+03  1.1700000 7.087860e+01   7.3780000   53.1080000
# SE.mean      6.313744e+02 8.689917e+01  0.0862010 1.898431e-01   0.5220626    1.1422600
# CI.mean.0.95 1.268794e+03 1.746304e+02  0.1732274 3.815040e-01   1.0491240    2.2954574
# var          1.993168e+07 3.775733e+05  0.3715306 1.802020e+00  13.6274653   65.2378939
# std.dev      4.464491e+03 6.144699e+02  0.6095331 1.342394e+00   3.6915397    8.0769978
# coef.var     1.051354e+00 1.385252e-01  0.5209685 1.893934e-02   0.5003442    0.1520863

library(dataMaid)
makeDataReport(state)

library(daff)
state <- data.frame(state.x77)
state2 <- state
identical(state, state2)

state2$Population <- state2$Population+1
diff_data(state,state2)

#' Daff Comparison: ‘state’ vs. ‘state2’ 
#' First 6 and last 6 patch lines:
#'   @:@              A:A    B:B        C:C      D:D    E:E     F:F   G:G    H:H
#' 1          @@   Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
#' 2     1:1  ->   3615->3616   3624        2.1    69.05   15.1    41.3    20  50708
#' 3     2:2  ->     365->366   6315        1.5    69.31   11.3    66.7   152 566432
#' 4     3:3  ->   2212->2213   4530        1.8    70.55    7.8    58.1    15 113417
#' 5     4:4  ->   2110->2111   3378        1.9    70.66   10.1    39.9    65  51945
#' 6     5:5  -> 21198->21199   5114        1.1    71.71   10.3    62.6    20 156361
#' ...   ... ...          ...    ...        ...      ...    ...     ...   ...    ...
#' 47  45:45  ->     472->473   3907        0.6    71.64    5.5    57.1   168   9267
#' 48  46:46  ->   4981->4982   4701        1.4    70.08    9.5    47.8    85  39780
#' 49  47:47  ->   3559->3560   4864        0.6    71.72    4.3    63.5    32  66570
#' 50  48:48  ->   1799->1800   3617        1.4    69.48    6.7    41.6   100  24070
#' 51  49:49  ->   4589->4590   4468        0.7    72.48      3    54.5   149  54464
#' 52  50:50  ->     376->377   4566        0.6    70.29    6.9    62.9   173  97203

diffed <- diff_data(state,state2)
render_diff(diffed)

# You can also "patch" the data using patch_data and merge datasets using merge_data

## Working with Web Data

# data.table: fread function can use an URL as the source
flights_extra <- fread("http://ourairports.com/data/airports.csv")

# httr: to work with webpages
install.packages("httr")
library(httr)

rxds <- GET("http://www.rxdatascience.com")
rxds

# View the HTTP headers
headers(rxds)

# View the content
content_rxds <- content(rxds, "text")
substr(content_rxds,1,500)

# Use parsers
str(content(rxds, "parsed"))

# Web APIs
# Examples: http://ropengov.github.io/projects/
# Examples: https://dev.socrata.com/docs/endpoints.html -> https://cran.r-project.org/web/packages/RSocrata/index.html

# Example of a Web API Call
install.packages("RSocrata")
library(RSocrata)
datasets <- ls.socrata("https://soda.demo.socrata.com")

names(datasets)

# View the datasets
data.frame(datasets$title, datasets$identifier)

# Simple download
population <- read.socrata("https://soda.demo.socrata.com/api/views/irft-er6i")

# Using App Tokens
token <- "ew2rEMuESuzWPqMkyPfOSGJgE" # From https://github.com/Chicago/RSocrata
population <- read.socrata("https://soda.demo.socrata.com/api/views/irft-er6i", app_token = token)
head(population)

# Using ID/Password
# ** This needs to be set up, create an account at https://opendata.socrata.com/login **

email <- Sys.getenv("SOCRATA_EMAIL", "Your_Registered_Email_Address")
password <- Sys.getenv("SOCRATA_PASSWORD", "Your_Password")
privateURL <- "https://soda.demo.socrata.com/resource/a9g2-feh2.csv" # dataset

# Uncomment the following line once you have set up your id and password
read.socrata(url = privateURL, email = email, password = password)


## Tutorial: Looking at Airline Flight Times Data
## This is a general tutorial aimed at finding interesting snippets of information from Flights data available in the package hflights

## The description of the dataset contains information on the characteristics of the data:
# This dataset contains all flights departing from Houston airports IAH (George Bush Intercontinental) and HOU (Houston Hobby). The data comes from the Research and Innovation Technology Administration at the Bureau of Transporation statistics: http://www.transtats.bts.gov/DatabaseInfo.asp?DB_ID=120&Link=0

install.packages("nycflights13")
library(nycflights13)

flights <- data.table(nycflights13::flights)
flights

# Get descriptive information about the dataset
describe(flights)


# > describe(flights)
# vars      n    mean      sd median trimmed     mad  min  max range  skew kurtosis   se
# year              1 336776 2013.00    0.00   2013 2013.00    0.00 2013 2013     0   NaN      NaN 0.00
# month             2 336776    6.55    3.41      7    6.56    4.45    1   12    11 -0.01    -1.19 0.01
# day               3 336776   15.71    8.77     16   15.70   11.86    1   31    30  0.01    -1.19 0.02
# dep_time          4 328521 1349.11  488.28   1401 1346.82  634.55    1 2400  2399 -0.02    -1.09 0.85
# sched_dep_time    5 336776 1344.25  467.34   1359 1341.60  613.80  106 2359  2253 -0.01    -1.20 0.81
# dep_delay         6 328521   12.64   40.21     -2    3.32    5.93  -43 1301  1344  4.80    43.95 0.07
# arr_time          7 328063 1502.05  533.26   1535 1526.42  619.73    1 2400  2399 -0.47    -0.19 0.93
# sched_arr_time    8 336776 1536.38  497.46   1556 1550.67  618.24    1 2359  2358 -0.35    -0.38 0.86
# arr_delay         9 327346    6.90   44.63     -5   -1.03   20.76  -86 1272  1358  3.72    29.23 0.08
# carrier*         10 336776    9.00    0.00      9    9.00    0.00    9    9     0   NaN      NaN 0.00
# flight           11 336776 1971.92 1632.47   1496 1830.51 1608.62    1 8500  8499  0.66    -0.85 2.81
# tailnum*         12 334264     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# origin*          13 336776     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# dest*            14 336776     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# air_time         15 327346  150.69   93.69    129  140.03   75.61   20  695   675  1.07     0.86 0.16
# distance         16 336776 1039.91  733.23    872  955.27  569.32   17 4983  4966  1.13     1.19 1.26
# hour             17 336776   13.18    4.66     13   13.15    5.93    1   23    22  0.00    -1.21 0.01
# minute           18 336776   26.23   19.30     29   25.64   23.72    0   59    59  0.09    -1.24 0.03
# time_hour*       19 336776     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA

# We can also get descriptive information on a per NYC Airport Basis
# There are 3 major airports in the nycflights13 dataset

flights[,c(unique(origin))]
# [1] "JFK" "EWR" "LGA"

# Using describe.by, we can get descriptive information on each of them
describe.by(flights, flights$origin)


# group: JFK
# vars      n    mean      sd median trimmed     mad  min  max range  skew kurtosis   se
# year              1 111279 2013.00    0.00   2013 2013.00    0.00 2013 2013     0   NaN      NaN 0.00
# month             2 111279    6.50    3.41      7    6.50    4.45    1   12    11  0.00    -1.18 0.01
# day               3 111279   15.73    8.79     16   15.73   11.86    1   31    30  0.00    -1.19 0.03
# dep_time          4 109416 1398.57  505.53   1500 1403.10  630.11    1 2400  2399 -0.17    -1.02 1.53
# sched_dep_time    5 111279 1401.93  482.27   1459 1403.55  636.04  540 2359  1819 -0.11    -1.20 1.45
# dep_delay         6 109416   12.11   39.04     -1    3.07    5.93  -43 1301  1344  5.45    64.43 0.12
# arr_time          7 109284 1520.07  579.09   1625 1565.91  690.89    1 2400  2399 -0.66    -0.08 1.75
# sched_arr_time    8 111279 1564.98  544.69   1647 1599.73  641.97    1 2359  2358 -0.64     0.00 1.63
# arr_delay         9 109079    5.55   44.28     -6   -2.03   20.76  -79 1272  1351  3.99    38.99 0.13
# carrier*         10 111279    9.00    0.00      9    9.00    0.00    9    9     0   NaN      NaN 0.00
# flight           11 111279 1365.75 1376.74    801 1181.75 1009.65    1 5765  5764  1.05     0.10 4.13
# tailnum*         12 110370     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# origin*          13 111279     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# dest*            14 111279     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA
# air_time         15 109079  178.35  113.79    149  172.81  139.36   21  691   670  0.49    -0.75 0.34
# distance         16 111279 1266.25  896.11   1069 1229.73 1138.64   94 4983  4889  0.48    -0.67 2.69
# hour             17 111279   13.74    4.80     14   13.77    5.93    5   23    18 -0.11    -1.22 0.01
# minute           18 111279   27.50   19.36     30   27.24   22.24    0   59    59  0.00    -1.26 0.06
# time_hour*       19 111279     NaN      NA     NA     NaN      NA  Inf -Inf  -Inf    NA       NA   NA


# Uncomment when re-loading the data
# flights_extra <- fread("http://ourairports.com/data/airports.csv")

# Extract the columns name, iata_code and region for only those entries where the IATA code exists
flights_extra2 <- flights_extra[!iata_code=="",.(dest_name=name, dest=iata_code, region=iso_region)]
flights_extra2

setkey(flights, dest)
setkey(flights_extra2, dest)

# The table, flights does not contain the names of the destination airport. Using the data from the flights_extra2 data frame, we'd like to add the respective information to the flights data
# We can do this using the "merge" function or the in-built functionality of data.table which lets us subset using the key columns

flights2 <- flights_extra2[flights]
flights2

# Find the maximum, minimum and average of flight timings between JFK and any other airport, ordered by the highest AvgTime (descending)

flights3 <- flights[,.(MaxTime=max(air_time, na.rm = T), MinTime=min(air_time, na.rm = T), AvgTime=mean(air_time, na.rm = T)), by=c("origin", "dest")]
flights3 <- merge(flights3, flights_extra2[,.(dest, dest_name)], by = "dest")
flights3[order(-AvgTime)]

# From this we can see that the flight with the longest average duraation (498 minutes) is between JFK/EWR and Daniel K Inouye International Airport in Honolulu
# > flights3[order(-AvgTime)]
# dest origin MaxTime MinTime   AvgTime                                       dest_name
# 1:  HNL    JFK     691     580 623.08772           Daniel K Inouye International Airport
# 2:  HNL    EWR     695     562 612.07521           Daniel K Inouye International Airport
# 3:  ANC    EWR     434     388 413.12500     Ted Stevens Anchorage International Airport
# 4:  SFO    JFK     490     301 347.40363             San Francisco International Airport
# 5:  SJC    JFK     396     305 346.60671 Norman Y. Mineta San Jose International Airport


# Find the average delays between all airports

flights4 <- flights[,.(MeanDelay=mean(arr_delay+dep_delay, na.rm=T)), by=c("origin","dest")][order(-MeanDelay)]

# origin dest MeanDelay
# 1:    EWR  TYS  82.80511
# 2:    EWR  CAE  78.94681
# 3:    EWR  TUL  68.54762
# 4:    EWR  OKC  59.80000
# 5:    EWR  JAC  59.73684

# Since the flights_extra2 is keyed on "dest" airport code, we can very easily extract the names of the airports (and join it with the respective table) by using the tablename[key] method as follows:

setkey(flights4, dest)
flights_extra2[flights4][order(-MeanDelay)]

# > flights_extra2[flights4][order(-MeanDelay)]
#     dest_name dest region origin MeanDelay
# 1:  McGhee Tyson Airport  TYS  US-TN    EWR  82.80511
# 2:  Columbia Metropolitan Airport  CAE  US-SC    EWR  78.94681
# 3:  Tulsa International Airport  TUL  US-OK    EWR  68.54762
# 4:  Will Rogers World Airport  OKC  US-OK    EWR  59.80000


# From this, we can see that the highest average delay was between EWR (Newark) and McGhee Tyson airport in Tennessee (82 minutes)

# To see which of the NYC Airports is the busiest, we can use:

flights[,.(total_flights=.N, total_distance_flown=sum(distance), total_air_time=sum(air_time, na.rm = T)), by=origin]

# origin total_flights total_distance_flown total_air_time
# 1:    JFK        111279            140906931       19454136
# 2:    EWR        120835            127691515       17955572
# 3:    LGA        104662             81619161       11916902

# This shows that even though JFK has comparatively lesser flights than EWR, it has a much higher total_distance_flown and total_air_time

# In order to check which airlines have the best overall performance, we can perform a group-by operation using the carrier code

# Notice that mix dplyr code along with data.table as follows

flights[,.(TotalFlights = .N, AvgDelay=mean(arr_delay+dep_delay, na.rm=T)), by=carrier][order(AvgDelay)] %>% left_join(nycflights13::airlines)

# > flights[,.(TotalFlights = .N, AvgDelay=mean(arr_delay+dep_delay, na.rm=T)), by=carrier][order(AvgDelay)] %>% left_join(nycflights13::airlines)
# Joining, by = "carrier"
# carrier TotalFlights  AvgDelay                        name
# 1       AS          714 -4.100141        Alaska Airlines Inc.
# 2       HA          342 -2.014620      Hawaiian Airlines Inc.
# 3       US        20536  5.874288             US Airways Inc.
# 4       AA        32729  8.933421      American Airlines Inc.
# 5       DL        48110 10.868291        Delta Air Lines Inc.
# 6       VX         5162 14.521110              Virgin America
# 7       UA        58665 15.574920       United Air Lines Inc.

# This shows the best on-time performance is that of Alaska Airlines followed by Hawaiian, US Airways, American Airlines and Delta

# We can combine outputs also with ggplot2 to make nice looking charts
flights[,.(TotalFlights = .N, AvgDelay=mean(arr_delay+dep_delay, na.rm=T)), by=carrier][order(AvgDelay)] %>% 
  left_join(nycflights13::airlines) %>%
  arrange(desc(AvgDelay)) %>%
  rename(Airline=name) %>%
  ggplot(aes(x=reorder(Airline, -AvgDelay),y=AvgDelay, fill=Airline)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(AvgDelay)), vjust=1.6, color="white", size=4) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1, size = 12)) +
  labs (title = "Average Airline Delays from NYC", x = "Airline", y = "Average Delay (Minutes)")
  



