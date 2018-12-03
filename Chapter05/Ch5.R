
library(data.table)
library(tidyverse)
library(carData)

car_data <- mtcars

# See the top few rows of the mtcars dataset
head(car_data)

# Creating the CSV file
write.csv(car_data,"car_data.csv")
fwrite(car_data, "car_data.csv")

# View the file that was saved in the current directory
list.files(".", pattern = "car_data*")
# [1] "car_data.csv"

# Reading the CSV file
read.csv("car_data.csv") # Base R
read_csv("car_data.csv") # Tidyverse
fread("car_data.csv")    # data.table

# While these are mostly similar, note that the output is slightly different. The first one, read.csv is from Base R (i.e., it comes in-built in R) and creates a data.frame. The second, read_csv is from the tidyverse package and creates a "tibble" as discussed in Chapter 3. The third option, fread produces a data.table which has different characteristics compared to data.frame, but is arguably much faster in reading large amounts of data.

# Once you have read the file, the next step is to see what the data contains and the respective data types. This can be doned using the str command in R. The command, str, stands for "Structure" and provides a summarised overview of the datatypes.

# To view data type and first few records
str(read.csv("car_data.csv"))     # Base R
glimpse(read_csv("car_data.csv")) # Tidyverse

## Using POSIXct/lt

# POSIXct
# This gives the number of seconds since the Unix "epoch", i.e., 1970-01-01
unclass(Sys.time())

# We can derive the date representation using as.POSIXct as follows:
as.POSIXct(unclass(Sys.time()), origin="1970-01-01")


# POSIXlt
# POSIXlt is similar to POSIXct, but is used for dates represented as characters. This is the most common representation you'll find in R
# As per the R help page:

# Character input is first converted to class "POSIXlt" by strptime: numeric input is first converted to "POSIXct".

as.POSIXlt(Sys.time())


# We can also specify the format in which we'd like to get the output using the appropriate convention (see ?strptime)
# For eg. in order to get the month/date/year format, we cna use %D as shown below

format(as.POSIXct(Sys.time()), "%D")

# Oftentime, your data would contain dates in a given format on which you may need to perform further operations
# If the column has been read in as a string, you'd need to instruct R of the appropriate format in which the date
# Has been stored in order for R to recognise it as a date/time object. For this we can use the R function strptime

strptime("1/1/2000 10:15:45.123", "%d/%m/%Y %H:%M:%OS")
class(strptime("1/1/2000 10:15:45.123", "%d/%m/%Y %H:%M:%OS"))

library(lubridate)

mydt <- mdy_hms("1/1/2000 10:15:45.123") # Same as above, but using the much simpler ymd_hms function in lubridate

# There are several useful features in lubridate and a complete tutorial of the topic is available at CRAN
# https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

# A few examples as shown below:
year(mydt)
month(mydt)
wday(mydt)

# The main benefit of using such features is the ability to add/subtract/perform date/time operations on the data
# Which would otherwise be not possible if the dates were simply treated as strings.

# For example, if I had to add 1 day to 2000/01/30, I can do so after converting the string to be recognised as 
# the appropriate date/time object in R

"2000/01/30" + 1 # This would produce an error message
# Error in "2000/01/30" + 1 : non-numeric argument to binary operator

ymd("2000/01/30") + 1

## String Manipulations

# Subsetting Strings

substr("I think, therefore I am",1,8) # Requires values for start and stop

substring("I think, therefore I am",10) # Requires only the start value

# Combining Strings
# Strings can be combined using paste, paste0 and other utilities

# Using paste: The default separator is space
paste("I","think","therefore","I","am")

# We caan use a different separator by using the sep flag
paste("I","think","therefore","I","am", sep="-")

# paste0 is an extension of paste where the separator is null. As a result all strings are concatenated into a single string unless separators are specified
paste0("I","think","therefore","I","am")

# In addition to the sep flag, there is also a collapse flag that can be used to separate the results

paste("grade",c("A","B","C"), sep=" ") # paste with separator space

paste("grade",c("A","B","C"), sep=" ", collapse=",") # paste with separator space and collapse with "," (comma)

paste0("grade",c("A","B","C"), collapse=",") # paste0 is set to the separator "" (null)

library(stringr) 

str_c("grade",c("A","B","C",NULL)) # str_c from the stringr package is a newer (and somewhat simpler) alternative to paste/paste0 in R


# Splitting Strings

strsplit("I think, therefore I am",",")

# tstrsplit does a transpose on the split vector
# This is helpful when splitting strings and updating say, a column in a data frame

tstrsplit("I think, therefore I am",",")

# The difference can be easily observed herein

# Create a simple data frame
df <- data.frame(a=c(1,2))

# strsplit creates a vector
df$s <- strsplit("I think, therefore I am",",")
df

# a                        s
# 1 1 I think,  therefore I am
# 2 2 I think,  therefore I am


# tstrsplit creates a transpose of the vector, t
df$s <- tstrsplit("I think, therefore I am",",")
df

# a               s
# 1 1         I think
# 2 2  therefore I am



# Simple Pattern Matching and Replacement with R

# grep/grepl
# grep is a common function to find matches in R. The general syntax is grep(pattern, text)

grep("think",c("I think","therefore I am")) # Gives the index of the match

grepl("think",c("I think","therefore I am")) # Gives a logical vector of the match

# It is possible to replace only characters that match using gsub, regexpr and other Pattern Matching and Replacement functions in R
gsub("th","ab",c("I think","therefore I am")) # To replace all occurrences of "th" in the text

# For users who are familiar with Regular Expressions, R also supports Regex patterns (eg., PCRE, etc)
gsub("t.","ab",c("I think","therefore I am"), perl=T) # To replace all occurrences of "th" in the text


# Printing results
# Results can be printed in R using print, cat and other commands

# print is often used as a means to document code output as
(function(x){x=10;print ("x is now 10")})()

# print also supports other flags such as digits (number of digits to print), justify (left/right), quote (whether strings should be quoted), for eg.,
print(1.1234567,digits=3)

# Formatting strings can be done using the format command in R
# It is one of the most versatile functions that allow users to present data in a more readable manner

# For instaance, to add a , separator to numbers, we can use the big.mark argument in format as follows
format(14324234,big.mark=",")

# Other options as specified in the R help page include:
# big.mark, big.interval, small.mark, small.interval, decimal.mark, zero.print, drop0trailing

# Data Visualisation
library(car)
library(RColorBrewer)
data("Salaries")

# Histogram

# Histogram of Salaries of Professors from the Salaries dataset
hist(Salaries$salary/1000, main="Histogram of Salaries", xlab="Salary (in '000s)", ylab="Count")

# Same as above with a Brewer Palette
h1 <- hist(Salaries$salary/1000,main="Histogram of Salaries", xlab="Salary (in '000s)", 
     ylab="Count", col=brewer.pal(8,"Reds"))

names(h1)
# [1] "breaks"   "counts"   "density"  "mids"     "xname"    "equidist"

h1$counts
#  [1]   1  50  90 114  60  48  23   8   2   1

h1$breaks
#  [1]  40  60  80 100 120 140 160 180 200 220 240

# Line Plots

data("Hartnagel")
?Hartnagel # Canadian Crime-Rates Time Series

plot(Hartnagel$year, Hartnagel$mtheft, type="l", col=brewer.pal(8,"Set1"))

# Scatter Plots
plot(Hartnagel$year, Hartnagel$mtheft, col=brewer.pal(8,"Set1"))

# To view the various parameters supported in R graphics
?par

# Boxplots

b = boxplot(Salaries$salary~Salaries$rank, col=brewer.pal(8,"Set1"))

names(b)
# [1] "stats" "n"     "conf"  "out"   "group" "names"

b$stats
#         [,1]     [,2]     [,3]
# [1,] 63100.0  62884.0  57800.0
# [2,] 74000.0  82350.0 105890.0
# [3,] 79800.0  95626.5 123321.5
# [4,] 88597.5 104331.5 145098.0
# [5,] 97032.0 126431.0 194800.0
# attr(,"class")
# AsstProf 
# "integer" 

# Bar Charts
barplot(Hartnagel$mtheft)
ggplot(Hartnagel, aes(year,mconvict,fill=as.factor(year - round(year%%10)))) + 
  geom_bar(stat="identity") + scale_fill_discrete(name = "Decade") + xlab("Decade") + 
  ylab ("Number of Convicts")

ggplot(Hartnagel, aes(year,mconvict,fill=as.factor(year - round(year%%10)))) + 
  geom_bar(stat="identity") + scale_fill_discrete(name = "Decade") + xlab("Decade") + 
  ylab ("Male indictable-offense conviction rate per 100,000") + coord_flip()

hm <- melt(Hartnagel, id.vars="year",measure.vars=c("mconvict","fconvict"), 
           variable.name="gender",value.name="convict")

ggplot(hm, aes(year,convict,fill=as.factor(gender))) + geom_bar(stat="identity") + 
  scale_fill_discrete(name = "Gender") + xlab("Decade") + ylab ("Number of Convicts")

# Heatmap
devtools::install_github("rlbarter/superheat")
library(superheat)
row.names(Hartnagel) <- Hartnagel$year

superheat(Hartnagel[,-1],heat.pal = c("#b35806", "white", "#542788"),heat.na.col = "white", 
          yr = Hartnagel$mconvict, yr.axis.name = "Male Conviction Rate", 
          bottom.label.text.angle = 90,scale=T,left.label.text.size = 3.5, bottom.label.text.size = 4)


# Summarise Data
install.packages("ggcorrplot")
library("ggcorrplot")
hcorr <- cor(na.omit(Hartnagel))
ggcorrplot(hcorr,hc.order = T, ggtheme = theme_bw, insig="blank", 
           colors = brewer.pal(11,"RdYlGn")[c(1,6,11)])

plot(Hartnagel) # Simply calling plot

# Table Plot

library(tabplot)
tableplot(Salaries)
