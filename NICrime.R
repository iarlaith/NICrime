
# Msc Big Data Analytics - Data Science
# Iarlaith McLaughlin (L00144319)
# 29/03/2019
# CA2 - NI Postcodes and crime data

############################### SECTION 1 ###########################################################

###Cleaning and Manipulating the Post Code Data from NIPostcodes File
## Loading the NIPostcodes file to a dataframe object and naming its columns
NIPostcodes <- read.csv("NIPostcodes.csv", header = FALSE)

# A) Show total number of rows, the structure and first 10 rows
head(NIPostcodes, 10)
nrow(NIPostcodes)
str(NIPostcodes)


# B) Add a suitable title for each attribute of the data 
colnames(NIPostcodes) <- c('Organisation_Name', 'Sub-building_Name', 'Building_Name', 'Number', 
                           'Primary_Thorfare', 'Alt_Thorfare','Secondary_Thorfare', 'Locality', 
                           'Townland', 'Town', 'County', 'Postcode', 'x-coordinates',
                           'y-coordinates', 'Primary_Key_(identifier)')
head(NIPostcodes, 10)
str(NIPostcodes)

# C) Replacing missing values from the data as some identifier.
NIPostcodes[NIPostcodes == ''] <- NA
head(NIPostcodes, 10)
str(NIPostcodes)

#D) Show the total number and mean missing values for each column in the postcode data frame

sapply(X = NIPostcodes, function(x) sum(is.na(x)))
sapply(NIPostcodes, function(x) mean(is.na(x))) * 100

#E) Modify the count attribute to be a categorizing factor.
str(NIPostcodes)
NIPostcodes$County <- factor(NIPostcodes$County, order = TRUE, levels = c("ANTRIM", "ARMAGH", 
                                                                          "LONDONDERRY", "TYRONE",
                                                                          "FERMANAGH", "DOWN"))
str(NIPostcodes)

# F) Moving the Primary Key identifier from last column to first column.
head(NIPostcodes)
NIPostcodes <- NIPostcodes[, c(15, 1:14)]
head(NIPostcodes)
str(NIPostcodes)

#G)  Create a new dataset called Limavady_data. Store within it only information
# that has locality, townland and town containing the name Limavady. Store this
# information in an external csv file called Limavady.
Limavady_data <- NIPostcodes[which(NIPostcodes$Town =="LIMAVADY" | NIPostcodes$Townland =="LIMAVADY" |
                                     NIPostcodes$Locality == "LIMAVADY"),]
write.csv(Limavady_data, file = "LimavadyData.csv", row.names = FALSE)
head(Limavady_data)
str(Limavady_data)

#H) Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData.
head(NIPostcodes)
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv", row.names = FALSE)
str(NIPostcodes)

############################### SECTION 2 ###########################################################

#Check working directory and List Files in the working directory
getwd()
list.files()

#Creating a function for read all the different month's NI Crime csv files in the directory
load_all_files <- function(path) {
  files <- dir(path, pattern = '\\.csv', full.names = TRUE, recursive = TRUE)
  tables <- lapply(files, read.csv)
  do.call(rbind, tables)
}

#A) Amalgamate all of the crime data from each csv file into one dataset. Save this dataset into a csv file called AllNICrimeData.
#Calling the function to combine all the files from extarcted rar folder into a new dataframe
AllNICrimeData <- load_all_files("./NI Crime Data")

#read the top 6 rows of the object AllNICrimeData using head function
head(AllNICrimeData)

#check the structure of AllNICrimeData
str(AllNICrimeData)

#write the dataframe to an output csv file and analyze the data
write.csv(AllNICrimeData, file = "AllNICrimeData.csv", row.names = FALSE)

#Print the number of Rows in AllNICrimeData
nrow(AllNICrimeData)

#2) Modify the crime data in the newly created csv file (stored in AllNICrimeData) and remove the attributes:
# CrimeID,Reported by,Falls within,LSOA code,LSOA name, Last.outcome, Context.
AllNICrimeData <- subset(AllNICrimeData, select = -c(Crime.ID, Reported.by, Falls.within, LSOA.code, 
                                                     LSOA.name, Last.outcome.category, Context))
#Check contents and structure of newly created dataframe with subset columns
str(AllNICrimeData)
head(AllNICrimeData)

# (c) Factorise the Crime type attribute. Show the modified structure.
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
# Show factored crime type attribute
str(AllNICrimeData)
# Show factored levels
levels(factor(AllNICrimeData$Crime.type))

# (d) Modify AllNICrimeData  so location attribute only contains street name
#using gsub to extract or trim "on or near" from locaion
head(AllNICrimeData)
AllNICrimeData$Location <- gsub( "On or near ", "", AllNICrimeData$Location, ignore.case = TRUE)
# Modify resultant empty strings with suitable idenetifier - NA
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
head(AllNICrimeData)
str(AllNICrimeData)

# (e) Choose 1000 random samples of crime data from the AllNICrimeData dataset where 
# the location attribute contains location information. This means that the location 
# information should NOT contain an NA identifier. Store this data in a data frame 
# called random_crime_sample. 
colSums(is.na(AllNICrimeData))
random_crime_sample <- AllNICrimeData[ sample( which(AllNICrimeData$Location !='NA'), 1000 ), ]
#print out number of rows to confirm 1000 have been selected
nrow(random_crime_sample)
#print a sample to investigate if all entries have a location
head(random_crime_sample, 10)
str(random_crime_sample)

# (e contnued): Then create a function called find_a_postcode that takes 
# as an input each location attribute from random_crime_sample and finds a suitable 
# postcode value from the postcode dataset. Use the CleanNIPostcodeData dataset you 
# created in section 1 as the reference data to find postcodes. If there are several 
# postcodes discovered with the same location, choose the most popular postcode for 
# that location. Store the output from the find_a_postcode function in a suitably 
# named variable in your random_crime_sample data frame. Make sure there are no missing 
# postcodes in the output from your function. Show the structure of this data frame 
# once you’ve completed this task and count the number of records in the modified 
# random_crime_sample data frame.

#install.packages("dplyr")
library(dplyr)

# Function to find a post code based on the location in the crime_data file.
find_a_postcode <- function(crime_data){
  
  # First the CleanNIPostcode.csv file is read in with any empty strings being replaced with 'NA'. All other columns
  # were then dropped apart from Primary_Thorfare and Postcode. Then Primary_Thorfare and Location changed to all upper case for comparision. 
  new_CleanNIPostCode <- read.csv(file = "CleanNIPostcodeData.csv", header=TRUE, na.strings=c("","NA"))
  new_CleanNIPostCode = subset(new_CleanNIPostCode, select = c(Primary_Thorfare, Postcode))
  new_CleanNIPostCode$Primary_Thorfare <- toupper(new_CleanNIPostCode$Primary_Thorfare)
  
  # Using dplyr package to send new_CleanNIPostcode to be grouped by Primary_Thorfare and then 
  # the max occurrence for each Poscode taken by using the function summarize. This reduces the multiple Postcode values down
  # to one by selecting the max value (most frequent postcode). Finally this is sent to our updated dataframe new_CleanNIPostcode.
  new_CleanNIPostCode <- new_CleanNIPostCode %>% group_by(Primary_Thorfare) %>% summarize(Postcode = names(which.max(table(Postcode)))) 
  
  # Compares Location in crime_data to Primary_Thorfare in new_CleanNIPostCode and stores results in match_result. 
  # The inner_join was used as this will ensure any NAs are dropped from the result.
  match_result <- dplyr::inner_join(crime_data, new_CleanNIPostCode, by=c("Location" = "Primary_Thorfare"))
  return(match_result)
}

# Changing Location to upper case to compare with Primary_Thorfare 
random_crime_sample$Location <- toupper(random_crime_sample$Location)

crime_data_with_postcode <- find_a_postcode(random_crime_sample)
head(crime_data_with_postcode)
str(crime_data_with_postcode)
nrow(crime_data_with_postcode)

# (f) Append the data output from your find_a_postcode function to the
# random_crime_sample dataset. Show the modified structure. Save the modified
# random crime sample data frame as random_crime_sample.csv.

head(crime_data_with_postcode)
write.csv(crime_data_with_postcode, file = "random_crime_sample.csv")
str(crime_data_with_postcode)


# (g) Now we need to update the random sample so that it contains only the following items 
# • Month 
# • Longitude 
# • Latitude 
# • Location 
# • Crime.type 
# • Postcode 
# Extract this data into a new data frame called updated_random_sample. Then create another 
# data frame called chart_data using the updated_random_sample data frame. Sort the 
# chart_data data frame by postcode where the postcode contains “BT1” and then by crime type. # 
# Show the summary statistics for the crime type from this chart_data data frame.

head(crime_data_with_postcode)
updated_random_sample <- crime_data_with_postcode
chart_data <- updated_random_sample
summary(chart_data)

chart_data <- dplyr::filter(updated_random_sample, grepl('BT1', Postcode))
head(chart_data, 20)
summary(chart_data)
str(chart_data)

# (h) Create a bar plot of the crime type from the chart_data data frame. Show a suitable 
# main title for the bar chart, and suitable x and y-axis labels. Make sure all labels on 
# the x-axis can be read. Show the bar plot in your CA document.

library(ggplot2)

# Using the ggplot package to send all crime types to a png file. GGplot was used as the standard plot in R didn't fit in 
# all the crime types but by setting the paramaters to 1490 x 550 in the png file this was achieved).

png("Crime Types.png", 1490, 550) 
ggplot(chart_data, aes(Crime.type)) + geom_bar(stat = "count") + ggtitle("Number of Occurences for each Crime Type in NI") +
  xlab("Crime Type") + ylab("Occurrences")
dev.off()



