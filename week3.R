# Enter data into vectors before constructing
# into the dataframe
date_col <- c("2018-15-10", "2018-01-11", 
              "2018-21-10", "2018-28-10", "2018-01-05")
country_col <- c("US", "US", "IRL", "IRL", "IRL")
gender_col <- c("M", "F", "F", "M", "F")
# 99 is one of the values in the age attribute which will require recoding
age_col <- c(32, 45, 25, 39, 99)
q1_col <- c(5, 3, 3, 3, 2)
q2_col <- c(4, 5, 5, 3, 2)
q3_col <- c(5, 2, 5, 4, 1)
#NA is inserted in place of the missing data for the attribute
q4_col <- c(5, 5, 5, NA, 2)
q5_col <- c(5, 5, 2, NA, 1)
column_names <- c("Date", "Country", "Gender", "Age", "Q1", "Q2", "Q3", "Q4", "Q5")
# construct a data frame form the above vectors
managers <- data.frame(date_col, country_col, gender_col, age_col, 
                       q1_col, q2_col, q3_col, q4_col, q5_col)
managers 
# Add column names to data frame using col_names vector
colnames(managers) <- column_names
str(managers)
head(managers, 20)
#Recode the incorrect age data from 99 to NA
managers$Age[managers$Age == 99] <- NA
#create a new attribute called age_cat and set values 
#in age_cat to the following if true
# <=25 = young
# >=45 = Elderly
#we will also recode age NA to Elderly
managers$age_cat[managers$Age >=45] <- "Elderly"
managers$age_cat[managers$Age >= 26 & managers$Age <=44] <- "Middle_Aged"
managers$age_cat[managers$Age >=25] <- "Young"
managers$age_cat[is.na(managers$Age)] <- "Elderly"
#add summary column to the data form
summary_col <- managers$Q1 + managers$Q2 +
  managers$Q3 + managers$Q4 + managers$Q5
managers <- data.frame(managers, summary_col)
managers
#calculate the mean value for each row
mean_value <- rowMeans(managers[5:9])
managers <- data.frame(managers, mean_value)
managers
#change name of this column to "Mean value"
names(managers)[12] <- "Mean value" #changing column names
names(managers)[11] <- "Answer Total"
names(managers)[10] <- "Age Category"
managers
#dealing with missing data
#remove rows that contain NA
new_managers <- na.omit(managers)
new_managers
#use complete cases to show rows where data is available
complete_data <- complete.cases(managers)
complete_data
sum(complete_data)
# list the rows that do not have missing values 
#note that the ',' and no number inside the [] means "all cols"
complete_data <- managers[complete.cases(managers),]
complete_data
#find the sum of all missing values in age col
sum(is.na(managers$Age))
#use the md.pattern() in the mice package
#to show tabulated missing data pattern
install.packages("mice")
library(mice)
md.pattern(managers)
#vim pacakge includes the aggr() function
#to show the number of missing values
#in each variable and for variable combinations
install.packages("VIM")
library("VIM")
missing_values <- aggr(managers, prop = FALSE, numbers = TRUE)
summary(missing_values)
#matrixplot shows missing data coded in red
matrixplot(managers, xlab ="Item", ylab= "Index",
           main = "Identyfing the missing values in managers dataset")
missing_values_relationship <- data.frame(is.na(managers))
missing_values_relationship
missing_values_logical <- data.frame(abs(is.na(managers)))
missing_values_logical
correlation_matrix <- missing_values_logical[(apply(missing_values_logical, 2, sum)> 0)]
correlation_matrix
#correlation matrix btwn extracted values
#1= perfect positive correlation, -1 =perfect negative corelation
#0 = no correlation
cor(correlation_matrix)




