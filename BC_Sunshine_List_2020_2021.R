### How much do public servants make? A BC Sunshine List FY 2021-2021 Analysis ###

#This dataset is collected from the Vancouver Sun, which has collected and compiled the names and salaries of approximately 120,000 workers at public sector agencies across British Columbia. 

# # # # # # # # # # # # # # # # # # # # # # # 
# Install required packages
# tidyverse for data import and wrangling
# ggplot for visualization
# # # # # # # # # # # # # # # # # # # # # # #  

library(tidyverse)  #helps wrangle data
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("/Users/edjohna/Desktop/Data Analytics/CASE STUDIES/BC Sunshine List Dataset") #sets your working directory to simplify calls to data

#=====================
# STEP 1: COLLECT DATA
#=====================
# Upload datasets (csv files) here

crown <- read_csv("public_sector_salary-fy20_21-crown.csv")
govt <- read_csv("public_sector_salary-fy20_21-government.csv")
health <- read_csv("public_sector_salary-fy20_21-health.csv")
police <- read_csv("public_sector_salary-fy20_21-police.csv")
schools <- read_csv("public_sector_salary-fy20_21-schools.csv")
universities <- read_csv("public_sector_salary-fy20_21-universities.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================
# Compare column names each of the files
# While the names don't have to be in the same order, they do need to match perfectly before we can use a command to join them into one file

colnames(crown)
colnames(govt)
colnames(health)
colnames(police)
colnames(schools)
colnames(universities)

# Inspect the data frames and look for incongruities

str(crown)
str(govt)
str(health)
str(police)
str(schools)
str(universities)

# Convert Data Types (i.e. Expenses to Double and Year to Character) so that they can stack correctly

govt <-  mutate(govt, Expenses = as.double(Expenses)
                   ,Year = as.character(Year)) 

police <-  mutate(police, Year = as.character(Year)) 


# Stack individual data frames into one big data frame
salary <- bind_rows(crown,govt,health,police,schools,universities)

# Since this dataset is for FY 2020-2021 and the different agencies had different ways of typing the year 
#you decide to standardize the Year column to just show "2020-2021"
salary$Year <- "2020-2021"


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================
# Inspect the new table that has been created

colnames(salary)  #List of column names
nrow(salary)  #How many rows are in the data frame
dim(salary)  #How many rows and columns are in the data frame
head(salary)  #See the first 6 rows of data frame
str(salary)  #See list of columns and data types (numeric, character, etc)
summary(salary)  #Statistical summary of data

#REMOVE "BAD" DATA

#Check for duplicates
duplicates <- salary[duplicated(salary) | duplicated(salary, fromLast = TRUE), ]
#You see there are 506 rows of duplicated data in your table. You omit them from your data. 
salary <- distinct(salary)

#Replace NAs in Remuneration and Expenses columns to Zero
salary <- salary %>% 
  mutate_at(c('Remuneration','Expenses'), ~replace_na(.,0))

##You want to check if NAs are still in the Remuneration and Expenses columns
sum(is.na(salary$Remuneration))
sum(is.na(salary$Expenses))


#You notice some cells display as scientific notation instead of an integer
#You can effectively remove scientific notation in printing with this code:
options(scipen=999)


#You notice one employee had an expense of $102 billion! 
#You know this is clearly incorrect so you decide to get the average expenses of employees working in the same agency and in a similar role as the employee with the incorrect value. 
salary_of_vic <- salary[salary$Agency == "City of Victoria" & grepl("Director", salary$Position) & !grepl("Assistant Director|Acting Assistant Director|Deputy Director", salary$Position), ]

#Get the mean expenses of the directors (not including the director with the incorrect expense)
mean(tail(salary_of_vic$Expenses, 2))

#Now replace the salary data frame with the average expense
which(salary$Expenses == 102000000000)
salary$Expenses[37880] <- 3104

#You also notice that one employee had an expense of -376070. You decide to replace it with the average expense as well. 
salary_of_langara <- salary[salary$Agency == "Langara College" & grepl("Manager", salary$Position),]
mean(salary_of_langara$Expenses[-9])
which(salary$Expenses == -376070)
salary$Expenses[107949] <- 1638

#You sort the Remuneration and Expenses column in ascending order.
#You notice there are employees whose names are "Employee Name" in the data set and their salaries are zero. You decide to omit these rows. 
salary <- salary[salary$Name != "Employee Name", ]

#Lastly, you notice employee Stephen Andrew also has his Remuneration and Expenses set to zero. 
#You decide to get the average Remuneration and Expenses for people working in the same agency and position and replace it with that. 
salary_of_vic2 <- salary[salary$Agency == "City of Victoria" & grepl("Councillor", salary$Position), ]
mean(salary_of_vic2$Remuneration[-2])
mean(salary_of_vic2$Expenses[-2])
which(salary$Name == "Andrew, Stephen")
salary$Remuneration[37611] <- 48038
salary$Expenses[37611] <- 854

#You can now add a new 'Total' column that combines Remuneration and Expenses.
salary$Total <- salary$Remuneration + salary$Expenses

#You check if there are NAs in both the Name and Total columns. These should be omitted. 
sum(is.na(salary$Name))
sum(is.na(salary$Total))
salary_na_name <- salary[is.na(salary$Name), ]
salary <- subset(salary, !is.na(Name))

#Check again to make sure there are no NA's in your Name column. 
sum(is.na(salary$Name))

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Now that you data is clean, you can begin conducting descriptive analysis. 
mean(salary$Total) #average salary of public sector companies in BC
median(salary$Total) #midpoint salary
max(salary$Total) #highest salary
min(salary$Total) #lowest salary

# You can condense the four lines above to one line using summary() on the specific attribute
summary(salary$Total)

#Calculate the average total compensation (remuneration and expenses) by sector
avg_by_sector <- aggregate((Total) ~ Sector, data = salary, FUN = mean)
#Here we notice that, on average, employees in school districts get paid the least while police get paid the most. 

#Next, we want to check how many agencies are in our data. 
length(unique(salary$Agency))
#We see there are 113.

#Calculate the average total compensation (remuneration and expenses) by agency
avg_by_agency <- aggregate((Total) ~ Agency, data = salary, FUN = mean)
#We see that the average total compensation for employees working for Powerex Corporation (a subsidiary of BC Hydro) get paid the most. New West Police employees is 2nd by a very close margin. Interestingly, BC Hydro itself is #13 on the list. 

#You create table indexing the CEO salaries of each agency. You note that the CEOs for UBC, PHSA, BC Ferries, Translink and Interior Health receives the most compensation. 
salary_of_ceo <- subset(salary, grepl("President|CEO|Chief Executive Officer", Position) & !grepl("Vice President|Executive Assistant|Vice-President|Vice0President|Admin Asst|Executive Director|CUPE|Cupe", Position))

#Agencies with Lowest Total Compensation: Bottom 20
sorted_agency_ascending <- avg_by_agency[order(avg_by_agency$`(Total)`), ]
head(sorted_agency_ascending, 20)
#You note that school districts across BC dominate the list are among the least paid. An agency that works with First Nations communities in BC is also in the bottom 20. 

#===================================
# STEP 5: CREATE DATA VISUALIZATIONS
#===================================

#Average Total Compensation by Sector Bar Graph
ggplot(avg_by_sector, aes(x = Sector, y = `(Total)`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Sector", y = "Average Total Compensation", title = "Average Total Compensation by Sector")

#Top 10 Government Agencies by Total Compensation Bar Graph
top_10_agencies <- head(avg_by_agency[order(-avg_by_agency$`(Total)`), ], 10)

ggplot(top_10_agencies, aes(x = reorder(Agency, -`(Total)`), y = `(Total)`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Agency", y = "Average Total Compensation", title = "Top 10 Government Agencies by Average Total Compensation") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
#We can see police agencies dominate the graph.

#Bottom 20 Agencies by Total Compensation
ggplot(bottom_20_agencies, aes(x = reorder(Agency, `(Total)`), y = `(Total)`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Agency", y = "Average Total Compensation", title = "Bottom 20 Agencies by Average Total Compensation") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

#=================================================
# STEP 6: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we can visualize in Excel or Tableau
# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\Users\YOUR_USERNAME\Desktop\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/
write.csv(salary, file = '~/Desktop/salary_bc.csv')

install.packages("rmarkdown")
