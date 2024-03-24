inforceOG <- read.csv('2024-srcsc-superlife-inforce-dataset.csv')
head(inforceOG,5)
inforce <- subset(inforceOG, !is.na(X.10) & X.10 == 1)
dead.smoker <- subset(inforce, X.5 == 'S')
nrow(dead.smoker)
nrow(inforce)
inforce['Age.of.death']
barplot(table(inforce$X.7), main = "Urban vs Rural mortality", ylab = "Frequency")
barplot(table(inforceOG$X.7), main = "Urban vs Rural customers", ylab = "Frequency")
barplot(table(inforce$X.5), main = "Smoker mortality", ylab = "Frequency")
barplot(table(inforceOG$X.5), main = "Smoker status", ylab = "Frequency")
inforce$X.1 <- as.numeric(inforce$X.1)
inforce$X.2 <- as.numeric(inforce$X.2)
inforce$X.11 <- as.numeric(inforce$X.11)
inforce$Age.of.death <- c(rep(NA,3), inforce$X.2[-(1:3)] + inforce$X.11[-(1:3)] - inforce$X.1[-(1:3)])

hist(inforce$Age.of.death, main = "Histogram of Age of Death", xlab = "Age", ylab = "Frequency")
(median_age <- median(inforce$Age.of.death, na.rm = TRUE))
(average_age <- mean(inforce$Age.of.death, na.rm = TRUE))
deaths_50_70 <- inforce$Age.of.death[inforce$Age.of.death >= 50 & inforce$Age.of.death <= 70]

# Count the number of deaths
num_deaths_50_70 <- length(deaths_50_70)

# Print the result
print(num_deaths_50_70)
(table(inforceOG$X.14))

inforce.alive <- subset(inforceOG, is.na(X.10) | X.10 != 1)# Subset the data to include only smokers who are alive
alive_smokers <- subset(inforce.alive, X.5 == 'S')
nrow(alive_smokers)
# Exclude the first three rows
alive_smokers_subset <- alive_smokers[-(1:3),]
alive_smokers_subset$X.2 <- as.numeric(alive_smokers_subset$X.2)
nrow(inforce.alive)
# Create a histogram of the age distribution for the subsetted data
hist(alive_smokers_subset$X.2, breaks = 20, main = "Age Distribution of Alive Smokers", xlab = "Age", ylab = "Frequency")

# Define age brackets
age_brackets <- seq(20, 80, by = 10)  # e.g., 20-29, 30-39, ..., 70-79

# Create a function to calculate the age bracket for a given age
age_to_bracket <- function(age) {
  paste0(floor(age / 10) * 10, "-", floor(age / 10) * 10 + 9)
}

# Calculate the age bracket for each policyholder
alive_smokers_subset$age_bracket <- sapply(alive_smokers_subset$X.2, age_to_bracket)

# Calculate the number of policyholders in each age bracket
(policyholders_by_age <- table(alive_smokers_subset$age_bracket))

# Calculate the total smoking cessation treatment cost for each age bracket
(smoking_expenses_by_age <- policyholders_by_age * 932.46)

# Create a table of smoking expenses by age bracket
(smoking_expenses_table <- data.frame(
  age_bracket = names(policyholders_by_age),
  policyholders = as.numeric(policyholders_by_age),
  total_cost = smoking_expenses_by_age))

#Age in 2023
inforce.nolapse <- subset(inforce.alive, is.na(X.13))
inforce.nolapse$X.1 <- as.numeric(inforce.nolapse$X.1)
inforce.nolapse$X.2 <- as.numeric(inforce.nolapse$X.2)


inforce.nolapse$Age.at.2023 <- c(rep(NA,3), inforce.nolapse$X.2[-(1:3)] + 2023 - inforce.nolapse$X.1[-(1:3)])
na.omit(inforce.nolapse$Age.at.2023)
age_distribution <- table(inforce.nolapse$Age.at.2023)
(percentage_distribution <- prop.table(age_distribution))
(age_distribution_df <- data.frame(Age = as.numeric(names(age_distribution)),
  Frequency = as.numeric(age_distribution),
  Percentage = as.numeric(percentage_distribution)))
# Define the file path
file_path <- "Age.distribution.2023.csv"

# Export the data frame to a CSV file
write.csv(age_distribution_df, file_path, row.names = FALSE)
