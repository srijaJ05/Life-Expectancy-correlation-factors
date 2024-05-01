####################### START OF CODES ##########################



# Reading the life expectancy data setinto R
setwd("C:/Users/Irene/Documents/RAnalytics")
getwd()
LifeExp <- read.csv(file = "Life Expectancy Data.csv", header = TRUE, sep = ",")
LifeExp

# View first few rows and column information
head(LifeExp)
str(LifeExp)

# Check for missing values in each column
colSums(is.na(LifeExp))

#List of numeric columns
numeric_cols <- c("Life.expectancy", "Adult.Mortality", "Alcohol", "Hepatitis.B", "BMI", "Polio", "Total.expenditure",
                  "Diphtheria", "HIV.AIDS", "GDP", "Population", "thinness..1.19.years", "thinness.5.9.years",
                  "Income.composition.of.resources", "Schooling")

# Replace missing values in numeric columns with mean
for (col in numeric_cols) {
  LifeExp[[col]][is.na(LifeExp[[col]])] <- mean(LifeExp[[col]], na.rm = TRUE)}

# Check for missing values in numeric columns
colSums(is.na(LifeExp[numeric_cols]))

# List of categorical columns
categorical_cols <- c("Country", "Status")

# Replace missing values in categorical columns with mode
for (col in categorical_cols) {
  # Calculate mode for each column
  mode_val <- names(sort(table(LifeExp[[col]]), decreasing = TRUE))[1]
  LifeExp[[col]][is.na(LifeExp[[col]])] <- ifelse(is.na(LifeExp[[col]]), mode_val, LifeExp[[col]])}

# Check for missing values in categorical columns
colSums(is.na(LifeExp[categorical_cols]))

# Summary statistics for numeric columns
summary(LifeExp[numeric_cols])

# Frequency of categorical values
table(LifeExp[categorical_cols])

# Removing the 'BMI' column from the dataset using subset function
LifeExp <- subset(LifeExp, select = -c(BMI))


# Load necessary libraries
library(countrycode)
library(dplyr)
library(ggplot2)











####################### Hypothesis 1 Srija Jasthi ##########################

# Adding a new column 'Continent' to the 'LifeExp' dataset based on country names
LifeExp$Continent <- countrycode(LifeExp$Country, "country.name", "continent")

# Check unique values in the 'Continent' column
unique_continents <- unique(LifeExp$Continent)
print(unique_continents)

# Applying mean and standard deviation to all columns
grouped_data <- LifeExp %>%
  group_by(Continent, Status) %>%
  reframe(across(where(is.numeric),  ~ list(mean = mean(., na.rm = TRUE), 
                                            sd = sd(., na.rm = TRUE))))

# Identify numeric columns
numeric_cols <- sapply(LifeExp, is.numeric)

# Count the number of countries in each continent
continent_counts <- table(LifeExp$Continent)

# Display the counts
print(continent_counts)

# Group by Continent and Status, then count occurrences
count_status <- LifeExp %>%
  group_by(Continent, Status) %>%
  summarise(Count = n())

# View the count of developing and developed countries in each continent
print(count_status)

#checking for NA or infinite values in the Life.expectancy column
summary(LifeExp$Life.expectancy)
sum(is.na(LifeExp$Life.expectancy))
sum(!is.finite(LifeExp$Life.expectancy))

# Plotting the count of developing and developed countries in each continent 
ggplot(count_status, aes(x = Continent, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Count), position = position_dodge(width = 0.7), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Count of Developing vs Developed Countries by Continent",x = "Continent", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box Plot for Life Expectancy across continents
Lifeexp.by_continents <- ggplot(LifeExp %>% filter(!is.na(Life.expectancy) & is.finite(Life.expectancy)),
                                aes(x = Continent, y = Life.expectancy, fill = Continent)) + geom_boxplot() +
  labs(title = "Life Expectancy Across Continents", x = "Continent", y = "Life Expectancy") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(Lifeexp.by_continents)

#####second layer suppoorting the hypothesis using hepatitis B and Income compostion of resources

#Scatter plot of Life Expectancy vs Hepatitis B 
ggplot(LifeExp, aes(x = Hepatitis.B, y = Life.expectancy, color = Status)) +
  geom_point() +
  labs(title = "Life Expectancy vs Hepatitis B Vaccination Rate",
       x = "Hepatitis B Vaccination Rate (%)",
       y = "Life Expectancy") +
  theme_minimal()


# Scatter plot of Life Expectancy vs Income Composition of Resources (HDI)
ggplot(LifeExp, aes(x = Income.composition.of.resources, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add a linear regression line without a confidence interval
  labs(title = "Life Expectancy vs Income Composition of Resources (HDI)",
       x = "Income Composition of Resources (HDI)",
       y = "Life Expectancy") +
  theme_minimal() +
  scale_color_manual(values = c("Developed" = "red", "Developing" = "blue"))










####################### Hypothesis 2 by Braum Russell ##########################


#histogram representing alc consumption in developed countries
hist(LifeExp$Alcohol[LifeExp$Status == "Developed"], 
     main = "Alcohol Consumption in Developed Countries",
     xlab = "Alcohol Consumption", ylab = "Frequency", col = "red")


#histogram representing alc consumption in developing countries
hist(LifeExp$Alcohol[LifeExp$Status == "Developing"], main = 
       "Alcohol Consumption in Developing Countries", 
     xlab = "Alcohol Consumption", ylab = "Frequency", col = "red")


#Median Alcohol Consumption Per Capita for Developing
developing.data <- LifeExp$Alcohol[LifeExp$Status == "Developing"]
AlcDeveloping.median <- median(developing.data)


#Median Alcohol Consumption Per Capita for Developed
developed.data <- LifeExp$Alcohol[LifeExp$Status == "Developed"]
AlcDeveloped.median <- median(developed.data)



#Table of the two consumption rates
median_table <- data.frame(Status = c("Developed", "Developing"), MedianConsumption_Rate = c(LifeExpDeveloped.median, LifeExpDeveloping.median))


#Median Life Expectancy for Developing countries
developingexp.data <- LifeExp$Life.expectancy[LifeExp$Status == "Developing"]
LifeExpDeveloping.median <- median(developingexp.data)

#Median Life Expectancy for Developed countries
developedexp.data <- LifeExp$Life.expectancy[LifeExp$Status == "Developed"]
LifeExpDeveloped.median <- median(developedexp.data)


#Plot of Median life expectancies
barplot(c(LifeExpDeveloping.median, LifeExpDeveloped.median), ylim = c(0,85), 
        xlab="Developing Developed", ylab = "Years", main = "Median Life Expectancy: 
Developing vs Developed", col = "darkred")





####################### Hypothesis 3 by Irene Georgestone ##########################


# List of West African countries
west_african_countries <- c("Benin", "Burkina Faso", "Cabo Verde", "CÃ´te d'Ivoire", 
                            "Gambia", "Ghana", "Guinea", "Guinea-Bissau", 
                            "Liberia", "Mali", "Mauritania", "Niger", 
                            "Nigeria", "Senegal", "Sierra Leone", "Togo")



# Installing and loading ggplot packages and libraries
install.packages("dplyr")
install.packages("ggplot2")
install.packages("crayon")

library("dplyr")
library("ggplot2")

west_african_data <- LifeExp %>%
  filter(Country %in% west_african_countries)


# Filter the data to specify for west African countries and years 2000 - 2015
filtered_WAdata <- subset(LifeExp, Country %in% west_african_countries
                          & Year >= 2000 & Year <= 2015)



#AVERAGE FOR ALL WEST AFRICAN COUNTRIES
# Calculating average life expectancy for each year
avg_life_expectancy_by_year <- filtered_WAdata %>%
  group_by(Year) %>%
  summarize(AverageLifeExpectancy = mean(Life.expectancy))

# Creating the plot
ggplot(avg_life_expectancy_by_year, aes(x = Year, y = AverageLifeExpectancy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  ggtitle("Average Life Expectancy Trends in West African Countries (2000-2015)") +
  xlab("Year") +
  ylab("Average Life Expectancy")



# Filter the data to include only years 2000 - 2015
measles_data <- filtered_WAdata %>%
  filter(Year >= 2000 & Year <= 2015)

# Create a new data frame with average measles count for each country and each year
measles_heatmap_data <- measles_data %>%
  group_by(Country, Year) %>%
  summarize(AverageMeasles = mean(Measles, na.rm = TRUE))

# Create the heatmap
ggplot(measles_heatmap_data, aes(x = Year, y = Country, fill = AverageMeasles)) +
  geom_tile() +
  scale_fill_gradient(low = "light blue", high = "red") +
  labs(title = "Heatmap of Measles Count (2000 - 2015)",
       x = "Year",
       y = "Country",
       fill = "Measles\nCount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Now, create a new data frame that summarizes the average total expenditure for each country and each year
expenditure_heatmap_data <- west_african_data %>%
  group_by(Country, Year) %>%
  summarize(AverageExpenditure = mean(Total.expenditure, na.rm = TRUE)) %>%
  ungroup()  # Ungroup for plotting

# Creating the heatmap
ggplot(expenditure_heatmap_data, aes(x = Year, y = Country, fill = AverageExpenditure)) +
  geom_tile() +
  scale_fill_gradient(low = "light yellow", high = "darkblue") +
  labs(title = "Heatmap of Total Health Expenditure (2000 - 2015)",
       x = "Year",
       y = "Country",
       fill = "Total\nExpenditure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for readability
        axis.title.x = element_text(vjust = -2)) 




#Expenditure VS Life Expectancy
# Creating the plot
ggplot(filtered_WAdata, aes(x = Total.expenditure, y = Life.expectancy)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = FALSE) +  # To add linear regression line
  labs(title = "Relationship Life Expectancy and Total Expenditure (% of GDP) in West African Countries",
       x = "Total Expenditure (% of GDP)",
       y = "Life Expectancy")


# Create the scatter plot with a trend line of measles counts vs life expectancy
ggplot(filtered_WAdata, aes(x = Measles, y = Life.expectancy)) +
  geom_point() +  # Add scatter points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line without standard error
  labs(title = "Relationship between Life Expectancy and Measles Cases in West African Countries",
       x = "Measles Count",
       y = "Life Expectancy")



#DO STATSITICAL MODELLING TO GET THE SIGNFICANCE OF THE VARIABLES ON LIFE EXPECTANCY


# Load the necessary library
library(stats)

# Fit a linear model
model <- lm(Life.expectancy ~ Total.expenditure 
            + Measles, data = filtered_WAdata)

# Summarize the model to see coefficients
summary(model)

modelExpend <- lm(Life.expectancy ~ Total.expenditure,
                  data = filtered_WAdata)

modelMeasles <- lm(Life.expectancy ~ Measles, 
                   data = filtered_WAdata)








####################### Hypothesis 4 by Benjamin Finley ##########################
# Any necessary libraries should already be installed from previous hypotheses

# Filter separate data frames by status
LifeExp.Developed <- LifeExp %>% filter(Status == "Developed")
LifeExp.Developing <- LifeExp %>% filter(Status == "Developing")




# Box Plot for Life Expectancy of Developed and Developing Countries
# To establish a base for life expectancy
box.LE <- ggplot(LifeExp, aes(x = Status, y = Life.expectancy, fill = Status)) +
  geom_boxplot() +
  labs(title = "Life Expectancy - Developed vs. Developing",
       x = "Country Type",
       y = "Life Expectancy")
box.LE


############  Plots to compare Life Expectancy, Schooling, and GDP  ##########


# Scatter plot for Schooling Years vs. Life Expectancy, Both Developed and Developing
school.plot <- ggplot(LifeExp, aes(x = Schooling, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(title = "Schooling Years vs. Life Expectancy - Developed vs. Developing",
       x = "Schooling Years",
       y = "Life Expectancy")
school.plot

# Get correlation coefficient 
cor(LifeExp$Schooling, LifeExp$Life.expectancy)



# Schooling years in developed
school.plot.developed <- ggplot(LifeExp.Developed, aes(x = Schooling, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  
  labs(title = "Schooling Years vs. Life Expectancy - Developed",
       x = "Schooling Years",
       y = "Life Expectancy")
school.plot.developed # looks like little correlations

# Get correlation coefficient
cor(LifeExp.Developed$Schooling, LifeExp.Developed$Life.expectancy)



# Schooling years in developing
school.plot.developing <- ggplot(LifeExp.Developing, aes(x = Schooling, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Schooling Years vs. Life Expectancy - Developing",
       x = "Schooling Years",
       y = "Life Expectancy")
school.plot.developing # clear positive correlation

# Get correlation coefficient
cor(LifeExp.Developing$Schooling, LifeExp.Developing$Life.expectancy)



# Scatter plot for GDP vs. Life Expectancy, Both Developed and Developing
gdp.plot <- ggplot(LifeExp, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "GDP vs. Life Expectancy - Developed vs. Developing",
       x = "GDP",
       y = "Life Expectancy")
gdp.plot

# Get correlation coefficient
cor(LifeExp$GDP, LifeExp$Life.expectancy)



# GDP for developed
gdp.plot.developed <- ggplot(LifeExp.Developed, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "GDP vs. Life Expectancy - Developed",
       x = "GDP",
       y = "Life Expectancy")
gdp.plot.developed

# Get correlation coefficient
cor(LifeExp.Developed$GDP, LifeExp.Developed$Life.expectancy)



# GDP for developing
gdp.plot.developing <- ggplot(LifeExp.Developing, aes(x = GDP, y = Life.expectancy, color = Status)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "GDP vs. Life Expectancy - Developing",
       x = "GDP",
       y = "Life Expectancy")
gdp.plot.developing

# Get correlation coefficient
cor(LifeExp.Developing$GDP, LifeExp.Developing$Life.expectancy)


##### Linear Regression for Developed and Developing countries #####

model.developed <- lm(LifeExp.Developed$Life.expectancy ~ LifeExp.Developed$GDP + LifeExp.Developed$Schooling, data = LifeExp.Developed)
summary(model.developed)

model.developing <- lm(LifeExp.Developing$Life.expectancy ~ LifeExp.Developing$GDP + LifeExp.Developing$Schooling, data = LifeExp.Developing)
summary(model.developing)





####################### END OF CODES ##########################


