install.packages("openxlsx")
library(openxlsx)
install.packages("corrplot")
library(corrplot)
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("DT")
library(DT)
install.packages("datarium")
library(datarium)
install.packages("carData")
library(qqplotr)

### In this script CCS is abbreviated as concrete compressive strength
## Reading the file
CCS_data<- read.xlsx("concrete compressive strength.xlsx")

# Display the column names
colnames(CCS_data)

# Replacing the columns name
new_names <- c("Cement", "Blast.Furnace", "Fly.Ash", "Water", "Superplasticizer", "Coarse.Aggregate","Fine.Aggregate","Age","Concrete.Category","Contains.Fly.Ash","CCS")

# Replace column names by index
colnames(CCS_data)[1:11] <- new_names 


## Displaying first few rows of Data in table form
datatable(head(CCS_data))
datatable(tail(CCS_data))


## Displaying the number of rows and columns in data
dim(CCS_data)

## Displaying the summary of the data. summary() gives you the minimum, maximum, mean, median, and quartiles for numeric variables, as well as frequency counts for factors
summary(CCS_data)

# Check for missing values in each column
colSums(is.na(CCS_data))

# Check the normality of data
# Null hypothesis is that our data is normally distributed
#shapiro.test(CCS_data$CCS)


# Plot histogram to check the normality of data
# Create a histogram with a normal curve
hist(CCS_data$CCS, 
     breaks = 20, 
     freq = FALSE, 
     main = "Histogram with Normal Curve",
     xlab = "Compressive Strength (MPa)",
     col = "lightblue", 
     border = "black")

# Add the normal curve
curve(dnorm(x, mean = mean(CCS_data$CCS), sd = sd(CCS_data$CCS)), 
      add = TRUE, col = "red", lwd = 2)


# Density Plot with Normal Curve
density_data <- density(CCS_data$CCS)
plot(density_data, 
     main = "Density Plot with Normal Curve", 
     xlab = "Compressive Strength (MPa)", 
     col = "blue", 
     lwd = 2)
curve(dnorm(x, mean = mean(CCS_data$CCS), 
            sd = sd(CCS_data$CCS)), 
      add = TRUE, col = "red", lwd = 2)


# Select only numerical columns
numerical_CCS <- CCS_data[, sapply(CCS_data, is.numeric)]

# Create box plots for all numerical columns
boxplot(numerical_CCS, main = "Box Plots for All Numerical Columns", 
        col = "lightblue", las = 2,  ylab = "Value")


## Histogram for numerical columns

# Set up a grid layout
par(mfrow = c(ceiling(sqrt(length(names(numerical_CCS)))), 
              ceiling(sqrt(length(names(numerical_CCS))))))

# Loop through each numerical column and plot histogram
for (col_name in names(numerical_CCS)) {
  hist(numerical_CCS[[col_name]],  
       main = paste("Histogram of", col_name), 
       xlab = col_name, 
       col = "skyblue", 
       border = "black")
}

# Count distinct values for age column
distinct_count <- length(unique(CCS_data$Age))
print(distinct_count)

table(CCS_data$Age)


## Correlation Matrix
par(mfrow = c(1, 1))
corrplot(cor(numerical_CCS), method="number", type="upper")



## Scatter plot

pairs(numerical_CCS[,c(9,1,2,3,4,5,6,7,8)], upper.panel = NULL, pch = 19
      ,cex = 0.5)



#pairs(numerical_CCS[,c(9,6,7)], lower.panel = NULL, pch = 19,cex = 0.2)


## Regression

model_1 <-lm(CCS~ Cement+ Blast.Furnace+ Fly.Ash + Water+ Superplasticizer+ 
               Coarse.Aggregate + Fine.Aggregate+Age,numerical_CCS)
summary.lm(model_1)




model_2 <-lm(CCS~ Cement+ Blast.Furnace+ Fly.Ash + Water+ Superplasticizer+Age,
             numerical_CCS)
summary.lm(model_2)


model_3 <-lm(CCS~ Cement+ Blast.Furnace+ Water+ Superplasticizer+Age,
             numerical_CCS)
summary.lm(model_3)


plot(model_3, 1)
plot(model_3, 2)
plot(model_3, 3)


## Linearity Check of independent variable

pairs(numerical_CCS[, c(9, 1, 2, 4, 5,8)], 
      lower.panel = function(x, y) {
        points(x, y, pch = 19, cex = 0.2)  # Add scatterplot points
        abline(lm(y ~ x), col = "red")    # Add regression line
      },
      upper.panel = NULL,  # Leave upper panel blank
      main = "Scatterplot Matrix with In-line Regression Lines")

## Q-Q plot to check if the variable follow a  normal distribution


#qqnorm(numerical_CCS$Cement, main = "Q-Q Plot of Cement")
#qqline(numerical_CCS$Cement, col = "red", lwd = 2)

#qqnorm(numerical_CCS$Blast.Furnace, main = "Q-Q Plot of Blast.Furnace")
#qqline(numerical_CCS$Blast.Furnace, col = "red", lwd = 2)

#qqnorm(numerical_CCS$Superplasticizer, main = "Q-Q Plot of Superplasticizer")
#qqline(numerical_CCS$Superplasticizer, col = "red", lwd = 2)

#qqnorm(numerical_CCS$Water, main = "Q-Q Plot of Water")
#qqline(numerical_CCS$Water, col = "red", lwd = 2)

#qqnorm(numerical_CCS$Age, main = "Q-Q Plot of Age")
#qqline(numerical_CCS$Age, col = "red", lwd = 2)

# Apply log transformation on Superplasticizer

# Filter out non-positive values
#cleaned_Superplasticizer <- numerical_CCS$Superplasticizer[numerical_CCS$Superplasticizer > 0]

# Apply log transformation
#log_Superplasticizer <- log(cleaned_Superplasticizer)
# Create Q-Q plot
#qqnorm(log_Superplasticizer, main = "Q-Q Plot of Log-Transformed Superplasticizer")
#qqline(log_Superplasticizer, col = "red", lwd = 2)

# Apply log function on Age

# Check for non-positive values in the Age column

# Remove non-positive values
#cleaned_age <- numerical_CCS$Age[numerical_CCS$Age > 0]

## Apply log transformation
#log_age <- log(cleaned_age)

# Create the Q-Q plot
#qqnorm(log_age, main = "Q-Q Plot of Log-Transformed Age")
#qqline(log_age, col = "red", lwd = 2)  # Add a reference line




## Perform log transformation on Final model 3

model_3_log_1 <-lm(CCS~ Cement+ log(Blast.Furnace+1)
                   + log(Superplasticizer+1)+log(Water)+log(Age),numerical_CCS)
summary.lm(model_3_log_1)


# Checking the normality assumption of residuals after log transformation

plot(model_3_log_1, 1)
plot(model_3_log_1, 2)
plot(model_3_log_1, 3)


library(car)

vif(model_3)
vif(model_3_log_1)



#ggplot(CCS, aes(sample = Cement)) +
 # stat_qq_point(size = 2,color = "blue") +
  #stat_qq_line(color="orange") +
  #xlab("Theoretical") + ylab("Cement")

#shapiro.test(CCS$Cement)
#before log
#hist(CCS$Cement, main = "Histogram of Skewed 'y'", xlab = "y", col =
       "lightblue", border = "black")
#log function
#CCS$log_Cement <- log(CCS$Cement)

#after log
#hist(CCS$log_Cement, main = "Histogram of Log-transformed 'y'", xlab = "log(y)",
     col = "lightblue", border = "black")

## Hypothesis 

#Null Hypothesis (H₀):
#There is no relationship between the amount of Cement and the Concrete Compressive Strength.
#Alternative Hypothesis (H₁):
# There is a relationship between the amount of Cement and the Concrete Compressive Strength.

#Hypothesis 1
#both are continuous variables, therefore need to run linear Regression

model <-lm(CCS ~ Cement, data = CCS)
summary.lm(model)

#Hypothesis 2

# create contingency_table
contingency_table<- table(CCS$Concrete.Category,CCS$Contains.Fly.Ash)
print(contingency_table)

# Perform the Chi-Square Test of Independence
chisq.test(contingency_table)

ggplot(CCS) +
  aes(x = Contains.Fly.Ash, fill = Concrete.Category) +
  geom_bar() +
  labs(title ="Count of  Concrete.Category w.r.t Fly Ash",
       x = "Fly.Ash", y = "Count Concrete.Category")
#we are keeping our H0 pvalue=1


#Hypothesis 3
#independent two sample t-test

t.test(CCS ~  Concrete.Category, data=CCS)

#independent two sample t-test
# Use box plot to compare coarse and fine
#in category we skip the normality distribution 
boxplot(CCS ~  Concrete.Category, data=CCS, names=c("Coarse", "Fine"),
        xlab="Concrete.Category", ylab="Concrete.Compressive.Strength",
        main="Concrete.Compressive.Strength vs Concrete Category")







#we are not rejecting H0














