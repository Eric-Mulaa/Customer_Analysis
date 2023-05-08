
customers1 <- read.table(file = "Customers.csv", header = TRUE, sep = ",")
head(customers1)
str(customers1)
summary(customers1)

# there are no NAs in this data but some values don't make sense, like age of zero
library(tidyverse)

customers <- customers1 %>%
  filter(customers1$Age != 0)
summary(customers)
# there is a person with age of 1 year, lets see those with age less than 10
young <- customers %>%
  filter(customers$Age <= 10)
summary(young)

# There a person at the age of 1 and a profession of a doctor and an engineer
# This is not consistent, so for this case I will work with age 18 and above 
# There are some values in Profession column that are empty too

customers <- customers %>%
  filter(Age >= 18, customers$Profession != "")
  
# I will then proceed to remove any incomplete cases
customers <- customers[complete.cases(customers), ]
summary(customers)

# I will convert Gender and Profession into factors
customers$Gender <- as.factor(customers$Gender)
customers$Profession <- as.factor(customers$Profession)

library(ggplot2)
install.packages("GGally")
library(GGally)

GGally::ggpairs(customers, columns = c(3:5, 7:8), aes(color = Gender))
## we can observe that Age and Annual income is positively correlated, but it is greater for male than for female
##2. Age is also positively correlated to family size, with the correlation for female being significant while than of male is not.
##3. Annual income is positively correlated to work experience and family size
##4. The correlation for Annual income and work experience is more significant for male than for female
##5. The correlation for Annual income and family size is more sgnificant for female than for male

head(customers)
str(customers)
plot(customers$Profession, col = "blue")


#ggplot(customers2, aes(reorder(Profession,-Spending.Score..1.100.), Spending.Score..1.100.), fill = "Gender")+ 
  #geom_bar(stat = "identity", position = "dodge")+ 
  #labs(title = "Spending Score for different Professions")
#I will create a model for this data.

set.seed(1234)
indexset <- sample(2, nrow(customers), replace = T, prob = c(0.7, 0.3))
train <- customers[indexset == 1,]
test <- customers[indexset == 2,]   

mymodel <- lm(Spending.Score..1.100.~Age + Annual.Income.... + 
                 Work.Experience + Family.Size, data = train)
summary(mymodel)

#we can use our trained model to make predictions on our testing data
train <- train %>%
  mutate(yhats = predict(mymodel, newdata = train))
head(train)

#The residuals for predicted values as well
train <- train %>%
  mutate(residuals = Spending.Score..1.100. - yhats)
head(train)

# Now our data has more variables and we can now perform 'cross-validation correlation'
# The correlation between predictions and the actual values.
train %>%
  summarise(cor = cor(Spending.Score..1.100., yhats))

# If we square this value it will give us the R-squared
train %>%
  summarise(cor = cor(Spending.Score..1.100., yhats)) %>%
  mutate(R2 = cor**2)
# Now lets quantify the difference between the R2 of the train data
# and the square cross validation
train %>%
  summarise(cor = cor(Spending.Score..1.100., yhats)) %>%
  mutate(R2 = cor**2, shrinkage = summary(mymodel)$r.squared - R2)
# we want smaller shrinkage values. The book says 'we won't give any hard
# and fast rules' for shrinkage values, but 'shrinkage of 10% or less should 
# not be a problem, but a shrinkage of more than 50% would be worrisome' 

# Lets look at the residual plots of our testing data
ggplot(train, aes(x = yhats, y = residuals)) + 
  geom_point() + 
  geom_smooth(se = FALSE)
#The mean of the residuals should be zero

train %>%
  summarise(mean(residuals), sd(residuals))

sd(residuals(mymodel))
# We expect the mean of residuals to be close to zero, and it is, and the 
# the standard deviation of residuals to be close to the standard deviation
# of the error term from the fir to the training sample.
## Those look pretty good!



