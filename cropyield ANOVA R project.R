# Install (run once)
install.packages("tidyverse")
install.packages("car")

# Load libraries
library(tidyverse)
library(car)
library(ggplot2)

# Load data
data <- read.csv("C:/Users/singh/OneDrive/Desktop/MCAII/FAOSTAT_data_en_4-9-2026.csv")

#checking elements
nrow(data)
names(data)
table(data$Element)

head(data)

# Explore
head(data)
str(data)
summary(data)



#clean spaces
data <- data[trimws(data$Element) == "Yield", ]

table(data$Element)
unique(data$Element)

table(data$Item)

# Filter Yield only
data <- data %>% filter(Element == "Yield")

#remove NA values
data <- na.omit(data)


# Convert to factors
data$Item <- as.factor(data$Item)
data$Area <- as.factor(data$Area)


# one way ANOVA
model <- aov(Value ~ Item, data = data)
summary(model)

# Assumption check
leveneTest(Value ~ Item, data = data)

# Post-hoc test
TukeyHSD(model)

# Normality
shapiro.test(residuals(model))



# Boxplot
ggplot(data, aes(x = Item, y = Value, fill = Item)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Crop Yield Distribution by Crop Type",
       x = "Crop Type",
       y = "Yield (kg/ha)")



#two way anova
model2 <- aov(Value ~ Item + Area + Item:Area, data = data)
summary(model2)
