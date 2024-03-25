# R-Programming
##Markdown of plots for Streptomycin project
---
title: "R poster"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


```{r}
library(broom)
library(knitr)
library(logistf)
library(medicaldata)
library(ggplot2)
```



#descriptive Stats
```{r}
# Load data
tb <- medicaldata::strep_tb
# Summary of the dataset
summary(tb)
```


```{r}
# Histogram for rad_num
ggplot(tb, aes(x = rad_num)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of rad_num",
       x = "rad_num",
       y = "Frequency")
```

```{r}
# Bar plot for arm
ggplot(tb, aes(x = arm)) +
  geom_bar(fill = "darkgrey") +
  labs(title = "Distribution of Arm",
       x = "Arm",
       y = "Count")

# Bar plot for gender
ggplot(tb, aes(x = gender)) +
  geom_bar(fill = "lightblue") +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count")



```


```{r}
# Boxplot for rad_num by improved
ggplot(tb, aes(x = improved, y = rad_num)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of rad_num by Improved",
       x = "Improved",
       y = "rad_num")
```
```{r}
# 
tb$baseline_condition <- factor(tb$baseline_condition)
tb$improved <- factor(tb$improved)

#  contingency table
contingency_table <- table(tb$baseline_condition, tb$improved)

# Display the contingency table
contingency_table

```

```{r}
# Chi-square test
chi_square_result <- chisq.test(contingency_table)
#  chi-square test result
chi_square_result
```

```{r}
# stacked bar plot
ggplot(tb, aes(x = baseline_condition, fill = improved)) +
  geom_bar() +
  labs(title = "Treatment Effectiveness by Baseline Condition",
       x = "Baseline Condition",
       y = "Count",
       fill = "Improved")

```

```{r}
library(ggplot2)


ggplot(tb, aes(x = radiologic_6m, y = rad_num, fill = rad_num)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Effect of Radiologic Assessment Number on Radiologic Assessment (6months) ",
       x = "Radiologic Assessment",
       y = "Radiologic Assessment (6months)",
       fill = "Radiologic Assessment Number") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 56, hjust = 1))



```

```{r}
library(ggplot2)

# Create a box plot
ggplot(tb, aes(x = arm, y = rad_num)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Radiology Examinations in 6 Months by Treatment Arm",
       x = "Treatment Arm",
       y = "Number of Radiology Examinations")

```

# Analysis Q1,Q2,Q3,Q4

```{r}

tb$radiologic_6m <- as.numeric(tb$radiologic_6m)

# Scatter plot
plot(tb$rad_num, tb$radiologic_6m, 
     xlab = "Frequency of radiologic examinations", ylab = "progression of tuberculosis(6m) ",
     main = "Frequency of radiologic examinations vs  progression of tuberculosis")

```



```{r}


model_h <- glm(improved ~ dose_strep_g + baseline_condition, data = tb, family = "binomial")
h_summary <- tidy(model_h)

# Print table
kable(h_summary, caption = "Logistic regression to assess the relationship between Streptomycin treatment and patient improvement,", format = "markdown")
```


```{r}
# Correlation analysis
correlation_matrix <- cor(tb$rad_num, tb$radiologic_6m, method = "spearman")

# Print Correlation Matrix table
kable(correlation_matrix, caption = "Relationship between the number of radiologic examinations and the radiologic findings at 6 months", format = "markdown")
```


```{r}
# Logistic regression
model_ha <- glm(strep_resistance ~ dose_strep_g, data = tb, family = "binomial")
ha_summary <- tidy(model_ha)

# Print 
kable(ha_summary, caption = "Summary of Logistic (Initial dose of Streptomycin vs development of drug resistance at 6 months.)", format = "markdown")
```


```{r}
#binary variable for Death
tb$Death <- as.numeric(tb$radiologic_6m == 1)

# Logistic regression
model_y_firth <- logistf(Death ~ baseline_cavitation + dose_strep_g, data = tb)
y_summary <- summary(model_y_firth)

# Print table
kable(y_summary$coefficients, caption = "Summary of Logistic regression(presence of lung cavitation at baseline vs mortality at 6 months, controlling for Streptomycin treatment. ", format = "markdown")



```






