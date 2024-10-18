# ---
title: "Itom 6253 Programming for Analytics Fall A 2024 - Assignment1 Solution"
output:
  html_document: default
  pdf_document: default
  date: "Fall A 2024"
---
<!-- name of chunk = set_global_options and include chunk in doc -->
```{r set_global_options, echo=TRUE}
 knitr::opts_chunk$set(echo = TRUE)
```
### Notes on homework #1

The raw data file for this assignment is full of problems. There are missing values, outliers and invalid values (e.g., negative revenue). There are extra commas at the end of each line that cause the creation of empty nonsense variables and a few lines are missing a comma to separate the last two variables. Many lines also have zero purchases for all products.

Before doing any analytics, we must first scrub the raw data to solve these problems. We make use of the **dplyr** and **tidyr** packages for many of the scrubbing tasks.

```{r echo=TRUE}
require(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

cbc <- as.data.frame(read_excel(file.choose()))
cbc <- cbc[,3:ncol(cbc)]
str(cbc)
```


```{r echo=TRUE}
cbc$M <- as.numeric(cbc$M)
cbc$R <- as.numeric(cbc$R)
summary(cbc)
str(cbc)

```
Note that there is a negative value in M !!

#### Count the unique values of each variable.
```{r}
cbc_counts <- cbc %>% summarise_all(funs(n_distinct(.)))
cbc_counts
```

#### Convert Gender and Florence to Factors
```{r}
cbc$Gender <- factor(cbc$Gender, labels = c("Female", "Male"))
cbc$Florence <- factor(cbc$Florence, labels = c("No", "Yes"))
```

#### Identify the numeric columns for which outlier detection is desired
```{r}
outvars <- c("M", "R", "F", "FirstPurch")
```


#### Find outliers and set them to missing
Note the use of the *anonymous* function in the following code:
```{r}

cbc[outvars] <- data.frame(lapply(cbc[outvars], function(x) {
  ifelse((x < 0) | x > (mean(x, na.rm = TRUE) + 3*sd(x, na.rm = TRUE)), NA, x) }))

```

#### Summary also counts the number of missing values
```{r}

summary(cbc[outvars])

```
#### Identify variables for which imputation of missing values is desired
```{r}
missvars <- c("M", "R", "F", "FirstPurch")
```

#### Impute missing values of columns with missing values
Here's another *anonymous* function use:
```{r}
cbc[missvars] <- data.frame(lapply(cbc[missvars], function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x) }))

summary(cbc)
```

#### Delete rows for which there are no books purchased.
```{r}
cbc_no_zeroes <- cbc[rowSums(cbc[,6:15]) != 0 | cbc$Florence =="Yes", ]
summary(cbc_no_zeroes)
nrow(cbc)
nrow(cbc_no_zeroes)
```

#### Sum the purchases of each book type.
```{r}
cbc_sums <- cbc_no_zeroes %>% summarise(across(c(6:15), sum))
cbc_sums
```
### Histogram plot of numeric variables

```{r}
library(psych)
histvars <- c("M", "R", "F", "FirstPurch")
multi.hist(cbc_no_zeroes[outvars],nrow=2,ncol=2, global =     FALSE, main=c("Monetary Value", "Recency", "Frequency", "First Purchase"))
```

### Bar plot of book type sums

```{r}
cbc_pivot <- pivot_longer(cbc_sums, cols=c(1:10))
names(cbc_pivot) <- c("Type", "Sum")
```


```{r}
ggplot(data=cbc_pivot, aes(y=Type, x=Sum)) +
  geom_bar(stat = 'identity')
```
  
### A custom function for calculating 4 moments

```{r}
library(e1071)
#browser()
calcfourstats <- function(x) {
  mu <- round(mean(x, na.rm = TRUE), 2)
  sigma <- round(sd(x, na.rm = TRUE), 2)
  skew <- round(skewness(x, na.rm = TRUE), 3)
  kurt <- round(kurtosis(x, na.rm = TRUE), 2)
  result <- data.frame(mu, sigma, skew, kurt)
                      
  #return(result)
  
}

results <- calcfourstats(cbc_no_zeroes[, 2])
results <- rbind(results, calcfourstats(cbc_no_zeroes[, 3]))
#browser()
results <- rbind(results, calcfourstats(cbc_no_zeroes[, 4]))
results <- rbind(results, calcfourstats(cbc_no_zeroes[, 5]))
varList <- names(cbc_no_zeroes[2:5])
print(varList)
rownames(results) <- varList
print(results)
```

### Creating RFM factors
#### Calculate HML cutoffs for RFM
```{r}
cbc_rfm <- data.frame(lapply(cbc_no_zeroes[c("R", "F", "M")], 
  function(x) {
    quantile(x, probs = c(.33, .66), na.rm = TRUE) }))
```

Verify results and test subsetting    
```{r}
cbc_rfm
cbc_rfm["33%", "M"] #What is the 33rd percentile of M?
```

Create three new variables for HML quantiles of RFM variables
```{r}
library(dplyr)
cbcRFM <- cbc_no_zeroes %>%
  mutate(rRFM = if_else(R <= cbc_rfm["33%", "R"], "L",
                        if_else(R >= cbc_rfm["66%", "R"], "H", "M"))) %>%
  mutate(fRFM = if_else(F <= cbc_rfm["33%", "F"], "L",
                        if_else(F >= cbc_rfm["66%", "F"], "H", "M"))) %>%
  mutate(mRFM = if_else(M <= cbc_rfm["33%", "M"], "L",
                        if_else(M >= cbc_rfm["66%", "M"], "H", "M")))
```
Convert the new HML variables into ordered factors
```{r}
cbcRFM[c("rRFM", "fRFM", "mRFM")] <- data.frame(lapply(cbcRFM[c("rRFM", "fRFM", "mRFM")], 
  function(x) {
    factor(x, c("L", "M", "H"), ordered = TRUE)
  }))

head(cbcRFM)
str(cbcRFM)
```


```{r}
sumTable <- cbcRFM %>% 
  group_by(rRFM, fRFM, mRFM) %>%
  summarise(meanM = round(mean(M), 2))


sumTable
```

#### Make three tables, one for each level of factor mRFM

```{r, echo=TRUE, message=FALSE, warning=FALSE}
for (i in c("L", "M", "H")) {
  shortTable <- xtabs(meanM ~ rRFM + fRFM, sumTable %>% filter(mRFM == i)) 
    print(paste('Monetary Value Segment =', i))
    print(shortTable)
    cat("\n") # Add a blank line between tables
    
} 
  
```




### Median monetary value per visit by gender

```{r}
visitValue <- cbcRFM %>%
  group_by(factor(Gender, labels = c("Female", "Male"))) %>%
  summarise(medianM = round(median(M / F), 2))

visitValue
```
```{r}
ggplot(cbcRFM, aes(x = R, y = M, col = factor(Gender, labels = c("Female", "Male")), size = FirstPurch)) +
  geom_point(alpha = .20) +
  
  labs(x = "Recency", y = "Monetary Value") +
  facet_wrap(~ factor(Gender, labels = c("Female", "Male")), labeller = label_parsed) +
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.key = element_rect(colour = 'white', fill = 'white'))
```

