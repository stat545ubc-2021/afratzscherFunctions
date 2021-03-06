
<!-- README.md is generated from README.Rmd. Please edit that file -->

# afratzscherFunctions

<!-- badges: start -->
<!-- badges: end -->

This R package provides a function `getTrainTestSplit` that splits data
into training and test sets for machine learning. Parameters can be
defined to change the train-test split ratio and to shuffle data prior
to splitting.

## Installation

The `afratzscherFunctions` package is not available on CRAN yet.

You can install `afratzscherFunctions` from Github with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2021/afratzscherFunctions")
```

## Example Usage

### 1. Using Default Parameters

The default parameters for function `getTrainTestSplit` select 70% of
the data for the training set and 30% for the test set. Data is not
shuffled prior to splitting. For the example dataframe below with 10
samples, samples 1-7 are put in the training set and samples 8-10 are
put in the test set.

``` r
library(afratzscherFunctions)

data <- data.frame(x = 1:10, y = 11:20, letter = letters[1:10]) #dataframe with instances 1-10
data
#>     x  y letter
#> 1   1 11      a
#> 2   2 12      b
#> 3   3 13      c
#> 4   4 14      d
#> 5   5 15      e
#> 6   6 16      f
#> 7   7 17      g
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j

splitData <- getTrainTestSplit(data) 

splitData$train
#>   x  y letter
#> 1 1 11      a
#> 2 2 12      b
#> 3 3 13      c
#> 4 4 14      d
#> 5 5 15      e
#> 6 6 16      f
#> 7 7 17      g
splitData$test
#>     x  y letter
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j
```

### 2. Custom Train-Test Split Ratio

It is also possible to specify the train-test split. In the example
below, we select a 50-50 split. For our example dataset, samples 1-5 are
put in the training set and samples 6-10 are put in the test set.

``` r
library(afratzscherFunctions)

data <- data.frame(x = 1:10, y = 11:20, letter = letters[1:10]) #dataframe with instances 1-10
data
#>     x  y letter
#> 1   1 11      a
#> 2   2 12      b
#> 3   3 13      c
#> 4   4 14      d
#> 5   5 15      e
#> 6   6 16      f
#> 7   7 17      g
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j

splitData <- getTrainTestSplit(data, train_size = 0.5) 

splitData$train
#>   x  y letter
#> 1 1 11      a
#> 2 2 12      b
#> 3 3 13      c
#> 4 4 14      d
#> 5 5 15      e
splitData$test
#>     x  y letter
#> 6   6 16      f
#> 7   7 17      g
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j
```

### 3. Shuffling Data Before Splitting

It is also possible to shuffle the data before splitting. One might want
to do this to prevent the training/test data from being biased. An
example is shown below:

``` r
library(afratzscherFunctions)

data <- data.frame(x = 1:10, y = 11:20, letter = letters[1:10]) #dataframe with instances 1-10
data
#>     x  y letter
#> 1   1 11      a
#> 2   2 12      b
#> 3   3 13      c
#> 4   4 14      d
#> 5   5 15      e
#> 6   6 16      f
#> 7   7 17      g
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j

splitData <- getTrainTestSplit(data, train_size = 0.5, shuffle = TRUE) 

splitData$train
#>     x  y letter
#> 10 10 20      j
#> 8   8 18      h
#> 5   5 15      e
#> 2   2 12      b
#> 1   1 11      a
splitData$test
#>   x  y letter
#> 4 4 14      d
#> 3 3 13      c
#> 9 9 19      i
#> 7 7 17      g
#> 6 6 16      f
```

If one wants to have reproducible splits (i. e. always get the same
50-50 split), one can define the `random_state` variable. Any time the
`getTrainTestSplit` function is run with the same `random_state` value,
the same split will be generated. Please note that, by defining
`random_state`, shuffling is automatically enabled. However, if you
would like, you can also define `shuffle` in the function call for
readability, although this is not necessary.

``` r
library(afratzscherFunctions)
data <- data.frame(x = 1:10, y = 11:20, letter = letters[1:10]) #dataframe with instances 1-10
data
#>     x  y letter
#> 1   1 11      a
#> 2   2 12      b
#> 3   3 13      c
#> 4   4 14      d
#> 5   5 15      e
#> 6   6 16      f
#> 7   7 17      g
#> 8   8 18      h
#> 9   9 19      i
#> 10 10 20      j

splitData <- getTrainTestSplit(data, train_size = 0.5, random_state = 123) 
# NOTE: this is the same as inputting getTrainTestSplit(data, train_size = 0.5, shuffle = TRUE, random_state = 123) 

splitData$train
#>     x  y letter
#> 3   3 13      c
#> 10 10 20      j
#> 2   2 12      b
#> 8   8 18      h
#> 6   6 16      f
splitData$test
#>   x  y letter
#> 9 9 19      i
#> 1 1 11      a
#> 7 7 17      g
#> 5 5 15      e
#> 4 4 14      d
```

We run the function again to show that the split is reproducible given
the same `random_state` value:

``` r
splitData2 <- getTrainTestSplit(data, train_size = 0.5, random_state = 123) 

splitData2$train
#>     x  y letter
#> 3   3 13      c
#> 10 10 20      j
#> 2   2 12      b
#> 8   8 18      h
#> 6   6 16      f
splitData2$test
#>   x  y letter
#> 9 9 19      i
#> 1 1 11      a
#> 7 7 17      g
#> 5 5 15      e
#> 4 4 14      d
```

## FAQ about Data Splitting

### Why do we need train/test data?

Machine learning can be used to generate a model to predict an outcome
based on data. To evaluate the performance of a model, data is split
into training and test data. The training data is used to generate the
model, whereas the test data is used to test the performance of the
model after trainng is complete. The same dataset is **not** used for
both training and testing, as this can lead to overestimation of model
performance.

### Why is the train/test split ratio important?

It is important to pick an optimal train-test split ratio for your
model. We want enough data to generate a good model, but we also want
enough test data to show that our model can make good predictions on
various types of data. Take predicting arrival times for different
transportation modes, for example. If our model predicts car arrival
times very well but not train/bus times and our test set only has one
car instance, we would think that our model does not predict well,
whereas, if we had only cars in our test set, we would think it predicts
extremely well. We want our test set to be representative of the
dataset.

#### Example: Effect of Splitting Ratio on Performance for Cancer Dataset

Below, we show an example of how the train-test split ratio can affect
performance for the cancer dataset. We use logistic regression models to
predict diagnosis (malignant or benign) of a tumour. We will try 5
different split ratios: 10%, 30%, 50%, 70%, and 90% of the data used in
the training set.

``` r
library(afratzscherFunctions)
suppressMessages(library(dplyr))
library(datateachr)

# clean up the data a bit
cancer_cleaned <- cancer_sample %>%
  mutate(diagnosis = case_when(diagnosis == 'M' ~ 1, TRUE ~ 0)) %>% select(-c(ID))

# split data using getTrainTestSplit function from this package
split_10 <- getTrainTestSplit(cancer_cleaned, train_size = 0.1, random_state = 1234)
split_30 <- getTrainTestSplit(cancer_cleaned, train_size = 0.3, random_state = 1234)
split_50 <- getTrainTestSplit(cancer_cleaned, train_size = 0.5, random_state = 1234)
split_70 <- getTrainTestSplit(cancer_cleaned, random_state = 1234)
split_90 <- getTrainTestSplit(cancer_cleaned, train_size = 0.9, random_state = 1234)

# build models
logit_10 <- glm(diagnosis~., data = split_10$train, family = "binomial")
logit_30 <- glm(diagnosis~., data = split_30$train, family = "binomial")
logit_50 <- glm(diagnosis~., data = split_50$train, family = "binomial")
logit_70 <- glm(diagnosis~., data = split_70$train, family = "binomial")
logit_90 <- glm(diagnosis~., data = split_90$train, family = "binomial")

# predict using models
predict_10 <- predict(logit_10, split_10$test, type = 'response')
predict_30 <- predict(logit_30, split_30$test, type = 'response')
predict_50 <- predict(logit_50, split_50$test, type = 'response')
predict_70 <- predict(logit_70, split_70$test, type = 'response')
predict_90 <- predict(logit_90, split_90$test, type = 'response')

# generate confusion matrix with false positive, false negative, true postiive, true negative counts
metrics_10_table <- table(split_10$test$diagnosis, predict_10 > 0.5)
metrics_30_table <- table(split_30$test$diagnosis, predict_30 > 0.5)
metrics_50_table <- table(split_50$test$diagnosis, predict_50 > 0.5)
metrics_70_table <- table(split_70$test$diagnosis, predict_70 > 0.5)
metrics_90_table <- table(split_90$test$diagnosis, predict_90 > 0.5)

# generate metrics to show performance between models
metrics <- tribble(
  ~split, ~accuracy, ~precision, ~recall,
  
  '0.1', ((metrics_10_table[2,2]+metrics_10_table[1,1])/(metrics_10_table[2,2]+metrics_10_table[1,2]+metrics_10_table[2,1]+metrics_10_table[1,1])), (metrics_10_table[2,2]/(metrics_10_table[2,2]+metrics_10_table[1,2])), 
  (metrics_10_table[2,2]/(metrics_10_table[2,2]+metrics_10_table[2,1])),
  
  '0.3', ((metrics_30_table[2,2]+metrics_30_table[1,1])/(metrics_30_table[2,2]+metrics_30_table[1,2]+metrics_30_table[2,1]+metrics_30_table[1,1])), (metrics_30_table[2,2]/(metrics_30_table[2,2]+metrics_30_table[1,2])), 
  (metrics_30_table[2,2]/(metrics_30_table[2,2]+metrics_30_table[2,1])),
  
  '0.5', ((metrics_50_table[2,2]+metrics_50_table[1,1])/(metrics_50_table[2,2]+metrics_50_table[1,2]+metrics_50_table[2,1]+metrics_50_table[1,1])), (metrics_50_table[2,2]/(metrics_50_table[2,2]+metrics_50_table[1,2])), 
  (metrics_50_table[2,2]/(metrics_50_table[2,2]+metrics_50_table[2,1])),
  
  '0.7', ((metrics_70_table[2,2]+metrics_70_table[1,1])/(metrics_70_table[2,2]+metrics_70_table[1,2]+metrics_70_table[2,1]+metrics_70_table[1,1])), (metrics_70_table[2,2]/(metrics_70_table[2,2]+metrics_70_table[1,2])), 
  (metrics_70_table[2,2]/(metrics_70_table[2,2]+metrics_70_table[2,1])),
  
  '0.9', ((metrics_90_table[2,2]+metrics_90_table[1,1])/(metrics_90_table[2,2]+metrics_90_table[1,2]+metrics_90_table[2,1]+metrics_90_table[1,1])), (metrics_90_table[2,2]/(metrics_90_table[2,2]+metrics_90_table[1,2])), 
  (metrics_90_table[2,2]/(metrics_90_table[2,2]+metrics_90_table[2,1]))
)

metrics
#> # A tibble: 5 ?? 4
#>   split accuracy precision recall
#>   <chr>    <dbl>     <dbl>  <dbl>
#> 1 0.1      0.768     0.653  0.806
#> 2 0.3      0.905     0.837  0.927
#> 3 0.5      0.923     0.853  0.952
#> 4 0.7      0.918     0.814  0.983
#> 5 0.9      0.930     0.909  0.909
```

In this example, we can see that accuracy and precision are highest when
we have a 90-10 training-test split. We can also see how have a low
ratio (10-90 split) leads to lower accuracy, precision, and recall. This
show just how much model performance can vary depending on data
splitting.
