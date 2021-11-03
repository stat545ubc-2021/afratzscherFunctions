Assignment B1
================
Anne-Sophie Fratzscher
November 2nd, 2021

# Learning Objectives

In this assignment, we want to :

-   Learn how to create a function in R
-   Document a function using [roxygen2
    tags](https://roxygen2.r-lib.org/articles/rd-formatting.html)
-   Test a function using the `expect_()` function from the `testthat`
    package

# Introduction

We were motivated to create this function based on work we did in the
Mini Data Analysis Project for STAT545A.

In our Mini Data Analysis Project, we were using the `cancer_sample`
dataset from the `datateachr` package. This dataset contains 569 tumour
samples and 32 variables. Tumours were labeled as either malignant or
benign (`diagnosis` variable). For each sample, 10 features were
measured for each cell in a sample: radius, texture, perimeter, area,
smoothness, compactness, concavity, concave points, symmetry, and
fractal dimension. For each feature, the mean measurement, worst
measurement, and standard error was calculated and included as variables
(e.g. denoted as `feature_mean` for mean values, `feature_worst` for
worst values, and `feature_se` for standard error values).

In Milestone 3, we were investigating whether `standard error` and
`worst` variables added additional predictive power compared to using
just `mean` variables when we tried to predict tumour malignancy. We
generated two logistic regression models (one using only `mean`
variables, the other using all variables). We split our data into
training and test data, trained the models using only the training data,
and used the test data for generating predictions. We used these
predictions to evaluate if the addition of the `standard error` and
`worst` variables added predictive power.

We used a 70-30 split for our model. However, it is important to pick a
good splitting ratio, as splitting can have an effect on prediction. For
example, [using too little data for training can bias the model towards
trends found in that subset of
data](https://towardsdatascience.com/data-splitting-technique-to-fit-any-machine-learning-model-c0d7f3f1c790).
As a result, for this assignment, we wanted to create a function to
split data into training and test sets.

In this assignment, we create the function `get_train_test`, document it
using [roxygen2
tags](https://roxygen2.r-lib.org/articles/rd-formatting.html), give
examples of how to use the function, and test it using `expect_()`
functions from the `testthat` package

# Setup - Installing and loading necessary packages

We begin by loading our data and the `testthat` package below:

``` r
library(datateachr) # contains cancer_sample dataset, used in MDA 
library(testthat)
```

# Exercise 1 and 2: Make and Document a Function

First, we want to create our function and document it using roxygen2
tags. Our function, `get_train_test`, splits the data in a dataframe
into test and training data. This data can then be input into a model
for prediction (e.g. using the `glm()` function). The documentation and
the function can be found below:

``` r
#' A Train/Test Data Splitting Function
#'
#' Split data into 1) training data and 2) test data for machine learning. 
#'
#' @param data: Tibble or data frame that you want to split.
#' @param train_size: float or fraction between 0.0 and 1.0 that represents the proportion of data in the train split. If it is not possible to split exactly, the train_size will be rounded down (e.g. if 10 rows and train_size = 1/3, train set contains 3 rows, test set contains 7 rows). train_size = 0 means all data is in the test set (training set is empty). train_size = 1 means all data is in the training set (test set is empty). If train_size is not defined by the user, it will be set the default value of 0.7 (7:3 train:test split).
#' @param shuffle: boolean, determines whether or not to shuffle the data before splitting. The default is shuffle = FALSE. If a random_state is defined, shuffle is automatically set to TRUE.
#' @param random_state: int, controls the shuffling applied to the data before the split is applied. Set random_state for reproducible output across multiple function calls. The default is random_state = NULL. If a random_state is defined, shuffle is automatically set to TRUE.
#' @return A list of length 2 containing train-test split of input data. Access training and test data using variable$train and variable$test, respectively. 
#' 
#' error if `data`, `train_size`, `shuffle`, or `random_state` are not the proper type.
#' error if `train_size` is not within range (less than 0 or greater than 1).
#' error if no rows in `data`

get_train_test <- function(data, train_size = 0.7, shuffle = FALSE, random_state = NULL)
{
  # check if correct data structure (df)
  if(!is.data.frame(data)) {
    stop('This function only works for dataframes. You have provided an object of class: ', class(data)[1]) 
  }
  
  # check that types of variables are correct 
  if (!is.numeric(train_size))
  {
    stop('The inputed train size is non-numeric. You have provided an object of class: ', class(train_size)[1])
  }
  if (!(is.numeric(random_state) || (is.null(random_state))))
  {
    stop('The inputed random state is non-numeric. You have provided an object of class: ', class(random_state)[1])
  }
  if (!is.logical(shuffle))
  {
    stop('Shuffle should be a boolean. You have provided an object of class: ', class(shuffle)[1])
  }
  
  # check if train_size is between 0 and 1
  if (train_size > 1 || train_size < 0)
  {
    stop('The train size is outside the range. Please select a train size between 0.0 and 1.0')
  }
  
  # set seed if needed
  if (is.numeric(random_state))
  {
    set.seed(random_state)
    shuffle = TRUE
  }
  
  n_row = nrow(data)
  
  # Check if dataframe is empty
  if (n_row == 0)
  {
    stop('There are no rows in this dataframe. Please input a dataframe containing data.')
  }
  
  # shuffle data if shuffle is TRUE
  if (shuffle == TRUE) {
    rows <- sample(nrow(data))
    data <- data[rows, ]
  }

  # split data
  total_row = train_size * n_row
  train <- data[1: total_row, ]
  test <- data[-(1:total_row), ]
  
  # if train_size == 0.0, test = all data, train = nothing
  if (train_size == 0.0)
  {
    train <- data[FALSE, ]
    test <- data
  }
  
  return (list(train = train, test = test))
}
```

# Exercise 3: Examples of Function Usage

Now, we will show some example usages of our function.

## Example 1: Without shuffling

In this example, we use the first 10 instances from the `cancer_sample`
dataset. We want to select the first 7 rows (instances 1-7) for the
training set and the last 3 rows (instances 8-10) for the test dataset.
In this case, we will not shuffle the data before splitting. We see that
the output is a dataframe.

``` r
input <- head(cancer_sample, 10)
example1 <- get_train_test(input, train_size = 0.7)
print(example1$train) 
```

    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 1   842302         M       17.99        10.38         122.80    1001.0
    ## 2   842517         M       20.57        17.77         132.90    1326.0
    ## 3 84300903         M       19.69        21.25         130.00    1203.0
    ## 4 84348301         M       11.42        20.38          77.58     386.1
    ## 5 84358402         M       20.29        14.34         135.10    1297.0
    ## 6   843786         M       12.45        15.70          82.57     477.1
    ## 7   844359         M       18.25        19.98         119.60    1040.0
    ##   smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 1         0.11840          0.27760         0.3001             0.14710
    ## 2         0.08474          0.07864         0.0869             0.07017
    ## 3         0.10960          0.15990         0.1974             0.12790
    ## 4         0.14250          0.28390         0.2414             0.10520
    ## 5         0.10030          0.13280         0.1980             0.10430
    ## 6         0.12780          0.17000         0.1578             0.08089
    ## 7         0.09463          0.10900         0.1127             0.07400
    ##   symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 1        0.2419                0.07871    1.0950     0.9053        8.589
    ## 2        0.1812                0.05667    0.5435     0.7339        3.398
    ## 3        0.2069                0.05999    0.7456     0.7869        4.585
    ## 4        0.2597                0.09744    0.4956     1.1560        3.445
    ## 5        0.1809                0.05883    0.7572     0.7813        5.438
    ## 6        0.2087                0.07613    0.3345     0.8902        2.217
    ## 7        0.1794                0.05742    0.4467     0.7732        3.180
    ##   area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 1  153.40      0.006399        0.04904      0.05373           0.01587
    ## 2   74.08      0.005225        0.01308      0.01860           0.01340
    ## 3   94.03      0.006150        0.04006      0.03832           0.02058
    ## 4   27.23      0.009110        0.07458      0.05661           0.01867
    ## 5   94.44      0.011490        0.02461      0.05688           0.01885
    ## 6   27.19      0.007510        0.03345      0.03672           0.01137
    ## 7   53.91      0.004314        0.01382      0.02254           0.01039
    ##   symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 1     0.03003             0.006193        25.38         17.33          184.60
    ## 2     0.01389             0.003532        24.99         23.41          158.80
    ## 3     0.02250             0.004571        23.57         25.53          152.50
    ## 4     0.05963             0.009208        14.91         26.50           98.87
    ## 5     0.01756             0.005115        22.54         16.67          152.20
    ## 6     0.02165             0.005082        15.47         23.75          103.40
    ## 7     0.01369             0.002179        22.88         27.66          153.20
    ##   area_worst smoothness_worst compactness_worst concavity_worst
    ## 1     2019.0           0.1622            0.6656          0.7119
    ## 2     1956.0           0.1238            0.1866          0.2416
    ## 3     1709.0           0.1444            0.4245          0.4504
    ## 4      567.7           0.2098            0.8663          0.6869
    ## 5     1575.0           0.1374            0.2050          0.4000
    ## 6      741.6           0.1791            0.5249          0.5355
    ## 7     1606.0           0.1442            0.2576          0.3784
    ##   concave_points_worst symmetry_worst fractal_dimension_worst
    ## 1               0.2654         0.4601                 0.11890
    ## 2               0.1860         0.2750                 0.08902
    ## 3               0.2430         0.3613                 0.08758
    ## 4               0.2575         0.6638                 0.17300
    ## 5               0.1625         0.2364                 0.07678
    ## 6               0.1741         0.3985                 0.12440
    ## 7               0.1932         0.3063                 0.08368

``` r
print(example1$test)
```

    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 8  84458202         M       13.71        20.83          90.20     577.9
    ## 9    844981         M       13.00        21.82          87.50     519.8
    ## 10 84501001         M       12.46        24.04          83.97     475.9
    ##    smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 8           0.1189           0.1645        0.09366             0.05985
    ## 9           0.1273           0.1932        0.18590             0.09353
    ## 10          0.1186           0.2396        0.22730             0.08543
    ##    symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 8         0.2196                0.07451    0.5835      1.377        3.856
    ## 9         0.2350                0.07389    0.3063      1.002        2.406
    ## 10        0.2030                0.08243    0.2976      1.599        2.039
    ##    area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 8    50.96      0.008805        0.03029      0.02488           0.01448
    ## 9    24.32      0.005731        0.03502      0.03553           0.01226
    ## 10   23.94      0.007149        0.07217      0.07743           0.01432
    ##    symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 8      0.01486             0.005412        17.06         28.14          110.60
    ## 9      0.02143             0.003749        15.49         30.73          106.20
    ## 10     0.01789             0.010080        15.09         40.68           97.65
    ##    area_worst smoothness_worst compactness_worst concavity_worst
    ## 8       897.0           0.1654            0.3682          0.2678
    ## 9       739.3           0.1703            0.5401          0.5390
    ## 10      711.4           0.1853            1.0580          1.1050
    ##    concave_points_worst symmetry_worst fractal_dimension_worst
    ## 8                0.1556         0.3196                  0.1151
    ## 9                0.2060         0.4378                  0.1072
    ## 10               0.2210         0.4366                  0.2075

We see that the first 7 rows are selected for training, and the last 3
rows are selected for testing.

We can also input the split as a fraction instead of a decimal. This
gives the same result.

``` r
example1_fraction <- get_train_test(input, train_size = 7/10)
print(example1_fraction$train)
```

    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 1   842302         M       17.99        10.38         122.80    1001.0
    ## 2   842517         M       20.57        17.77         132.90    1326.0
    ## 3 84300903         M       19.69        21.25         130.00    1203.0
    ## 4 84348301         M       11.42        20.38          77.58     386.1
    ## 5 84358402         M       20.29        14.34         135.10    1297.0
    ## 6   843786         M       12.45        15.70          82.57     477.1
    ## 7   844359         M       18.25        19.98         119.60    1040.0
    ##   smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 1         0.11840          0.27760         0.3001             0.14710
    ## 2         0.08474          0.07864         0.0869             0.07017
    ## 3         0.10960          0.15990         0.1974             0.12790
    ## 4         0.14250          0.28390         0.2414             0.10520
    ## 5         0.10030          0.13280         0.1980             0.10430
    ## 6         0.12780          0.17000         0.1578             0.08089
    ## 7         0.09463          0.10900         0.1127             0.07400
    ##   symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 1        0.2419                0.07871    1.0950     0.9053        8.589
    ## 2        0.1812                0.05667    0.5435     0.7339        3.398
    ## 3        0.2069                0.05999    0.7456     0.7869        4.585
    ## 4        0.2597                0.09744    0.4956     1.1560        3.445
    ## 5        0.1809                0.05883    0.7572     0.7813        5.438
    ## 6        0.2087                0.07613    0.3345     0.8902        2.217
    ## 7        0.1794                0.05742    0.4467     0.7732        3.180
    ##   area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 1  153.40      0.006399        0.04904      0.05373           0.01587
    ## 2   74.08      0.005225        0.01308      0.01860           0.01340
    ## 3   94.03      0.006150        0.04006      0.03832           0.02058
    ## 4   27.23      0.009110        0.07458      0.05661           0.01867
    ## 5   94.44      0.011490        0.02461      0.05688           0.01885
    ## 6   27.19      0.007510        0.03345      0.03672           0.01137
    ## 7   53.91      0.004314        0.01382      0.02254           0.01039
    ##   symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 1     0.03003             0.006193        25.38         17.33          184.60
    ## 2     0.01389             0.003532        24.99         23.41          158.80
    ## 3     0.02250             0.004571        23.57         25.53          152.50
    ## 4     0.05963             0.009208        14.91         26.50           98.87
    ## 5     0.01756             0.005115        22.54         16.67          152.20
    ## 6     0.02165             0.005082        15.47         23.75          103.40
    ## 7     0.01369             0.002179        22.88         27.66          153.20
    ##   area_worst smoothness_worst compactness_worst concavity_worst
    ## 1     2019.0           0.1622            0.6656          0.7119
    ## 2     1956.0           0.1238            0.1866          0.2416
    ## 3     1709.0           0.1444            0.4245          0.4504
    ## 4      567.7           0.2098            0.8663          0.6869
    ## 5     1575.0           0.1374            0.2050          0.4000
    ## 6      741.6           0.1791            0.5249          0.5355
    ## 7     1606.0           0.1442            0.2576          0.3784
    ##   concave_points_worst symmetry_worst fractal_dimension_worst
    ## 1               0.2654         0.4601                 0.11890
    ## 2               0.1860         0.2750                 0.08902
    ## 3               0.2430         0.3613                 0.08758
    ## 4               0.2575         0.6638                 0.17300
    ## 5               0.1625         0.2364                 0.07678
    ## 6               0.1741         0.3985                 0.12440
    ## 7               0.1932         0.3063                 0.08368

``` r
print(example1_fraction$test)
```

    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 8  84458202         M       13.71        20.83          90.20     577.9
    ## 9    844981         M       13.00        21.82          87.50     519.8
    ## 10 84501001         M       12.46        24.04          83.97     475.9
    ##    smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 8           0.1189           0.1645        0.09366             0.05985
    ## 9           0.1273           0.1932        0.18590             0.09353
    ## 10          0.1186           0.2396        0.22730             0.08543
    ##    symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 8         0.2196                0.07451    0.5835      1.377        3.856
    ## 9         0.2350                0.07389    0.3063      1.002        2.406
    ## 10        0.2030                0.08243    0.2976      1.599        2.039
    ##    area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 8    50.96      0.008805        0.03029      0.02488           0.01448
    ## 9    24.32      0.005731        0.03502      0.03553           0.01226
    ## 10   23.94      0.007149        0.07217      0.07743           0.01432
    ##    symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 8      0.01486             0.005412        17.06         28.14          110.60
    ## 9      0.02143             0.003749        15.49         30.73          106.20
    ## 10     0.01789             0.010080        15.09         40.68           97.65
    ##    area_worst smoothness_worst compactness_worst concavity_worst
    ## 8       897.0           0.1654            0.3682          0.2678
    ## 9       739.3           0.1703            0.5401          0.5390
    ## 10      711.4           0.1853            1.0580          1.1050
    ##    concave_points_worst symmetry_worst fractal_dimension_worst
    ## 8                0.1556         0.3196                  0.1151
    ## 9                0.2060         0.4378                  0.1072
    ## 10               0.2210         0.4366                  0.2075

Also, we can get the same result by calling the function with only the
`input` data. This is because the default splitting ratio is set to 0.7.

``` r
noratio_result <- get_train_test(input)
print(noratio_result$train)
```

    ##         ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 1   842302         M       17.99        10.38         122.80    1001.0
    ## 2   842517         M       20.57        17.77         132.90    1326.0
    ## 3 84300903         M       19.69        21.25         130.00    1203.0
    ## 4 84348301         M       11.42        20.38          77.58     386.1
    ## 5 84358402         M       20.29        14.34         135.10    1297.0
    ## 6   843786         M       12.45        15.70          82.57     477.1
    ## 7   844359         M       18.25        19.98         119.60    1040.0
    ##   smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 1         0.11840          0.27760         0.3001             0.14710
    ## 2         0.08474          0.07864         0.0869             0.07017
    ## 3         0.10960          0.15990         0.1974             0.12790
    ## 4         0.14250          0.28390         0.2414             0.10520
    ## 5         0.10030          0.13280         0.1980             0.10430
    ## 6         0.12780          0.17000         0.1578             0.08089
    ## 7         0.09463          0.10900         0.1127             0.07400
    ##   symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 1        0.2419                0.07871    1.0950     0.9053        8.589
    ## 2        0.1812                0.05667    0.5435     0.7339        3.398
    ## 3        0.2069                0.05999    0.7456     0.7869        4.585
    ## 4        0.2597                0.09744    0.4956     1.1560        3.445
    ## 5        0.1809                0.05883    0.7572     0.7813        5.438
    ## 6        0.2087                0.07613    0.3345     0.8902        2.217
    ## 7        0.1794                0.05742    0.4467     0.7732        3.180
    ##   area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 1  153.40      0.006399        0.04904      0.05373           0.01587
    ## 2   74.08      0.005225        0.01308      0.01860           0.01340
    ## 3   94.03      0.006150        0.04006      0.03832           0.02058
    ## 4   27.23      0.009110        0.07458      0.05661           0.01867
    ## 5   94.44      0.011490        0.02461      0.05688           0.01885
    ## 6   27.19      0.007510        0.03345      0.03672           0.01137
    ## 7   53.91      0.004314        0.01382      0.02254           0.01039
    ##   symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 1     0.03003             0.006193        25.38         17.33          184.60
    ## 2     0.01389             0.003532        24.99         23.41          158.80
    ## 3     0.02250             0.004571        23.57         25.53          152.50
    ## 4     0.05963             0.009208        14.91         26.50           98.87
    ## 5     0.01756             0.005115        22.54         16.67          152.20
    ## 6     0.02165             0.005082        15.47         23.75          103.40
    ## 7     0.01369             0.002179        22.88         27.66          153.20
    ##   area_worst smoothness_worst compactness_worst concavity_worst
    ## 1     2019.0           0.1622            0.6656          0.7119
    ## 2     1956.0           0.1238            0.1866          0.2416
    ## 3     1709.0           0.1444            0.4245          0.4504
    ## 4      567.7           0.2098            0.8663          0.6869
    ## 5     1575.0           0.1374            0.2050          0.4000
    ## 6      741.6           0.1791            0.5249          0.5355
    ## 7     1606.0           0.1442            0.2576          0.3784
    ##   concave_points_worst symmetry_worst fractal_dimension_worst
    ## 1               0.2654         0.4601                 0.11890
    ## 2               0.1860         0.2750                 0.08902
    ## 3               0.2430         0.3613                 0.08758
    ## 4               0.2575         0.6638                 0.17300
    ## 5               0.1625         0.2364                 0.07678
    ## 6               0.1741         0.3985                 0.12440
    ## 7               0.1932         0.3063                 0.08368

``` r
print(noratio_result$test)
```

    ##          ID diagnosis radius_mean texture_mean perimeter_mean area_mean
    ## 8  84458202         M       13.71        20.83          90.20     577.9
    ## 9    844981         M       13.00        21.82          87.50     519.8
    ## 10 84501001         M       12.46        24.04          83.97     475.9
    ##    smoothness_mean compactness_mean concavity_mean concave_points_mean
    ## 8           0.1189           0.1645        0.09366             0.05985
    ## 9           0.1273           0.1932        0.18590             0.09353
    ## 10          0.1186           0.2396        0.22730             0.08543
    ##    symmetry_mean fractal_dimension_mean radius_se texture_se perimeter_se
    ## 8         0.2196                0.07451    0.5835      1.377        3.856
    ## 9         0.2350                0.07389    0.3063      1.002        2.406
    ## 10        0.2030                0.08243    0.2976      1.599        2.039
    ##    area_se smoothness_se compactness_se concavity_se concave_points_se
    ## 8    50.96      0.008805        0.03029      0.02488           0.01448
    ## 9    24.32      0.005731        0.03502      0.03553           0.01226
    ## 10   23.94      0.007149        0.07217      0.07743           0.01432
    ##    symmetry_se fractal_dimension_se radius_worst texture_worst perimeter_worst
    ## 8      0.01486             0.005412        17.06         28.14          110.60
    ## 9      0.02143             0.003749        15.49         30.73          106.20
    ## 10     0.01789             0.010080        15.09         40.68           97.65
    ##    area_worst smoothness_worst compactness_worst concavity_worst
    ## 8       897.0           0.1654            0.3682          0.2678
    ## 9       739.3           0.1703            0.5401          0.5390
    ## 10      711.4           0.1853            1.0580          1.1050
    ##    concave_points_worst symmetry_worst fractal_dimension_worst
    ## 8                0.1556         0.3196                  0.1151
    ## 9                0.2060         0.4378                  0.1072
    ## 10               0.2210         0.4366                  0.2075

## Example 2: With shuffling

We can also shuffle the data before splitting.

First, let us take a look at 10 rows of the `cancer_sample` dataset. We
select rows 200-210 so that we get some samples that are benign and some
that are malignant.

``` r
input <- cancer_sample[200:209, ]
input$ID # prints IDs instead of full dataframe (for simplicity)
```

    ##  [1]  877500  877501  877989  878796   87880   87930  879523  879804  879830
    ## [10] 8810158

If we do not set the `random_state` variable, we will not get the same
results if we call the function multiple times because the shuffling
will be different for each function call. We can see this below:

``` r
result1 <- get_train_test(input, train_size = 0.7, shuffle = TRUE)
result2 <-get_train_test(input, train_size = 0.7, shuffle = TRUE)
cat('Training data is identical?:', (all.equal(result1$train$ID, result2$train$ID) == TRUE))
```

    ## Training data is identical?: FALSE

``` r
cat('Test data is identical?:', (all.equal(result1$test$ID, result2$test$ID) == TRUE))
```

    ## Test data is identical?: FALSE

However, if we set the `random_state` variable, we will get the same
result every time we call the function with the same `random_state`
value:

``` r
result1 <- get_train_test(input, train_size = 0.7, shuffle = TRUE, random_state = 123)
result2 <-get_train_test(input, train_size = 0.7, shuffle = TRUE, random_state = 123)

cat('Training data is identical?:', (all.equal(result1$train, result2$train) == TRUE))
```

    ## Training data is identical?: TRUE

``` r
cat('Test data is identical?:', (all.equal(result1$test, result2$test) == TRUE))
```

    ## Test data is identical?: TRUE

Shuffling depends on the `random_state` value. Changing the value will
lead to a different shuffling. However, using the same `random_state`
value will always give the same results

``` r
result3 <- get_train_test(input, train_size = 0.7, shuffle = TRUE, random_state = 1234)
result4 <-get_train_test(input, train_size = 0.7, shuffle = TRUE, random_state = 1234)

cat('Training data is identical if different random state value?:', (all.equal(result2$train$ID, result3$train$ID) == TRUE))
```

    ## Training data is identical if different random state value?: FALSE

``` r
cat('Test data is identical if different random state value?:', (all.equal(result2$test$ID, result3$test$ID) == TRUE))
```

    ## Test data is identical if different random state value?: FALSE

``` r
cat('Training data is identical if same random state value?:', (all.equal(result3$train, result3$train) == TRUE))
```

    ## Training data is identical if same random state value?: TRUE

``` r
cat('Test data is identical if same random state value?:', (all.equal(result3$test, result3$test) == TRUE))
```

    ## Test data is identical if same random state value?: TRUE

## Example 3: Rounding Down of Splitting Ratio

In the case that it is not possible to split exactly, the training ratio
will be rounded down. For example, if we have 10 rows but a splitting
ratio of 1/3, we will round 1/3 down to 0.3, so 3 rows will be selected
for the training set and 7 rows for the test set.

``` r
input <- cancer_sample[200:209, ]
roundDownResult <- get_train_test(input, train_size = 0.33)
cat('Number rows in training set:', nrow(roundDownResult$train))
```

    ## Number rows in training set: 3

``` r
cat('Number of rows in test set:', nrow(roundDownResult$test))
```

    ## Number of rows in test set: 7

## Example 4: Select all (or no) Data for Training

We can set `train_size` to 1 so that all the data is used for training.

``` r
input <- cancer_sample[1:10, ]
allTrainingResult <- get_train_test(input, train_size = 1.0)
cat('Number rows in training set:', nrow(allTrainingResult$train))
```

    ## Number rows in training set: 10

``` r
cat('Number of rows in test set:', nrow(allTrainingResult$test))
```

    ## Number of rows in test set: 0

We can also set `train_size` to 0 if we want all of our data to be used
for testing.

``` r
input <- cancer_sample[1:10, ]
allTestingResult <- get_train_test(input, train_size = 0.0)
cat('Number rows in training set:', nrow(allTestingResult$train))
```

    ## Number rows in training set: 0

``` r
cat('Number of rows in test set:', nrow(allTestingResult$test))
```

    ## Number of rows in test set: 10

## Example 5: Errors - Wrong data type

If we input the wrong data type, we will get errors.

For example, if we try to input an array, we will get an error:

``` r
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

# Take these vectors as input to the array.
arrayInput <- array(c(vector1,vector2),dim = c(3,3,2))
print(arrayInput)
```

    ## , , 1
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]    5   10   13
    ## [2,]    9   11   14
    ## [3,]    3   12   15
    ## 
    ## , , 2
    ## 
    ##      [,1] [,2] [,3]
    ## [1,]    5   10   13
    ## [2,]    9   11   14
    ## [3,]    3   12   15

``` r
arrayResult <-get_train_test(arrayInput, train_size = 0.7)
```

    ## Error in get_train_test(arrayInput, train_size = 0.7): This function only works for dataframes. You have provided an object of class: array

Also, we will get errors if we do not input `train_size` or
`random_state`as a numeric or `shuffle` as a boolean:

``` r
input <- cancer_sample[200:210, ]
wrongTrainSizeResult <-get_train_test(input, train_size = '0.90')
```

    ## Error in get_train_test(input, train_size = "0.90"): The inputed train size is non-numeric. You have provided an object of class: character

``` r
wrongRandomStateResult <- get_train_test(input, train_size = 0.7, random_state = '123')
```

    ## Error in get_train_test(input, train_size = 0.7, random_state = "123"): The inputed random state is non-numeric. You have provided an object of class: character

``` r
wrongShuffleResult <- get_train_test(input, train_size = 0.7, shuffle = 1)
```

    ## Error in get_train_test(input, train_size = 0.7, shuffle = 1): Shuffle should be a boolean. You have provided an object of class: numeric

However, if multiple types are wrong, only the first input error will be
printed.

``` r
input <- cancer_sample[200:210, ]
multipleWrongResult <- get_train_test(input, train_size = '0.7', shuffle = 1, random_state = '123')
```

    ## Error in get_train_test(input, train_size = "0.7", shuffle = 1, random_state = "123"): The inputed train size is non-numeric. You have provided an object of class: character

## Example 6: Errors - Training split outside range

The training split has been limited to a range of 0.0 to 1.0. This is
because we can at most select none or all of the data (0% or 100%,
respectively). If the training split is not within this range, an error
will occur:

``` r
input <- cancer_sample[200:210, ]
wrongRangeResult <- get_train_test(input, train_size = 1.2)
```

    ## Error in get_train_test(input, train_size = 1.2): The train size is outside the range. Please select a train size between 0.0 and 1.0

``` r
wrongRangeResultFraction <- get_train_test(input, train_size = 4/3)
```

    ## Error in get_train_test(input, train_size = 4/3): The train size is outside the range. Please select a train size between 0.0 and 1.0

## Example 7: Errors - Empty dataframe

Lastly, an error will occur if there is no data in the dataframe (i.e. 0
rows).

``` r
#generate empty dataframe
input <- cancer_sample[FALSE,]
emptyDFResult <- get_train_test(input)
```

    ## Error in get_train_test(input): There are no rows in this dataframe. Please input a dataframe containing data.

# Exercise 4: Function Testing

To check that our function works as expected, we will test it using
`expect_` functions in the `testthat` package.

First, we will test for errors. We expect errors if:

-   The dataframe is empty/ has no rows
-   The input is of the wrong type
    -   `data` is not a tibble or dataframe
    -   `train_size` is not numeric
    -   `random_state` is not numeric
    -   `shuffle` is not boolean (TRUE or FALSE)
-   `train_size` is outside of the range
    -   `train_size` is negative
    -   `train_size` is greater than 1.0

``` r
function_error_testing <- test_that("Testing errors for get_train_test function", {
  expect_error(get_train_test(cancer_sample[FALSE,])) # empty dataframe causes error
  expect_error(get_train-test(list('temp1', 'temp2'))) # error if data is not a dataframe (is list)
  expect_error(get_train_test(cancer_sample[1:10, ], train_size = '1.0')) # train_size not numeric
  expect_error(get_train_test(cancer_sample[1:10, ], random_state = '123')) #random_state not numeric
  expect_error(get_train_test(cancer_sample[1:10, ], shuffle = 0)) # shuffle not boolean
  expect_error(get_train_test(cancer_sample[1:10, ], train_size = 4/3)) # train_size outside of range (too large)
  expect_error(get_train_test(cancer_sample[1:10, ], train_size = -1)) # train_size outside of range (negative)
})
```

    ## Test passed 🥳

We will also check that we get expected results when running our
function.

-   Running without shuffling should give the same results as if done
    manually
    -   e.g. If run on 10 rows (splitting ratio of 7:3), the first 7
        rows will be in `train` and the last 3 will be in `test`
    -   This should work regardless of if `train_size` is defined by the
        user or not
-   Running with `train_size` = 1 gives all data in train set (test set
    is empty)
-   Running with `train_size` = 0 gives all data in test set (train set
    is empty)
-   Running with different `random_state` values should give different
    splits
-   Running with the same `random_state` value should give identical
    splits (reproducible results)
-   Running with `shuffle` should give shuffled splits (i.e. different
    result from manual selection)
-   Running with `shuffle` and `random_state` defined or only
    `random_state` defined should give the same results (assuming same
    `random_state` value selected)
-   If the `train_size` ratio can not be fulfilled exactly, we will
    round the ratio prior to splitting (e.g. 10 rows and splitting ratio
    of 1/3 -> 3 rows in train, 7 in test)

``` r
function_testing <- test_that("Testing get_train_test function", {
# check that running with default split (0.7) on 10 samples gives same result as manually selecting
  expect_true(identical(get_train_test(cancer_sample[1:10, ])$train, cancer_sample[1:7, ]))
  expect_true(identical(get_train_test(cancer_sample[1:10, ])$test, cancer_sample[8:10, ]))

  # check that running with specified split (0.2) on 10 samples gives same result as manually selecting
  expect_true(identical(get_train_test(cancer_sample[1:10, ], train_size = 0.2)$train, cancer_sample[1:2, ]))
  expect_true(identical(get_train_test(cancer_sample[1:10, ], train_size = 0.2)$test, cancer_sample[3:10, ]))

  # train_size = 1 means empty test set
  expect_equal(nrow(get_train_test(cancer_sample[1:10, ], train_size = 1)$train), 10)
  expect_equal(nrow(get_train_test(cancer_sample[1:10, ], train_size = 1)$test), 0)
  
  # train_size = 1 means empty test set
  expect_equal(nrow(get_train_test(cancer_sample[1:10, ], train_size = 0.0)$train), 0)
  expect_equal(nrow(get_train_test(cancer_sample[1:10, ], train_size = 0.0)$test), 10)

  # check that running with different random_state gives different result
  expect_false(identical(get_train_test(cancer_sample[1:10, ], random_state=123)$train, get_train_test(cancer_sample[1:10, ],random_state=1234)$train))
  expect_false(identical(get_train_test(cancer_sample[1:10, ], random_state=123)$test, get_train_test(cancer_sample[1:10, ], random_state=1234)$test))

  # check that running with same random_state gives same result
  expect_true(identical(get_train_test(cancer_sample[1:10, ], random_state=123)$train, get_train_test(cancer_sample[1:10, ],random_state=123)$train))
  expect_true(identical(get_train_test(cancer_sample[1:10, ], random_state=123)$test, get_train_test(cancer_sample[1:10, ], random_state=123)$test))
  
  # check that running with shuffle works (different result than when manually select)
  expect_false(identical(get_train_test(cancer_sample[1:100, ], shuffle = TRUE, random_state=123)$train, cancer_sample[1:70, ]))
  expect_false(identical(get_train_test(cancer_sample[1:100, ], shuffle = TRUE, random_state=123)$test, cancer_sample[71:100, ]))
  
  # same result if do or do not defined shuffle if same random state
  expect_true(identical(get_train_test(cancer_sample[1:10, ], shuffle = TRUE, random_state=123)$train, get_train_test(cancer_sample[1:10, ], random_state=123)$train))
  expect_true(identical(get_train_test(cancer_sample[1:10, ], shuffle = TRUE, random_state=123)$test, get_train_test(cancer_sample[1:10, ], random_state=123)$test))
  
  # check that rounds down (1/3 for 10 rows = 3 rows in train, 7 rows in test)
  expect_true(identical(get_train_test(cancer_sample[1:10, ], train_size = 1/3)$train, cancer_sample[1:3, ]))
  expect_true(identical(get_train_test(cancer_sample[1:10, ], train_size = 1/3)$test, cancer_sample[4:10, ]))

})
```

    ## Test passed 🥳
