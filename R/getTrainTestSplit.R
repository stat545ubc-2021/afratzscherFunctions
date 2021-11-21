#' @title A Train/Test Data Splitting Function
#'
#' @description Split data into training data and test data for machine learning. 
#'
#' @param data Tibble or data frame that you want to split.
#' @param train_size float or fraction between 0.0 and 1.0 that represents the proportion of data in 
#'  the train split. If it is not possible to split exactly, the train_size will be rounded down 
#'  (e.g. if 10 rows and train_size = 1/3, train set contains 3 rows, test set contains 7 rows). 
#'  train_size = 0 means all data is in the test set (training set is empty). train_size = 1 means 
#'  all data is in the training set (test set is empty). If train_size is not defined by the user, 
#'  it will be set the default value of 0.7 (7:3 train:test split).
#' @param shuffle boolean, determines whether or not to shuffle the data before splitting. 
#'  The default is shuffle = FALSE. If a random_state is defined, shuffle is 
#'  automatically set to TRUE.
#' @param random_state int, controls the shuffling applied to the data before the split is applied. 
#'  Set random_state for reproducible output across multiple function calls. 
#'  The default is random_state = NULL. If a random_state is defined, shuffle is automatically set to TRUE.
#' @return A list of length 2 containing train-test split of input data. 
#'  Access training and test data using `variable$train` and `variable$test`, respectively.
#' 
#' @examples
#' # Let us assume our data is stored in the following dataframe:
#' data <- data.frame(x = 1:10, y = 11:20, letter = letters[1:10])
#' 
#' # Using default parameters (7:3 train-test split, no shuffling)
#' getTrainTestSplit(data) 
#' 
#' # Specified 8:2 train-test split
#' getTrainTestSplit(data, 0.8) 
#' 
#' # Shuffle prior to splitting
#' getTrainTestSplit(data, shuffle = TRUE)
#' 
#' # Generate reproducible shuffling using `random_state`
#' getTrainTestSplit(data, random_state = 123) 
#' 
#' @note 
#' Errors occur if:
#' 1. `data` is not a tibble or dataframe. 
#' 2. `data`, `train_size`, `shuffle`, or `random_state` are not the proper type.
#' 3. `train_size` is not within range (less than 0 or greater than 1).
#' 4. There are no rows in `data`
#' 
#' @export

getTrainTestSplit <- function(data, train_size = 0.7, shuffle = FALSE, random_state = NULL)
{
  # check if correct data structure (df)
  if(!is.data.frame(data)) {
    stop('This function only works for dataframes.\n You have provided an object of class: ', class(data)[1]) 
  }
  
  # check that types of variables are correct 
  if (!is.numeric(train_size))
  {
    stop('The inputed train size is non-numeric.\n You have provided an object of class: ', class(train_size)[1])
  }
  if (!(is.numeric(random_state) || (is.null(random_state))))
  {
    stop('The inputed random state is non-numeric.\n You have provided an object of class: ', class(random_state)[1])
  }
  if (!is.logical(shuffle))
  {
    stop('Shuffle should be a boolean.\n You have provided an object of class: ', class(shuffle)[1])
  }
  
  # check if train_size is between 0 and 1
  if (train_size > 1 || train_size < 0)
  {
    stop('The train size is outside the range.\n Please select a train size between 0.0 and 1.0')
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
    stop('There are no rows in this dataframe.\n Please input a dataframe containing data.')
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