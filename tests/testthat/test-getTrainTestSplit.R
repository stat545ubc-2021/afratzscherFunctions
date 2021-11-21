test_that("Testing wrong data type input (not dataframe or tibble)", {
  expect_error(getTrainTestSplit(c(1,2,3))) # error if try with vector
  expect_error(getTrainTestSplit("1")) # error if try with character
  expect_error(getTrainTestSplit(TRUE)) # error if try with boolean
  expect_error(getTrainTestSplit(1)) # error if try with integer
  expect_error(getTrainTestSplit(matrix(1:6, nrow = 2, ncol = 3))) # error if try with matrix
  expect_error(getTrainTestSplit(list(1, "a", TRUE, 1+4i))) # error if try with list
  expect_error(getTrainTestSplit()) # error if no input
})


test_that("Testing errors for getTrainTestSplit function on cancer_sample dataset", {
  expect_error(getTrainTestSplit(datateachr::cancer_sample[FALSE,])) # empty dataframe causes error
  expect_error(getTrainTestSplit-test(list('temp1', 'temp2'))) # error if data is not a dataframe (is list)
  expect_error(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = '1.0')) # train_size not numeric
  expect_error(getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state = '123')) #random_state not numeric
  expect_error(getTrainTestSplit(datateachr::cancer_sample[1:10, ], shuffle = 0)) # shuffle not boolean
  expect_error(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 4/3)) # train_size outside of range (too large)
  expect_error(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = -1)) # train_size outside of range (negative)
})

test_that("Testing getTrainTestSplit function with default params on cancer_sample dataset", {
  # check that running with default split (0.7) on 10 samples gives same result as manually selecting
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ])$train, datateachr::cancer_sample[1:7, ]))
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ])$test, datateachr::cancer_sample[8:10, ]))
})

test_that("Testing getTrainTestSplit function with specified params on cancer_sample dataset", {
  # check that running with specified split (0.2) on 10 samples gives same result as manually selecting
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 0.2)$train, datateachr::cancer_sample[1:2, ]))
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 0.2)$test, datateachr::cancer_sample[3:10, ]))
  
  # train_size = 1 means empty test set
  expect_equal(nrow(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 1)$train), 10)
  expect_equal(nrow(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 1)$test), 0)
  
  # train_size = 1 means empty test set
  expect_equal(nrow(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 0.0)$train), 0)
  expect_equal(nrow(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 0.0)$test), 10)
  
  # check that running with different random_state gives different result
  expect_false(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$train, getTrainTestSplit(datateachr::cancer_sample[1:10, ],random_state=1234)$train))
  expect_false(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$test, getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=1234)$test))
  
  # check that running with same random_state gives same result
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$train, getTrainTestSplit(datateachr::cancer_sample[1:10, ],random_state=123)$train))
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$test, getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$test))
  
  # check that running with shuffle works (different result than when manually select)
  expect_false(identical(getTrainTestSplit(datateachr::cancer_sample[1:100, ], shuffle = TRUE, random_state=123)$train, datateachr::cancer_sample[1:70, ]))
  expect_false(identical(getTrainTestSplit(datateachr::cancer_sample[1:100, ], shuffle = TRUE, random_state=123)$test, datateachr::cancer_sample[71:100, ]))
  
  # same result if do or do not defined shuffle if same random state
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], shuffle = TRUE, random_state=123)$train, getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$train))
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], shuffle = TRUE, random_state=123)$test, getTrainTestSplit(datateachr::cancer_sample[1:10, ], random_state=123)$test))
})

test_that("Testing getTrainTestSplit function rounding correctly", {
  # check that rounds down (1/3 for 10 rows = 3 rows in train, 7 rows in test)
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 1/3)$train, datateachr::cancer_sample[1:3, ]))
  expect_true(identical(getTrainTestSplit(datateachr::cancer_sample[1:10, ], train_size = 1/3)$test, datateachr::cancer_sample[4:10, ]))
})

test_that("Testing getTrainTestSplit function on dataframe", {
  df <- data.frame(id = letters[1:10], x = 1:10, y = 11:20) #df object for testing
  expect_true(identical(getTrainTestSplit(df)$train, df[1:7, ]))
  expect_true(identical(getTrainTestSplit(df)$test, df[8:10, ]))
})