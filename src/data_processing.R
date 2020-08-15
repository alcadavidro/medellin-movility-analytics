# --------------------------- Reading data files -----------------------------

## All the data files
raw_files <- list.files('data/raw/')

## Data for exploratory data analysis and training models
train_raw_files <- raw_files[1:(length(raw_files) - 1)]
## test data
test_raw_files <- raw_files[length(raw_files)]

## building the data paths
train_data_files <- sapply(
  train_raw_files
  , function (x) paste0("./data/raw/", x)
  , USE.NAMES = F
  )

## Building the dataframes
train_data_list <- lapply(train_data_files, read.csv)
train_data <- do.call("rbind", train_data_list)
test_data <- read.csv(file = paste0("./data/raw/", test_raw_files))

## Saving the files into processed data

write.csv(train_data, file = "./data/processed/train_data.csv")
write.csv(test_data, file = "./data/processed/test_data.csv")