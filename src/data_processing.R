# --------------------------- Reading data files -----------------------------
library(data.table)
library(dplyr)
library(stringi)

## All the data files
raw_files <- list.files('data/raw/')

## building the data paths
data_files <- sapply(
  raw_files
  , function (x) paste0("./data/raw/", x)
  , USE.NAMES = F
  )

## Building the dataframes
data_list <- lapply(data_files, function(x) read.csv(file = x))
df <- do.call("rbind", data_list)

# --------------------------- data cleansing -----------------------------

# Drop the columns of the dataframe
df <- select (df,-c(X,Y,OBJECTID,RADICADO,CBML,MES_NOMBRE,
                    X_MAGNAMED,Y_MAGNAMED, BARRIO,DIRECCION_ENC))

# Lowcase rownames
df <- mutate_each(df, funs=tolower)

# Change encoding
df <- df %>% 
  mutate_if(is.character, function(x){iconv(x, to = "ASCII//TRANSLIT")})

# Create column date formart
df <- df %>% 
  mutate(FECHA = as.Date.character(FECHA, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))

# Remove empty rows
df <- df[df$CLASE!= "", ]
df <- df[df$DISENO!= "", ]
df <- df[df$COMUNA!= "", ]
df <- df[df$COMUNA!= '0', ]

# Select data frame training and testing
train_data <- df[df$PERIODO!= '2018', ]
test_data <- df[df$PERIODO== '2018', ]

# Save data frame training and testing
write.csv(train_data, file = "./data/processed/train_data.csv")
write.csv(test_data, file = "./data/processed/test_data.csv")