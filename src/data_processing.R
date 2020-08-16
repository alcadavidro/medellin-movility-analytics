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
df <- select (df,-c(X,Y,OBJECTID,RADICADO,MES_NOMBRE,X_MAGNAMED,Y_MAGNAMED))

# Lowcase rownames
df <- mutate_each(df, funs=tolower)

# Change encoding
df <- df %>% 
  mutate_if(is.character, function(x){iconv(x, to = "ASCII//TRANSLIT")})

# Create column date formart
df <- df %>% 
  mutate(FECHA = as.Date.character(FECHA, tryFormats = c("%Y-%m-%d", "%Y/%m/%d")))

## CLASE
accident_class <- table(df$CLASE)
unique_accident_class <- names(accident_class)

extract_str <- function(str_vector, regex){
  idx <- stringi::stri_detect(str = str_vector, regex = regex)
  return(str_vector[idx])
}      
# caida ocupante
one_class <- extract_str(str_vector = unique_accident_class, regex = "[O|o]cupante$")
df$CLASE <- ifelse(df$CLASE %in% one_class, "caida ocupante", df$CLASE)

accident_class <- table(df$CLASE)
unique_accident_class <- names(accident_class)

# choque
(two_class <- extract_str(str_vector = unique_accident_class, regex = "[C|c]hoque"))
df$CLASE <- ifelse(df$CLASE %in% two_class, "choque", df$CLASE)

# Otros
df$CLASE <- ifelse(df$CLASE == "", "otro", df$CLASE)
df$CLASE <- tolower(df$CLASE)

# Select data frame training and testing
train_data <- df[df$PERIODO!= '2018', ]
test_data <- df[df$PERIODO== '2018', ]

# Save data frame training and testing
write.csv(train_data, file = "./data/processed/train_data.csv")
write.csv(test_data, file = "./data/processed/test_data.csv")