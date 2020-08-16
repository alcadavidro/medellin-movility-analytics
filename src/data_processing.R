# --------------------------- Reading data files -----------------------------
library(data.table)
library(stringi)

filePaths <- list.files(file.path("./data/raw"), pattern = 'Acc', full.names = TRUE)
result <- lapply(filePaths, fread)
names(result) <- filePaths
accidentes <- rbindlist(result, use.names = TRUE, idcol = "source")
accidentes <- accidentes[,source:=NULL]


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
str(df)

## CLASE
table(df$CLASE)
unique_accident_class <- unique(accident_class)

extract_str <- function(str_vector, regex){
  idx <- stringi::stri_detect(str = str_vector, regex = regex)
  return(str_vector[idx])
}      
# caida ocupante
one_class <- extract_str(str_vector = unique_accident_class, regex = "[O|o]cupante$")
df$CLASE <- ifelse(df$CLASE %in% one_class, "caida_ocupante", df$CLASE)
unique_accident_class <- unique(accident_class)

# choque
(two_class <- extract_str(str_vector = unique_accident_class, regex = "[C|c]hoque"))
df$CLASE <- ifelse(df$CLASE %in% two_class, "choque", df$CLASE)

# Otros
df$CLASE <- ifelse(df$CLASE == "", "otro", df$CLASE)
df$CLASE <- tolower(df$CLASE)


table(df$CLASE)

## BARRIO


## Saving the files into processed data



write.csv(train_data, file = "./data/processed/train_data.csv")
write.csv(test_data, file = "./data/processed/test_data.csv")
