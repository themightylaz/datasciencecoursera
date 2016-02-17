library(stringr)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  full_path_name <- NULL
  total = 0 
  observations = 0 
  for(i in seq_along(id)) {
    full_path_name[i] <- paste(paste(directory, str_pad(toString(id[i]), 3, pad = "0"), sep = "/"), "csv", sep = ".")
    print(full_path_name[i])
    data <- read.csv(full_path_name[i], header = TRUE, na.strings = c("NA","NaN", " "))
    data = na.omit(data) 
    observations = observations + nrow(data)
    if (pollutant == "sulfate") {
      total = total + sum(data$sulfate)
    } else {
      total = total + sum(data$nitrate)
    }
  }
  return (total / observations)
}
