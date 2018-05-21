pollutantmean <- function(directory, pollutant, id = 1:332){
  
  fileNames <- paste0(directory, "/", formatC(id, width = 3, flag = "0"), ".csv")
  
  dataset <- lapply(fileNames, read.csv)
  
  data_table <- do.call(rbind.data.frame, dataset)
  
  
  if(c(pollutant) %in% names(data_table)){
    mean <- lapply(data_table[pollutant], mean, na.rm = TRUE)
  }
  return(mean)
}


complete <- function(directory, id = 1:332){
  fileNames <- paste0(directory, "/", formatC(id, width = 3, flag = "0"), ".csv")
  
  dataset <- lapply(fileNames, read.csv)
  
  data_list <- lapply(dataset, complete.cases)
  nobs <- lapply(data_list, sum)
  output <- data.frame(id, nobs = cbind(nobs))
  
  return(output)
}

corr <- function(directory, threshold = 0){
  
  completes <- complete(directory)
  good_data <- subset(completes, nobs > threshold)
  correlations <- vector()
 
  if(nrow(good_data) != 0){
  
    fileNames <- paste0(directory, "/", formatC(good_data$id, width = 3, flag = "0"), ".csv")
    
    
    for(filename in fileNames){
      data <- read.csv(filename)
      complete_data <- data[complete.cases(data), ]
    
      correlations <- c(correlations, cor(complete_data$sulfate, complete_data$nitrate))
      }
    
  }
  correlations
  }