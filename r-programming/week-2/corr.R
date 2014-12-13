readcsv <- function(directory, id) {
  filename <- sprintf('%03d.csv', id)
  read.csv(file.path(directory, filename))
}

complete <- function(directory, id = 1:332) {
  complete <- vector()
  
  for (monitor_id in id) {
    frame = readcsv(directory, monitor_id)
    sulfate_is_complete <- !is.na(frame$sulfate)
    nitrate_is_complete <- !is.na(frame$nitrate)
    complete <- append(complete, sum(sulfate_is_complete & nitrate_is_complete))
  }
  
  data.frame(id=id, nobs=complete)
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  correlations <- vector('numeric')

  for (monitor_id in 1:332) {
    frame = readcsv(directory, monitor_id)
    
    sulfate <- frame$sulfate
    nitrate <- frame$nitrate

    sulfate_is_defined <- !is.na(sulfate)
    nitrate_is_defined <- !is.na(nitrate)
    
    is_complete <- sulfate_is_defined & nitrate_is_defined
    if (sum(is_complete) >= threshold) {
      correlation <- cor(sulfate[is_complete], nitrate[is_complete])
      correlations <- append(correlations, correlation)
    }
  }

  correlations[!is.na(correlations)]
}
