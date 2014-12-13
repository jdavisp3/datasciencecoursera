readcsv <- function(directory, id) {
    filename <- sprintf('%03d.csv', id)
    read.csv(file.path(directory, filename))
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  values = vector('numeric')

  for (monitor_id in id) {
    frame = readcsv(directory, monitor_id)
    values = append(values, data.matrix(frame[pollutant]))
  }

  mean(values, na.rm = TRUE)
}
