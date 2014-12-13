readcsv <- function(directory, id) {
  filename <- sprintf('%03d.csv', id)
  read.csv(file.path(directory, filename))
}

complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  complete <- vector()

  for (monitor_id in id) {
    frame = readcsv(directory, monitor_id)
    sulfate_is_complete <- !is.na(frame$sulfate)
    nitrate_is_complete <- !is.na(frame$nitrate)
    complete <- append(complete, sum(sulfate_is_complete & nitrate_is_complete))
  }

  data.frame(id=id, nobs=complete)
}
