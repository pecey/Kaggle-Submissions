accuracy <- function(predicted, actual){
  p = predicted+1
  a = actual+1
  squared_log_error = (log(p) - log(a))**2
  return(sqrt(mean(squared_log_error)))
}