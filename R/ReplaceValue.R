#' Replace NA values
#'
#' @param data A FData object
#' @param colName A column name
#' @param method Either "mean" or "random". Method "mean" replaces a NA value by the mean value of the sample.
#' Method "random" replaces a NA value by a value drawn randomly from the sample.
#'
#' @return Return a vector completed.
#' @export
#'
#' @examples In progress
ReplaceValue <- function(data,colName,method){

  vector <-  data[,colName]

  if(method == "mean"){vectorCompleted <- sapply(1:length(vector),function(y) if(!is.na(vector[y])){vector[y]} else{mean(vector,na.rm=TRUE)})}

  if(method == "random"){vectorCompleted <- sapply(1:length(vector),function(y) if(!is.na(vector[y])){vector[y]} else{sample(vector[!is.na(vector)],1)})}
  return(vectorCompleted)
}
