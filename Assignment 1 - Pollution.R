dir <- "C:/Users/jnony/Desktop/R Data Science/R Programming/rprog_data_specdata/specdata/"

# Part 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
  #C:\Users\jnony\Desktop\R Data Science\R Programming\rprog_data_specdata\specdata
  #"sulfate", "nitrate"
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  values <- numeric()
  
  for (i in id) {
    data <- read.csv(filelist[i])
    values <- c(values, data[[pollutant]])
  }
  mean(values, na.rm = TRUE)
}

pollutantmean(dir, "nitrate", 70:72)


# Part 2
complete <- function(directory, id = 1:332) {
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  nobs <- numeric()
  
  for (i in id) {
    data <- read.csv(filelist[i])
    nobs <- c(nobs, sum(complete.cases(data)))
    
  }
  data.frame(id, nobs)
}

complete(dir,30:25)


# Part 3
# corr <- function(directory, threshold = 0) {
#   filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
#   cordata <- data.frame()
#   for (i in length(filelist)) {
#     data <- read.csv(filelist[i])
#     if(sum(complete.cases(data)) > threshold)
#       {
#       c(cordata, cor(data$nitrate, data$sulfate))
#       }
#   }
#   print(cordata)
# }

corr<-function(directory,threshold=0){
  #create list of file names
  filelist<-list.files(directory,full.names = TRUE)
  
  #create empty vector
  dat <- vector(mode = "numeric", length = 0)
  
  for(i in 1:length(filelist)){
    #read in file
    temp<- read.csv(filelist[i],header=TRUE)
    #delete NAs
    temp<-temp[complete.cases(temp),]
    #count the number of observations
    csum<-nrow(temp)
    #if the number of rows is greater than the threshold
    if(csum>threshold){
      #for that file you find the correlation between nitrate and sulfate
      #combine each correlation for each file in vector format using the concatenate function 
      #since this is not a data frame we cannot use rbind or cbind
      dat<-c(dat,cor(temp$nitrate,temp$sulfate))
    }
      
  }
  return(dat)
}