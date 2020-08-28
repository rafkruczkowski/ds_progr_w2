pollutantmean <- function(directory, pollutant, id = 1:332) {
    data <- read.csv(directory)
    sub1 <- subset(play, ID = id, select = pollutant)
    bad <- is.na(sub1)
    sub5 <- sub1[!bad]
    mean(sub5)
}
pollutantmean("/mnt/c/Users/raf/CloudStation/Creative/DataSci/LearningR/rprog_data_specdata/specdata/ds_progr_w2", "sulfate", 1:10)
/mnt/c/Users/raf/CloudStation/Creative/DataSci/LearningR/rprog_data_specdata/specdata/ds_progr_w2

pollutantmean <- function(directory, pollutant, id = 1:332) {
    file_list <- list.files(path=directory, pattern="*.csv")
    dataset <- data.frame()
    for (i in 1:length(file_list)) {
        temp_data <- read.csv(file_list[i])
        #print(file_list[i])
        dataset <- rbind(dataset, temp_data)
    }
    # full dataset loaded here
    dsdim <- dim(dataset)
    print(dsdim)
    print("ID is")
    print(id)
    # select on full ds
    sub1 <- c()
    for (i in id) {
        subloop <- subset(dataset, ID == i ,select = pollutant)
        sub1 <- rbind(sub1, subloop)
    }
    #sub1 <- subset(dataset, ID == c(id) ,select = pollutant)
    sub1_s <- dim(sub1)
    print("sub1_s is")
    print(sub1_s)
    #selectDS <- dataset
    #print(selectDS)
    #myclass <- class(selectDS)
    #print(myclass)
    #mytype <- typeof(selectDS)
    #print(mytype)
    #mydim <- dim(selectDS)
    #print(mydim)
    #mysummary <- summary(selectDS)
    #print(mysummary)
    # clean dataset
    bad <- is.na(sub1)
    clean <- sub1[!bad] 
    #print(clean stats)
    mycleanstat <- dim(clean)
    print(mycleanstat)
    #sub1 <- subset(clean, ID = id, select = pollutant)
    # calculate mean
    mymean <- mean(sub1)
    print(mymean)
}
#######################################################################
rm(list = ls())
setwd("/mnt/c/Users/raf/CloudStation/Creative/DataSci/LearningR/rprog_data_specdata/specdatads_progr_w2/")

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of  the pollutant for which we will calcultate the
    ## mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result
    means <- c()
    
    for(monitor in id){
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        monitor_data <- read.csv(path)
        interested_data <- monitor_data[pollutant]
        means <- c(means, interested_data[!is.na(interested_data)])
    }
    
    mean(means)
}


complete <- function(directory, id = 1:332){
    ## 'director' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the from:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    results <- data.frame(id=numeric(0), nobs=numeric(0))
    for(monitor in id){
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        monitor_data <- read.csv(path)
        interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
        interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
        nobs <- nrow(interested_data)
        results <- rbind(results, data.frame(id=monitor, nobs=nobs))
    }
    results
}

corr <- function(directory, threshold = 0){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the 
    ## number of completely observed observations (on all
    ## variables) requi?red to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    cor_results <- numeric(0)
    
    complete_cases <- complete(directory)
    complete_cases <- complete_cases[complete_cases$nobs>=threshold, ]
    #print(complete_cases["id"])
    #print(unlist(complete_cases["id"]))
    #print(complete_cases$id)
    
    if(nrow(complete_cases)>0){
        for(monitor in complete_cases$id){
            path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
            #print(path)
            monitor_data <- read.csv(path)
            #print(monitor_data)
            interested_data <- monitor_data[(!is.na(monitor_data$sulfate)), ]
            interested_data <- interested_data[(!is.na(interested_data$nitrate)), ]
            sulfate_data <- interested_data["sulfate"]
            nitrate_data <- interested_data["nitrate"]
            cor_results <- c(cor_results, cor(sulfate_data, nitrate_data))
        }
    }
    cor_results
}

source("corr.R")
source("complete.R")
cr <- corr("", 150)
head(cr)