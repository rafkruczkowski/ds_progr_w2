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
        dataset <- read.csv(file_list[i])
        sub1 <- subset(dataset, ID = id, select = pollutant)
        bad <- is.na(sub1)
        sub5 <- sub1[!bad]
        result <- mean(sub5)
        print(result)
    }
}

complete <- function(directory, id = 1:332) {
    file_list <- list.files(path=directory, pattern="*.csv")
    dataset <- data.frame()
    for (i in 1:length(file_list)) {
        dataset <- read.csv(file_list[i])
        sub1 <- subset(dataset, ID = id,)
        sub2 <- nrow(sub1)
        print(c(i, sub2))
    }
}



list.files(path="/mnt/c/Users/raf/CloudStation/Creative/DataSci/LearningR/rprog_data_specdata/specdata/ds_progr_w2")