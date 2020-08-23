pollutantmean <- function(directory, pollutant, id = 1:332) {
    data <- read.csv(directory)
    sub1 <- subset(play, ID = id, select = pollutant)
    bad <- is.na(sub1)
    sub5 <- sub1[!bad]
    mean(sub5)
}