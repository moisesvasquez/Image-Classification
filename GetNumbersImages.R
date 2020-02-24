# Generate CSV From Images
library(imager)
library(tidyverse)
library(data.table)


getNumbers <- function(image,label){
  im <- load.image(image)
  im <- deriche(im,2,order=2,axis="x") %>% deriche(2,order=2,axis="y") %>% resize(50,50)
  df <- as.data.frame(im)
  row <- data.frame(Label = label)
  R <- subset(df,df$cc == 1)
  G <- subset(df,df$cc == 2)
  B <- subset(df,df$cc == 3)
  
  c10 <- c(1:10)
  c20 <- c(11:20)
  c30 <- c(21:30)
  c40 <- c(31:40)
  c50 <- c(41:50)
  for(i in 1:5) {
    x <- i
    onX <- case_when(x == 1 ~ c10,
                     x == 2 ~ c20,
                     x == 3 ~ c30,
                     x == 4 ~ c40,
                     x == 5 ~ c50)
    for (j in 1:5) {
      y <- j
      onY <- case_when(y == 1 ~ c10,
                       y == 2 ~ c20,
                       y == 3 ~ c30,
                       y == 4 ~ c40,
                       y == 5 ~ c50)
      valName <- paste0("x", i, "y", j, ".R")
      temp <-
        R %>% filter(x %in% onX &
                       y %in% onY) %>% summarize(val = mean(value))
      colnames(temp) <- valName
      row <- cbind(row, temp)
    }
    
  }
  for(i in 1:5) {
    x <- i
    onX <- case_when(x == 1 ~ c10,
                     x == 2 ~ c20,
                     x == 3 ~ c30,
                     x == 4 ~ c40,
                     x == 5 ~ c50)
    for (j in 1:5) {
      y <- j
      onY <- case_when(y == 1 ~ c10,
                       y == 2 ~ c20,
                       y == 3 ~ c30,
                       y == 4 ~ c40,
                       y == 5 ~ c50)
      valName <- paste0("x", i, "y", j, ".G")
      temp <-
        G %>% filter(x %in% onX &
                       y %in% onY) %>% summarize(val = mean(value))
      colnames(temp) <- valName
      row <- cbind(row, temp)
    }
    
  }
  for(i in 1:5) {
    x <- i
    onX <- case_when(x == 1 ~ c10,
                     x == 2 ~ c20,
                     x == 3 ~ c30,
                     x == 4 ~ c40,
                     x == 5 ~ c50)
    for (j in 1:5) {
      y <- j
      onY <- case_when(y == 1 ~ c10,
                       y == 2 ~ c20,
                       y == 3 ~ c30,
                       y == 4 ~ c40,
                       y == 5 ~ c50)
      valName <- paste0("x", i, "y", j, ".B")
      temp <-
        B %>% filter(x %in% onX &
                       y %in% onY) %>% summarize(val = mean(value))
      colnames(temp) <- valName
      row <- cbind(row, temp)
    }
    
  }
  return(row)
}



Car_files <-
  list.files(path = "C:/Users/moise/Documents/R/STU/Spring 2020/Cars")
Cars <- data.frame()

for (i in 1:length(Car_files)) {
  foto <- paste0("Cars/", Car_files[i])
  data <- getNumbers(foto, "Car")
  Cars <- rbind(Cars, data)
}
No_Cars_files <-
  list.files(path = "C:/Users/moise/Documents/R/STU/Spring 2020/Not_Cars")
Not_Cars <- data.frame()

for (i in 1:length(No_Cars_files)) {
  foto <- paste0("Not_Cars/", No_Cars_files[i])
  data <- getNumbers(foto, "Not_Car")
  Not_Cars <- rbind(Not_Cars, data)
}
Data <- rbind(Cars,Not_Cars)
Data$Label <- as.factor(Data$Label)

write.csv(Data,file = "Images.csv")

