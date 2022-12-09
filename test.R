getwd()

library(dplyr)
library(ggplot2)


students <- read.csv("data/students-institution-data.csv", sep=";", dec = ",")
graduates <- read.csv("data/graduates-institution-data.csv", sep=";", dec = ",")
phd_students <- read.csv("data/phd_students-institution-discipline-data.csv", sep=";", dec = ",")


summary(students)
