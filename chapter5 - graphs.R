

##saveRDS(a,  file = "a_results.rds")

a <- readRDS(file = "rem_14072022.rds")
##Rename table row and column
a <- readRDS(file = "rem_14082022outcome.rds")
#datarem_14082022outcome
count <- c(1:1000)
theta <- a[[1]][[1]][,1]
SE <- a[[1]][[1]][,7]
relative_bias <- abs(((a[[1]][[1]][,1]-0.4055)/0.4055))

a1 <- data.frame(count,theta,SE, relative_bias)
a1$Method <- "M1"
a1$Structure <- "(10,1000)"
a1$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[1]][,2]
SE <- a[[1]][[1]][,8]
relative_bias <- abs(((a[[1]][[1]][,2]-0.4055)/0.4055))

a2 <- data.frame(count,theta,SE, relative_bias)
a2$Method <- "M2"
a2$Structure <- "(10,1000)"
a2$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[1]][,3]
SE <- a[[1]][[1]][,9]
relative_bias <- abs(((a[[1]][[1]][,3]-0.4055)/0.4055))

a3 <- data.frame(count,theta,SE, relative_bias)
a3$Method <- "M3"
a3$Structure <- "(10,1000)"
a3$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[1]][,4]
SE <- a[[1]][[1]][,10]
relative_bias <- abs(((a[[1]][[1]][,4]-0.4055)/0.4055))

a4 <- data.frame(count,theta,SE, relative_bias)
a4$Method <- "M4"
a4$Structure <- "(10,1000)"
a4$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[1]][,5]
SE <- a[[1]][[1]][,11]
relative_bias <- abs(((a[[1]][[1]][,5]-0.4055)/0.4055))

a5 <- data.frame(count,theta,SE, relative_bias)
a5$Method <- "M5"
a5$Structure <- "(10,1000)"
a5$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[1]][,6]
SE <- a[[1]][[1]][,12]
relative_bias <- abs(((a[[1]][[1]][,6]-0.4055)/0.4055))

a6 <- data.frame(count,theta,SE, relative_bias)
a6$Method <- "M6"
a6$Structure <- "(10,1000)"
a6$Surgeon <- "OR = 1.01"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,1]
SE <- a[[2]][[1]][,7]
relative_bias <- abs(((a[[2]][[1]][,1]-0.4055)/0.4055))

b1 <- data.frame(count,theta,SE, relative_bias)
b1$Method <- "M1"
b1$Structure <- "(10,1000)"
b1$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,2]
SE <- a[[2]][[1]][,8]
relative_bias <- abs(((a[[2]][[1]][,2]-0.4055)/0.4055))

b2 <- data.frame(count,theta,SE, relative_bias)
b2$Method <- "M2"
b2$Structure <- "(10,1000)"
b2$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,3]
SE <- a[[2]][[1]][,9]
relative_bias <- abs(((a[[2]][[1]][,3]-0.4055)/0.4055))

b3 <- data.frame(count,theta,SE, relative_bias)
b3$Method <- "M3"
b3$Structure <- "(10,1000)"
b3$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,4]
SE <- a[[2]][[1]][,10]
relative_bias <- abs(((a[[2]][[1]][,4]-0.4055)/0.4055))

b4 <- data.frame(count,theta,SE, relative_bias)
b4$Method <- "M4"
b4$Structure <- "(10,1000)"
b4$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,5]
SE <- a[[2]][[1]][,11]
relative_bias <- abs(((a[[2]][[1]][,5]-0.4055)/0.4055))

b5 <- data.frame(count,theta,SE, relative_bias)
b5$Method <- "M5"
b5$Structure <- "(10,1000)"
b5$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[1]][,6]
SE <- a[[2]][[1]][,12]
relative_bias <- abs(((a[[2]][[1]][,6]-0.4055)/0.4055))

b6 <- data.frame(count,theta,SE, relative_bias)
b6$Method <- "M6"
b6$Structure <- "(10,1000)"
b6$Surgeon <- "OR = 1.25"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,1]
SE <- a[[3]][[1]][,7]
relative_bias <- abs(((a[[3]][[1]][,1]-0.4055)/0.4055))

c1 <- data.frame(count,theta,SE, relative_bias)
c1$Method <- "M1"
c1$Structure <- "(10,1000)"
c1$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,2]
SE <- a[[3]][[1]][,8]
relative_bias <- abs(((a[[3]][[1]][,2]-0.4055)/0.4055))

c2 <- data.frame(count,theta,SE, relative_bias)
c2$Method <- "M2"
c2$Structure <- "(10,1000)"
c2$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,3]
SE <- a[[3]][[1]][,9]
relative_bias <- abs(((a[[3]][[1]][,3]-0.4055)/0.4055))

c3 <- data.frame(count,theta,SE, relative_bias)
c3$Method <- "M3"
c3$Structure <- "(10,1000)"
c3$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,4]
SE <- a[[3]][[1]][,10]
relative_bias <- abs(((a[[3]][[1]][,4]-0.4055)/0.4055))

c4 <- data.frame(count,theta,SE, relative_bias)
c4$Method <- "M4"
c4$Structure <- "(10,1000)"
c4$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,5]
SE <- a[[3]][[1]][,11]
relative_bias <- abs(((a[[3]][[1]][,5]-0.4055)/0.4055))

c5 <- data.frame(count,theta,SE, relative_bias)
c5$Method <- "M5"
c5$Structure <- "(10,1000)"
c5$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[1]][,6]
SE <- a[[3]][[1]][,12]
relative_bias <- abs(((a[[3]][[1]][,6]-0.4055)/0.4055))

c6 <- data.frame(count,theta,SE, relative_bias)
c6$Method <- "M6"
c6$Structure <- "(10,1000)"
c6$Surgeon <- "OR = 1.5"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[4]][[1]][,1]
SE <- a[[4]][[1]][,7]
relative_bias <- abs(((a[[4]][[1]][,1]-0.4055)/0.4055))

d1 <- data.frame(count,theta,SE, relative_bias)
d1$Method <- "M1"
d1$Structure <- "(10,1000)"
d1$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[1]][,2]
SE <- a[[4]][[1]][,8]
relative_bias <- abs(((a[[4]][[1]][,2]-0.4055)/0.4055))

d2 <- data.frame(count,theta,SE, relative_bias)
d2$Method <- "M2"
d2$Structure <- "(10,1000)"
d2$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[1]][,3]
SE <- a[[4]][[1]][,9]
relative_bias <- abs(((a[[4]][[1]][,3]-0.4055)/0.4055))

d3 <- data.frame(count,theta,SE, relative_bias)
d3$Method <- "M3"
d3$Structure <- "(10,1000)"
d3$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[1]][,4]
SE <- a[[4]][[1]][,10]
relative_bias <- abs(((a[[4]][[1]][,4]-0.4055)/0.4055))

d4 <- data.frame(count,theta,SE, relative_bias)
d4$Method <- "M4"
d4$Structure <- "(10,1000)"
d4$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[1]][,5]
SE <- a[[4]][[1]][,11]
relative_bias <- abs(((a[[4]][[1]][,5]-0.4055)/0.4055))

d5 <- data.frame(count,theta,SE, relative_bias)
d5$Method <- "M5"
d5$Structure <- "(10,1000)"
d5$Surgeon <- "OR = 2.5"


#data
count <- c(1:1000)
theta <- a[[4]][[1]][,6]
SE <- a[[4]][[1]][,12]
relative_bias <- abs(((a[[4]][[1]][,6]-0.4055)/0.4055))

d6 <- data.frame(count,theta,SE, relative_bias)
d6$Method <- "M6"
d6$Structure <- "(10,1000)"
d6$Surgeon <- "OR = 2.5"



final_10 <- rbind(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6)
##Rename table row and column

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,1]
SE <- a[[1]][[2]][,7]
relative_bias <- abs(((a[[1]][[2]][,1]-0.4055)/0.4055))

a1 <- data.frame(count,theta,SE, relative_bias)
a1$Method <- "M1"
a1$Structure <- "(50,200)"
a1$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,2]
SE <- a[[1]][[2]][,8]
relative_bias <- abs(((a[[1]][[2]][,2]-0.4055)/0.4055))

a2 <- data.frame(count,theta,SE, relative_bias)
a2$Method <- "M2"
a2$Structure <- "(50,200)"
a2$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,3]
SE <- a[[1]][[2]][,9]
relative_bias <- abs(((a[[1]][[2]][,3]-0.4055)/0.4055))

a3 <- data.frame(count,theta,SE, relative_bias)
a3$Method <- "M3"
a3$Structure <- "(50,200)"
a3$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,4]
SE <- a[[1]][[2]][,10]
relative_bias <- abs(((a[[1]][[2]][,4]-0.4055)/0.4055))

a4 <- data.frame(count,theta,SE, relative_bias)
a4$Method <- "M4"
a4$Structure <- "(50,200)"
a4$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,5]
SE <- a[[1]][[2]][,11]
relative_bias <- abs(((a[[1]][[2]][,5]-0.4055)/0.4055))

a5 <- data.frame(count,theta,SE, relative_bias)
a5$Method <- "M5"
a5$Structure <- "(50,200)"
a5$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[2]][,6]
SE <- a[[1]][[2]][,12]
relative_bias <- abs(((a[[1]][[2]][,6]-0.4055)/0.4055))

a6 <- data.frame(count,theta,SE, relative_bias)
a6$Method <- "M6"
a6$Structure <- "(50,200)"
a6$Surgeon <- "OR = 1.01"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,1]
SE <- a[[2]][[2]][,7]
relative_bias <- abs(((a[[2]][[2]][,1]-0.4055)/0.4055))

b1 <- data.frame(count,theta,SE, relative_bias)
b1$Method <- "M1"
b1$Structure <- "(50,200)"
b1$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,2]
SE <- a[[2]][[2]][,8]
relative_bias <- abs(((a[[2]][[2]][,2]-0.4055)/0.4055))

b2 <- data.frame(count,theta,SE, relative_bias)
b2$Method <- "M2"
b2$Structure <- "(50,200)"
b2$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,3]
SE <- a[[2]][[2]][,9]
relative_bias <- abs(((a[[2]][[2]][,3]-0.4055)/0.4055))

b3 <- data.frame(count,theta,SE, relative_bias)
b3$Method <- "M3"
b3$Structure <- "(50,200)"
b3$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,4]
SE <- a[[2]][[2]][,10]
relative_bias <- abs(((a[[2]][[2]][,4]-0.4055)/0.4055))

b4 <- data.frame(count,theta,SE, relative_bias)
b4$Method <- "M4"
b4$Structure <- "(50,200)"
b4$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,5]
SE <- a[[2]][[2]][,11]
relative_bias <- abs(((a[[2]][[2]][,5]-0.4055)/0.4055))

b5 <- data.frame(count,theta,SE, relative_bias)
b5$Method <- "M5"
b5$Structure <- "(50,200)"
b5$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[2]][,6]
SE <- a[[2]][[2]][,12]
relative_bias <- abs(((a[[2]][[2]][,6]-0.4055)/0.4055))

b6 <- data.frame(count,theta,SE, relative_bias)
b6$Method <- "M6"
b6$Structure <- "(50,200)"
b6$Surgeon <- "OR = 1.25"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,1]
SE <- a[[3]][[2]][,7]
relative_bias <- abs(((a[[3]][[2]][,1]-0.4055)/0.4055))

c1 <- data.frame(count,theta,SE, relative_bias)
c1$Method <- "M1"
c1$Structure <- "(50,200)"
c1$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,2]
SE <- a[[3]][[2]][,8]
relative_bias <- abs(((a[[3]][[2]][,2]-0.4055)/0.4055))

c2 <- data.frame(count,theta,SE, relative_bias)
c2$Method <- "M2"
c2$Structure <- "(50,200)"
c2$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,3]
SE <- a[[3]][[2]][,9]
relative_bias <- abs(((a[[3]][[2]][,3]-0.4055)/0.4055))

c3 <- data.frame(count,theta,SE, relative_bias)
c3$Method <- "M3"
c3$Structure <- "(50,200)"
c3$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,4]
SE <- a[[3]][[2]][,10]
relative_bias <- abs(((a[[3]][[2]][,4]-0.4055)/0.4055))

c4 <- data.frame(count,theta,SE, relative_bias)
c4$Method <- "M4"
c4$Structure <- "(50,200)"
c4$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,5]
SE <- a[[3]][[2]][,11]
relative_bias <- abs(((a[[3]][[2]][,5]-0.4055)/0.4055))

c5 <- data.frame(count,theta,SE, relative_bias)
c5$Method <- "M5"
c5$Structure <- "(50,200)"
c5$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[2]][,6]
SE <- a[[3]][[2]][,12]
relative_bias <- abs(((a[[3]][[2]][,6]-0.4055)/0.4055))

c6 <- data.frame(count,theta,SE, relative_bias)
c6$Method <- "M6"
c6$Structure <- "(50,200)"
c6$Surgeon <- "OR = 1.5"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,1]
SE <- a[[4]][[2]][,7]
relative_bias <- abs(((a[[4]][[2]][,1]-0.4055)/0.4055))

d1 <- data.frame(count,theta,SE, relative_bias)
d1$Method <- "M1"
d1$Structure <- "(50,200)"
d1$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,2]
SE <- a[[4]][[2]][,8]
relative_bias <- abs(((a[[4]][[2]][,2]-0.4055)/0.4055))

d2 <- data.frame(count,theta,SE, relative_bias)
d2$Method <- "M2"
d2$Structure <- "(50,200)"
d2$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,3]
SE <- a[[4]][[2]][,9]
relative_bias <- abs(((a[[4]][[2]][,3]-0.4055)/0.4055))

d3 <- data.frame(count,theta,SE, relative_bias)
d3$Method <- "M3"
d3$Structure <- "(50,200)"
d3$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,4]
SE <- a[[4]][[2]][,10]
relative_bias <- abs(((a[[4]][[2]][,4]-0.4055)/0.4055))

d4 <- data.frame(count,theta,SE, relative_bias)
d4$Method <- "M4"
d4$Structure <- "(50,200)"
d4$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,5]
SE <- a[[4]][[2]][,11]
relative_bias <- abs(((a[[4]][[2]][,5]-0.4055)/0.4055))

d5 <- data.frame(count,theta,SE, relative_bias)
d5$Method <- "M5"
d5$Structure <- "(50,200)"
d5$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[2]][,6]
SE <- a[[4]][[2]][,12]
relative_bias <- abs(((a[[4]][[2]][,6]-0.4055)/0.4055))

d6 <- data.frame(count,theta,SE, relative_bias)
d6$Method <- "M6"
d6$Structure <- "(50,200)"
d6$Surgeon <- "OR = 2.5"





final_50 <- rbind(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6)
##Rename table row and column

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,1]
SE <- a[[1]][[3]][,7]
relative_bias <- abs(((a[[1]][[3]][,1]-0.4055)/0.4055))

a1 <- data.frame(count,theta,SE, relative_bias)
a1$Method <- "M1"
a1$Structure <- "(100,100)"
a1$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,2]
SE <- a[[1]][[3]][,8]
relative_bias <- abs(((a[[1]][[3]][,2]-0.4055)/0.4055))

a2 <- data.frame(count,theta,SE, relative_bias)
a2$Method <- "M2"
a2$Structure <- "(100,100)"
a2$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,3]
SE <- a[[1]][[3]][,9]
relative_bias <- abs(((a[[1]][[3]][,3]-0.4055)/0.4055))

a3 <- data.frame(count,theta,SE, relative_bias)
a3$Method <- "M3"
a3$Structure <- "(100,100)"
a3$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,4]
SE <- a[[1]][[3]][,10]
relative_bias <- abs(((a[[1]][[3]][,4]-0.4055)/0.4055))

a4 <- data.frame(count,theta,SE, relative_bias)
a4$Method <- "M4"
a4$Structure <- "(100,100)"
a4$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,5]
SE <- a[[1]][[3]][,11]
relative_bias <- abs(((a[[1]][[3]][,5]-0.4055)/0.4055))

a5 <- data.frame(count,theta,SE, relative_bias)
a5$Method <- "M5"
a5$Structure <- "(100,100)"
a5$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[3]][,6]
SE <- a[[1]][[3]][,12]
relative_bias <- abs(((a[[1]][[3]][,6]-0.4055)/0.4055))

a6 <- data.frame(count,theta,SE, relative_bias)
a6$Method <- "M6"
a6$Structure <- "(100,100)"
a6$Surgeon <- "OR = 1.01"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[2]][[3]][,1]
SE <- a[[2]][[3]][,7]
relative_bias <- abs(((a[[2]][[3]][,1]-0.4055)/0.4055))

b1 <- data.frame(count,theta,SE, relative_bias)
b1$Method <- "M1"
b1$Structure <- "(100,100)"
b1$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[3]][,2]
SE <- a[[2]][[3]][,8]
relative_bias <- abs(((a[[2]][[3]][,2]-0.4055)/0.4055))

b2 <- data.frame(count,theta,SE, relative_bias)
b2$Method <- "M2"
b2$Structure <- "(100,100)"
b2$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[3]][,3]
SE <- a[[2]][[3]][,9]
relative_bias <- abs(((a[[2]][[3]][,3]-0.4055)/0.4055))

b3 <- data.frame(count,theta,SE, relative_bias)
b3$Method <- "M3"
b3$Structure <- "(100,100)"
b3$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[3]][,4]
SE <- a[[2]][[3]][,10]
relative_bias <- abs(((a[[2]][[3]][,4]-0.4055)/0.4055))

b4 <- data.frame(count,theta,SE, relative_bias)
b4$Method <- "M4"
b4$Structure <- "(100,100)"
b4$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[3]][,5]
SE <- a[[2]][[3]][,11]
relative_bias <- abs(((a[[2]][[3]][,5]-0.4055)/0.4055))

b5 <- data.frame(count,theta,SE, relative_bias)
b5$Method <- "M5"
b5$Structure <- "(100,100)"
b5$Surgeon <- "OR = 1.25"


#data
count <- c(1:1000)
theta <- a[[2]][[3]][,6]
SE <- a[[2]][[3]][,12]
relative_bias <- abs(((a[[2]][[3]][,6]-0.4055)/0.4055))

b6 <- data.frame(count,theta,SE, relative_bias)
b6$Method <- "M6"
b6$Structure <- "(100,100)"
b6$Surgeon <- "OR = 1.25"
################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,1]
SE <- a[[3]][[3]][,7]
relative_bias <- abs(((a[[3]][[3]][,1]-0.4055)/0.4055))

c1 <- data.frame(count,theta,SE, relative_bias)
c1$Method <- "M1"
c1$Structure <- "(100,100)"
c1$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,2]
SE <- a[[3]][[3]][,8]
relative_bias <- abs(((a[[3]][[3]][,2]-0.4055)/0.4055))

c2 <- data.frame(count,theta,SE, relative_bias)
c2$Method <- "M2"
c2$Structure <- "(100,100)"
c2$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,3]
SE <- a[[3]][[3]][,9]
relative_bias <- abs(((a[[3]][[3]][,3]-0.4055)/0.4055))

c3 <- data.frame(count,theta,SE, relative_bias)
c3$Method <- "M3"
c3$Structure <- "(100,100)"
c3$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,4]
SE <- a[[3]][[3]][,10]
relative_bias <- abs(((a[[3]][[3]][,4]-0.4055)/0.4055))

c4 <- data.frame(count,theta,SE, relative_bias)
c4$Method <- "M4"
c4$Structure <- "(100,100)"
c4$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,5]
SE <- a[[3]][[3]][,11]
relative_bias <- abs(((a[[3]][[3]][,5]-0.4055)/0.4055))

c5 <- data.frame(count,theta,SE, relative_bias)
c5$Method <- "M5"
c5$Structure <- "(100,100)"
c5$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[3]][,6]
SE <- a[[3]][[3]][,12]
relative_bias <- abs(((a[[3]][[3]][,6]-0.4055)/0.4055))

c6 <- data.frame(count,theta,SE, relative_bias)
c6$Method <- "M6"
c6$Structure <- "(100,100)"
c6$Surgeon <- "OR = 1.5"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,1]
SE <- a[[4]][[3]][,7]
relative_bias <- abs(((a[[4]][[3]][,1]-0.4055)/0.4055))

d1 <- data.frame(count,theta,SE, relative_bias)
d1$Method <- "M1"
d1$Structure <- "(100,100)"
d1$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,2]
SE <- a[[4]][[3]][,8]
relative_bias <- abs(((a[[4]][[3]][,2]-0.4055)/0.4055))

d2 <- data.frame(count,theta,SE, relative_bias)
d2$Method <- "M2"
d2$Structure <- "(100,100)"
d2$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,3]
SE <- a[[4]][[3]][,9]
relative_bias <- abs(((a[[4]][[3]][,3]-0.4055)/0.4055))

d3 <- data.frame(count,theta,SE, relative_bias)
d3$Method <- "M3"
d3$Structure <- "(100,100)"
d3$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,4]
SE <- a[[4]][[3]][,10]
relative_bias <- abs(((a[[4]][[3]][,4]-0.4055)/0.4055))

d4 <- data.frame(count,theta,SE, relative_bias)
d4$Method <- "M4"
d4$Structure <- "(100,100)"
d4$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,5]
SE <- a[[4]][[3]][,11]
relative_bias <- abs(((a[[4]][[3]][,5]-0.4055)/0.4055))

d5 <- data.frame(count,theta,SE, relative_bias)
d5$Method <- "M5"
d5$Structure <- "(100,100)"
d5$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[3]][,6]
SE <- a[[4]][[3]][,12]
relative_bias <- abs(((a[[4]][[3]][,6]-0.4055)/0.4055))

d6 <- data.frame(count,theta,SE, relative_bias)
d6$Method <- "M6"
d6$Structure <- "(100,100)"
d6$Surgeon <- "OR = 2.5"

final_100 <- rbind(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6)
##Rename table row and column

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,1]
SE <- a[[1]][[4]][,7]
relative_bias <- abs(((a[[1]][[4]][,1]-0.4055)/0.4055))

a1 <- data.frame(count,theta,SE, relative_bias)
a1$Method <- "M1"
a1$Structure <- "(200,50)"
a1$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,2]
SE <- a[[1]][[4]][,8]
relative_bias <- abs(((a[[1]][[4]][,2]-0.4055)/0.4055))

a2 <- data.frame(count,theta,SE, relative_bias)
a2$Method <- "M2"
a2$Structure <- "(200,50)"
a2$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,3]
SE <- a[[1]][[4]][,9]
relative_bias <- abs(((a[[1]][[4]][,3]-0.4055)/0.4055))

a3 <- data.frame(count,theta,SE, relative_bias)
a3$Method <- "M3"
a3$Structure <- "(200,50)"
a3$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,4]
SE <- a[[1]][[4]][,10]
relative_bias <- abs(((a[[1]][[4]][,4]-0.4055)/0.4055))

a4 <- data.frame(count,theta,SE, relative_bias)
a4$Method <- "M4"
a4$Structure <- "(200,50)"
a4$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,5]
SE <- a[[1]][[4]][,11]
relative_bias <- abs(((a[[1]][[4]][,5]-0.4055)/0.4055))

a5 <- data.frame(count,theta,SE, relative_bias)
a5$Method <- "M5"
a5$Structure <- "(200,50)"
a5$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[4]][,6]
SE <- a[[1]][[4]][,12]
relative_bias <- abs(((a[[1]][[4]][,6]-0.4055)/0.4055))

a6 <- data.frame(count,theta,SE, relative_bias)
a6$Method <- "M6"
a6$Structure <- "(200,50)"
a6$Surgeon <- "OR = 1.01"
################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,1]
SE <- a[[2]][[4]][,7]
relative_bias <- abs(((a[[2]][[4]][,1]-0.4055)/0.4055))

b1 <- data.frame(count,theta,SE, relative_bias)
b1$Method <- "M1"
b1$Structure <- "(200,50)"
b1$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,2]
SE <- a[[2]][[4]][,8]
relative_bias <- abs(((a[[2]][[4]][,2]-0.4055)/0.4055))

b2 <- data.frame(count,theta,SE, relative_bias)
b2$Method <- "M2"
b2$Structure <- "(200,50)"
b2$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,3]
SE <- a[[2]][[4]][,9]
relative_bias <- abs(((a[[2]][[4]][,3]-0.4055)/0.4055))

b3 <- data.frame(count,theta,SE, relative_bias)
b3$Method <- "M3"
b3$Structure <- "(200,50)"
b3$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,4]
SE <- a[[2]][[4]][,10]
relative_bias <- abs(((a[[2]][[4]][,4]-0.4055)/0.4055))

b4 <- data.frame(count,theta,SE, relative_bias)
b4$Method <- "M4"
b4$Structure <- "(200,50)"
b4$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,5]
SE <- a[[2]][[4]][,11]
relative_bias <- abs(((a[[2]][[4]][,5]-0.4055)/0.4055))

b5 <- data.frame(count,theta,SE, relative_bias)
b5$Method <- "M5"
b5$Structure <- "(200,50)"
b5$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[4]][,6]
SE <- a[[2]][[4]][,12]
relative_bias <- abs(((a[[2]][[4]][,6]-0.4055)/0.4055))

b6 <- data.frame(count,theta,SE, relative_bias)
b6$Method <- "M6"
b6$Structure <- "(200,50)"
b6$Surgeon <- "OR = 1.25"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,1]
SE <- a[[3]][[4]][,7]
relative_bias <- abs(((a[[3]][[4]][,1]-0.4055)/0.4055))

c1 <- data.frame(count,theta,SE, relative_bias)
c1$Method <- "M1"
c1$Structure <- "(200,50)"
c1$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,2]
SE <- a[[3]][[4]][,8]
relative_bias <- abs(((a[[3]][[4]][,2]-0.4055)/0.4055))

c2 <- data.frame(count,theta,SE, relative_bias)
c2$Method <- "M2"
c2$Structure <- "(200,50)"
c2$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,3]
SE <- a[[3]][[4]][,9]
relative_bias <- abs(((a[[3]][[4]][,3]-0.4055)/0.4055))

c3 <- data.frame(count,theta,SE, relative_bias)
c3$Method <- "M3"
c3$Structure <- "(200,50)"
c3$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,4]
SE <- a[[3]][[4]][,10]
relative_bias <- abs(((a[[3]][[4]][,4]-0.4055)/0.4055))

c4 <- data.frame(count,theta,SE, relative_bias)
c4$Method <- "M4"
c4$Structure <- "(200,50)"
c4$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,5]
SE <- a[[3]][[4]][,11]
relative_bias <- abs(((a[[3]][[4]][,5]-0.4055)/0.4055))

c5 <- data.frame(count,theta,SE, relative_bias)
c5$Method <- "M5"
c5$Structure <- "(200,50)"
c5$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[4]][,6]
SE <- a[[3]][[4]][,12]
relative_bias <- abs(((a[[3]][[4]][,6]-0.4055)/0.4055))

c6 <- data.frame(count,theta,SE, relative_bias)
c6$Method <- "M6"
c6$Structure <- "(200,50)"
c6$Surgeon <- "OR = 1.5"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,1]
SE <- a[[4]][[4]][,7]
relative_bias <- abs(((a[[4]][[4]][,1]-0.4055)/0.4055))

d1 <- data.frame(count,theta,SE, relative_bias)
d1$Method <- "M1"
d1$Structure <- "(200,50)"
d1$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,2]
SE <- a[[4]][[4]][,8]
relative_bias <- abs(((a[[4]][[4]][,2]-0.4055)/0.4055))

d2 <- data.frame(count,theta,SE, relative_bias)
d2$Method <- "M2"
d2$Structure <- "(200,50)"
d2$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,3]
SE <- a[[4]][[4]][,9]
relative_bias <- abs(((a[[4]][[4]][,3]-0.4055)/0.4055))

d3 <- data.frame(count,theta,SE, relative_bias)
d3$Method <- "M3"
d3$Structure <- "(200,50)"
d3$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,4]
SE <- a[[4]][[4]][,10]
relative_bias <- abs(((a[[4]][[4]][,4]-0.4055)/0.4055))

d4 <- data.frame(count,theta,SE, relative_bias)
d4$Method <- "M4"
d4$Structure <- "(200,50)"
d4$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,5]
SE <- a[[4]][[4]][,11]
relative_bias <- abs(((a[[4]][[4]][,5]-0.4055)/0.4055))

d5 <- data.frame(count,theta,SE, relative_bias)
d5$Method <- "M5"
d5$Structure <- "(200,50)"
d5$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[4]][,6]
SE <- a[[4]][[4]][,12]
relative_bias <- abs(((a[[4]][[4]][,6]-0.4055)/0.4055))

d6 <- data.frame(count,theta,SE, relative_bias)
d6$Method <- "M6"
d6$Structure <- "(200,50)"
d6$Surgeon <- "OR = 2.5"

final_200 <- rbind(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6)


##Rename table row and column

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,1]
SE <- a[[1]][[5]][,7]
relative_bias <- abs(((a[[1]][[5]][,1]-0.4055)/0.4055))

a1 <- data.frame(count,theta,SE, relative_bias)
a1$Method <- "M1"
a1$Structure <- "(500,20)"
a1$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,2]
SE <- a[[1]][[5]][,8]
relative_bias <- abs(((a[[1]][[5]][,2]-0.4055)/0.4055))

a2 <- data.frame(count,theta,SE, relative_bias)
a2$Method <- "M2"
a2$Structure <- "(500,20)"
a2$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,3]
SE <- a[[1]][[5]][,9]
relative_bias <- abs(((a[[1]][[5]][,3]-0.4055)/0.4055))

a3 <- data.frame(count,theta,SE, relative_bias)
a3$Method <- "M3"
a3$Structure <- "(500,20)"
a3$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,4]
SE <- a[[1]][[5]][,10]
relative_bias <- abs(((a[[1]][[5]][,4]-0.4055)/0.4055))

a4 <- data.frame(count,theta,SE, relative_bias)
a4$Method <- "M4"
a4$Structure <- "(500,20)"
a4$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,5]
SE <- a[[1]][[5]][,11]
relative_bias <- abs(((a[[1]][[5]][,5]-0.4055)/0.4055))

a5 <- data.frame(count,theta,SE, relative_bias)
a5$Method <- "M5"
a5$Structure <- "(500,20)"
a5$Surgeon <- "OR = 1.01"

#data
count <- c(1:1000)
theta <- a[[1]][[5]][,6]
SE <- a[[1]][[5]][,12]
relative_bias <- abs(((a[[1]][[5]][,6]-0.4055)/0.4055))

a6 <- data.frame(count,theta,SE, relative_bias)
a6$Method <- "M6"
a6$Structure <- "(500,20)"
a6$Surgeon <- "OR = 1.01"
################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,1]
SE <- a[[2]][[5]][,7]
relative_bias <- abs(((a[[2]][[5]][,1]-0.4055)/0.4055))

b1 <- data.frame(count,theta,SE, relative_bias)
b1$Method <- "M1"
b1$Structure <- "(500,20)"
b1$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,2]
SE <- a[[2]][[5]][,8]
relative_bias <- abs(((a[[2]][[5]][,2]-0.4055)/0.4055))

b2 <- data.frame(count,theta,SE, relative_bias)
b2$Method <- "M2"
b2$Structure <- "(500,20)"
b2$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,3]
SE <- a[[2]][[5]][,9]
relative_bias <- abs(((a[[2]][[5]][,3]-0.4055)/0.4055))

b3 <- data.frame(count,theta,SE, relative_bias)
b3$Method <- "M3"
b3$Structure <- "(500,20)"
b3$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,4]
SE <- a[[2]][[5]][,10]
relative_bias <- abs(((a[[2]][[5]][,4]-0.4055)/0.4055))

b4 <- data.frame(count,theta,SE, relative_bias)
b4$Method <- "M4"
b4$Structure <- "(500,20)"
b4$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,5]
SE <- a[[2]][[5]][,10]
relative_bias <- abs(((a[[2]][[5]][,5]-0.4055)/0.4055))

b5 <- data.frame(count,theta,SE, relative_bias)
b5$Method <- "M5"
b5$Structure <- "(500,20)"
b5$Surgeon <- "OR = 1.25"

#data
count <- c(1:1000)
theta <- a[[2]][[5]][,6]
SE <- a[[2]][[5]][,12]
relative_bias <- abs(((a[[2]][[5]][,6]-0.4055)/0.4055))

b6 <- data.frame(count,theta,SE, relative_bias)
b6$Method <- "M6"
b6$Structure <- "(500,20)"
b6$Surgeon <- "OR = 1.25"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,1]
SE <- a[[3]][[5]][,7]
relative_bias <- abs(((a[[3]][[5]][,1]-0.4055)/0.4055))

c1 <- data.frame(count,theta,SE, relative_bias)
c1$Method <- "M1"
c1$Structure <- "(500,20)"
c1$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,2]
SE <- a[[3]][[5]][,8]
relative_bias <- abs(((a[[3]][[5]][,2]-0.4055)/0.4055))

c2 <- data.frame(count,theta,SE, relative_bias)
c2$Method <- "M2"
c2$Structure <- "(500,20)"
c2$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,3]
SE <- a[[3]][[5]][,9]
relative_bias <- abs(((a[[3]][[5]][,3]-0.4055)/0.4055))

c3 <- data.frame(count,theta,SE, relative_bias)
c3$Method <- "M3"
c3$Structure <- "(500,20)"
c3$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,4]
SE <- a[[3]][[5]][,10]
relative_bias <- abs(((a[[3]][[5]][,4]-0.4055)/0.4055))

c4 <- data.frame(count,theta,SE, relative_bias)
c4$Method <- "M4"
c4$Structure <- "(500,20)"
c4$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,5]
SE <- a[[3]][[5]][,11]
relative_bias <- abs(((a[[3]][[5]][,5]-0.4055)/0.4055))

c5 <- data.frame(count,theta,SE, relative_bias)
c5$Method <- "M5"
c5$Structure <- "(500,20)"
c5$Surgeon <- "OR = 1.5"

#data
count <- c(1:1000)
theta <- a[[3]][[5]][,6]
SE <- a[[3]][[5]][,12]
relative_bias <- abs(((a[[3]][[5]][,6]-0.4055)/0.4055))

c6 <- data.frame(count,theta,SE, relative_bias)
c6$Method <- "M6"
c6$Structure <- "(500,20)"
c6$Surgeon <- "OR = 1.5"

################################################################################
#data

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,1]
SE <- a[[4]][[5]][,7]
relative_bias <- abs(((a[[4]][[5]][,1]-0.4055)/0.4055))

d1 <- data.frame(count,theta,SE, relative_bias)
d1$Method <- "M1"
d1$Structure <- "(500,20)"
d1$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,2]
SE <- a[[4]][[5]][,8]
relative_bias <- abs(((a[[4]][[5]][,2]-0.4055)/0.4055))

d2 <- data.frame(count,theta,SE, relative_bias)
d2$Method <- "M2"
d2$Structure <- "(500,20)"
d2$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,3]
SE <- a[[4]][[5]][,9]
relative_bias <- abs(((a[[4]][[5]][,3]-0.4055)/0.4055))

d3 <- data.frame(count,theta,SE, relative_bias)
d3$Method <- "M3"
d3$Structure <- "(500,20)"
d3$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,4]
SE <- a[[4]][[5]][,10]
relative_bias <- abs(((a[[4]][[5]][,4]-0.4055)/0.4055))

d4 <- data.frame(count,theta,SE, relative_bias)
d4$Method <- "M4"
d4$Structure <- "(500,20)"
d4$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,5]
SE <- a[[4]][[5]][,11]
relative_bias <- abs(((a[[4]][[5]][,5]-0.4055)/0.4055))

d5 <- data.frame(count,theta,SE, relative_bias)
d5$Method <- "M5"
d5$Structure <- "(500,20)"
d5$Surgeon <- "OR = 2.5"

#data
count <- c(1:1000)
theta <- a[[4]][[5]][,6]
SE <- a[[4]][[5]][,12]
relative_bias <- abs(((a[[4]][[5]][,6]-0.4055)/0.4055))

d6 <- data.frame(count,theta,SE, relative_bias)
d6$Method <- "M6"
d6$Structure <- "(500,20)"
d6$Surgeon <- "OR = 2.5"

final_500 <- rbind(a1,a2,a3,a4,a5,a6,b1,b2,b3,b4,b5,b6,c1,c2,c3,c4,c5,c6,d1,d2,d3,d4,d5,d6)

final_all <- rbind(final_10,final_50,final_100,final_200,final_500)

library(rsimsum)
library(ggplot2)
library(dplyr)

s1 <- simsum(
  data = final_all, estvarname = "theta", se = "SE", true = 0.4055,
  methodvar = "Method", by = c("Structure","Surgeon"), x = TRUE, ref = "M1")


s2 <- simsum(
  data = final_all, estvarname = "relative_bias", se = "SE", true = 0,
  methodvar = "Method", by = c("Structure","Surgeon"), x = TRUE, ref = "M1")

test <- summary(s2)
dataset <- test$summ
dataset$Method <- factor(dataset$Method, levels = c("M1",
                                                    "M2",
                                                    "M3",
                                                    "M4",
                                                    "M5",
                                                    "M6"))
dataset$Structure<-factor(dataset$Structure, levels=c("(10,1000)",
                                              "(50,200)",
                                              "(100,100)",
                                              "(200,50)",
                                              "(500,20)"
))


library(dplyr)
#figure 5.1
ggplot(data=filter(dataset,stat == "bias"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias %  (95% CI)")+
  scale_y_continuous(limits = c(0,0.5))+
  ggtitle("Relative bias")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))

#figure 5.7
ggplot(data=filter(dataset,stat == "bias"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on outcome (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias %  (95% CI)")+
  scale_y_continuous(limits = c(0.05,0.35))+
  ggtitle("Relative bias")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))

#figure 5.4
ggplot(data=filter(dataset,stat == "bias"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias % (95% CI)")+
  scale_y_continuous(limits = c(0,0.5))+
  ggtitle("Relative bias - different OR on treatment allocation")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))

#figure 5.10
ggplot(data=filter(dataset,stat == "bias"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias % (95% CI)")+
  scale_y_continuous(limits = c(0.05,0.35))+
  ggtitle("Relative bias - different OR on treatment outcome")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))

test1 <- summary(s1)
dataset2 <- test1$summ
dataset2$Method <- factor(dataset$Method, levels=c("M1",
                                                  "M2",
                                                  
                                                  "M3",
                                                  "M4",
                                                  "M5",
                                                  "M6"
))
dataset2$Structure<-factor(dataset$Structure, levels=c("(10,1000)",
                                                      "(50,200)",
                                                      "(100,100)",
                                                      "(200,50)",
                                                      "(500,20)"
))

#figure 5.2
ggplot(data=filter(dataset2,stat == "empse"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Empirical standard error%  (95% CI)")+
  scale_y_continuous(limits = c(0.02,0.1))+
  ggtitle("Empirical standard error")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))

#figure 5.8
ggplot(data=filter(dataset2,stat == "empse"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on outcome (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Empirical standard error%  (95% CI)")+
  scale_y_continuous(limits = c(0.02,0.08))+
  ggtitle("Empirical standard error")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))

#figure 5.3
ggplot(data=filter(dataset2,stat == "cover"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI model coverage %  (95% CI)")+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  ggtitle("95% CI model coverage")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))+geom_hline(yintercept=0.95, linetype="dashed", 
                                                    color = "black", size=1)

#figure 5.9
ggplot(data=filter(dataset2,stat == "cover"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on outcome (Odd Ratio)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI model coverage %  (95% CI)")+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  ggtitle("95% CI model coverage")+
  facet_wrap(~Structure,nrow=3) + theme(legend.position = c(0.75, 0.15),plot.title = element_text(face="bold"))+ 
  guides(color = guide_legend(nrow = 2))+geom_hline(yintercept=0.95, linetype="dashed", 
                                                    color = "black", size=1)


#figure 5.5
ggplot(data=filter(dataset2,stat == "empse"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Empirical standard error%  (95% CI)")+
  scale_y_continuous(limits = c(0.02,0.1))+
  ggtitle("Empirical standard error - different OR on treatment allocation")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))

#figure 5.11
ggplot(data=filter(dataset2,stat == "empse"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Empirical standard error%  (95% CI)")+
  scale_y_continuous(limits = c(0.02,0.08))+
  ggtitle("Empirical standard error - different OR on treatment outcome")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))

#figure 5.6
ggplot(data=filter(dataset2,stat == "cover"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI model coverage%  (95% CI)")+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  ggtitle("95% CI model coverage - different OR on treatment allocation")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))+geom_hline(yintercept=0.95, linetype="dashed", 
                                                                                         color = "black", size=1)
#figure 5.12
ggplot(data=filter(dataset2,stat == "cover"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= Method, color=Method, shape = Method)) +
  geom_line(lty=2, size =1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI model coverage%  (95% CI)")+
  scale_y_continuous(limits = c(0,1),labels = scales::percent)+
  ggtitle("95% CI model coverage - different OR on treatment outcome")+
  facet_wrap(~Surgeon,nrow=3) + theme(plot.title = element_text(face="bold"))+geom_hline(yintercept=0.95, linetype="dashed", 
                                                                                         color = "black", size=1)