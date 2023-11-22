#Import library
library(lme4)
library(foreach)
library(doParallel)
library(simstudy)
##Setting cores for parallel computings
cores <- detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)
##set seeds
set.seed(123)
seed<-sample(1:99999,1000)
##define cluster size and number
cluster_number <- c(10, 50, 100,200,500)
cluster_size <- c(1000, 200,100,50,20)
effect_size <- c(0.01,0.2231,0.4055,0.9163)
#saveRDS(allocation_data, file = "allocation_data.rds")
#1.6094
aa = list()#define result table
bb = list()#define result table
for (k in 1:4){
for (j in 1:length(cluster_number)){j
  ##nam <- paste("summary.results", j, sep = ".")

##  Output <- for (i = 1:1){
  Output <- foreach(i=1:1000, .combine=cbind) %dopar% {
    set.seed(seed[i])
    print(i)
    summary<-matrix (0, nrow=1000,ncol=4)
    library(simstudy)
    library(grf)
    library(boot)


    gen.s <- defData(varname = "c0", formula = 0, variance = 0.5, id = "grp")
    gen.s <- defData(gen.s, varname = "e0", dist = "normal", formula = 0, variance = 0.5)
    gen.s <- defData(gen.s, varname = "z1", dist = "normal", formula = 0, variance = 1)
    gen.s <- defData(gen.s, varname = "z2", dist = "binary", formula = "0.5", link = "identity")
    gen.s <- defData(gen.s, varname = "cluster_size", dist = "noZeroPoisson", formula = cluster_size[j])
    SL <- genData(cluster_number[j], gen.s)



    #Adding patients level data
    gen.p <- defDataAdd(varname = "x1", dist = "binary", formula = "0.4", link = "identity")
    gen.p <- defDataAdd(gen.p,varname = "x2", dist = "binary", formula = "0.45", link = "identity")
    gen.p <- defDataAdd(gen.p,varname = "x3", dist = "binary", formula = "0.5", link = "identity")
    gen.p <- defDataAdd(gen.p,varname = "x4", dist = "normal", formula = 0, variance = 1)
    gen.p <- defDataAdd(gen.p,varname = "x5", dist = "normal", formula = 0, variance = 1)
    gen.p <- defDataAdd(gen.p,varname = "x6", dist = "binary", formula = "0.5", link = "identity")
    gen.p <- defDataAdd(gen.p,varname = "x7", dist = "binary", formula = "0.5", link = "identity")
    gen.p <-defDataAdd(gen.p,varname = "es",formula=effect_size[k])
    cluster <- genCluster(dtClust = SL, cLevelVar = "grp",
                          numIndsVar = "cluster_size", level1ID = "id")#define patients in each cluster

    cluster <- addColumns(gen.p, cluster)


    #Treatment allocation variable
    gen.p <- defDataAdd(varname = "log_T", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x6+es*(z1+z2)+0.4055*(z1*x1)+c0")
    gen.p <- defDataAdd(gen.p, varname = "T_score", formula = "exp(log_T)/(1+exp(log_T))")
    gen.p <- defData(gen.p, varname = "T", dist = "binary", formula = "log_T", link = "logit")
    #Outcome d7 0.01 OR = 1.01
    gen.p <- defDataAdd(gen.p, varname = "log_Y", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x7+0.4055*(z1+z2)+e0+0.4055*T")
    gen.p <- defDataAdd(gen.p, varname = "Y1_score", formula = "exp(log_Y)/(1+exp(log_Y))")
    gen.p <- defData(gen.p, varname = "Y1", dist = "binary", formula = "log_Y", link = "logit")


    data <- addColumns(gen.p, cluster)

    # ##define model 1, 2, 3
    #
    # model1 <-
    #   glm(T ~ x1 + x2 + x3 + x4 + x5, data = data, family = binomial)
    #
    # model2 <-
    #   glm(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2, data = data, family = binomial)
    #
    # model3 <-
    #   glm(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 + z1 * z2,
    #       data = data,
    #       family = binomial)
    #
    # model4 <- glmer(T ~ x1 + x2 + x3 + x4 + x5 + (1 | grp),
    #                 data = data,
    #                 family = binomial)
    #
    # model5 <- glmer(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 + (1 |
    #                                                           grp),
    #                 data = data,
    #                 family = binomial)
    #
    # model6 <-
    #   glmer(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 + z1 * z2 + (1 | grp),
    #         data = data,
    #         family = binomial)
    #
    #
    # ##summary_1[[i]]<-model$coefficients
    #
    # ##stablise IPW calculation
    # P_score1 <- predict(model1, type = "response")
    # P_score2 <- predict(model2, type = "response")
    # P_score3 <- predict(model3, type = "response")
    # P_score4 <- predict(model4, type = "response")
    # P_score5 <- predict(model5, type = "response")
    # P_score6 <- predict(model6, type = "response")
    #
    # marginal_weight <- sum(data$T) / length(data$T)
    # ##IPW for model 1
    # data$P_score1 <- P_score1
    # data$P_score1a <- marginal_weight / data$P_score1
    # data$IPW1 <- (1 - marginal_weight) / (1 - data$P_score1)
    # data$IPW1[data$T == 1] <-  data$P_score1a[data$T == 1]
    # ##IPW for model 2
    # data$P_score2 <- P_score2
    # data$P_score2a <- marginal_weight / data$P_score2
    # data$IPW2 <- (1 - marginal_weight) / (1 - data$P_score2)
    # data$IPW2[data$T == 1] <-  data$P_score2a[data$T == 1]
    # ##IPW for model 3
    # data$P_score3 <- P_score3
    # data$P_score3a <- marginal_weight / data$P_score3
    # data$IPW3 <- (1 - marginal_weight) / (1 - data$P_score3)
    # data$IPW3[data$T == 1] <-  data$P_score3a[data$T == 1]
    # ##IPW for model 4
    # data$P_score4 <- P_score4
    # data$P_score4a <- marginal_weight / data$P_score4
    # data$IPW4 <- (1 - marginal_weight) / (1 - data$P_score4)
    # data$IPW4[data$T == 1] <-  data$P_score4a[data$T == 1]
    # ##IPW for model 5
    # data$P_score5 <- P_score5
    # data$P_score5a <- marginal_weight / data$P_score5
    # data$IPW5 <- (1 - marginal_weight) / (1 - data$P_score5)
    # data$IPW5[data$T == 1] <-  data$P_score5a[data$T == 1]
    # ##IPW for model 6
    # data$P_score6 <- P_score6
    # data$P_score6a <- marginal_weight / data$P_score6
    # data$IPW6 <- (1 - marginal_weight) / (1 - data$P_score6)
    # data$IPW6[data$T == 1] <-  data$P_score6a[data$T == 1]
    #
    # data$w_1 <- 1
    #
    #
    # ##Working out treatment effects
    # #model 1
    # ##d7 0.01
    # model1.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW1)
    #
    # kd$logLik
    # #model 2
    # ##d7 0.01
    # model2.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW2)
    #
    #
    # #model 3
    # ##d7 0.01
    # model3.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW3)
    #
    # #model 4
    # ##d7 0.01
    # model4.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW4)
    #
    # #model 5
    # ##d7 0.01
    # model5.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW5)
    #
    # #model 6
    # ##d7 0.01
    # model6.1 <- glmer(Y1 ~ T + (1 | grp),
    #                   data = data,
    #                   family = binomial,
    #                   weights = IPW6)

    cau.forest.1 <- function(data, i) {
      # These are estimates of m(X) = E[Y | X]
      forest.Y <-
        regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1,num.trees = 200)
      Y.hat <- predict(forest.Y)$predictions

      # These are estimates of the propensity score E[W | X]
      forest.W <-
        regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$T,num.trees = 200)
      W.hat <- predict(forest.W)$predictions

      c.forest <-
        causal_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, data[i]$T, Y.hat, W.hat,num.trees = 200)
      tau.hat <- predict(c.forest)$predictions
      # E[Y | X, W = 0]
      mu.hat.0 <- Y.hat - W.hat * tau.hat
      # E[Y | X, W = 1]
      mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
      te <- mean(mu.hat.0 / (1 - mu.hat.0))
      te.2 <- mean(mu.hat.1 / (1 - mu.hat.1))
      log(te.2 / te)
    }

    cau.forest.2 <- function(data, i) {
      # These are estimates of m(X) = E[Y | X]
      forest.Y <-
        regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, clusters = data[i]$grp,num.trees = 200)
      Y.hat <- predict(forest.Y)$predictions

      # These are estimates of the propensity score E[W | X]
      forest.W <-
        regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$T, clusters = data[i]$grp,num.trees = 200)
      W.hat <- predict(forest.W)$predictions

      c.forest <-
        causal_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, data[i]$T, Y.hat, W.hat, clusters = data[i]$grp,num.trees = 200)
      tau.hat <- predict(c.forest)$predictions
      # E[Y | X, W = 0]
      mu.hat.0 <- Y.hat - W.hat * tau.hat
      # E[Y | X, W = 1]
      mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
      te <- mean(mu.hat.0 / (1 - mu.hat.0))
      te.2 <- mean(mu.hat.1 / (1 - mu.hat.1))
      log(te.2 / te)
    }
    #load boot library
  model.1 <- boot(data, cau.forest.1,R = 20)
 model.2 <- boot(data, cau.forest.2,R = 20)


  # Results table
 summary[i,] <- c(model.1$t0,apply(model.1$t,2,sd,na.rm=TRUE)[1],model.2$t0,apply(model.2$t,2,sd,na.rm=TRUE)[1])
  }
  # Results table

  A <- t(Output)

  aa[[j]] <- A # save results as list
}

  bb[[4]] <- aa

}

saveRDS(bb, file = "causal_allocation.rds")

saveRDS(allocation_data, file = "allocation_data.rds")



# Train a causal forest.
n <- 50
p <- 10
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)
c.forest <- causal_forest(X, Y, W)

saveRDS(allocation_data,file = "allocation_data")
#sim_data <- readRDS(file = "simulation_data.rds")
sim_data <- causal_output
causal_output <- readRDS(file = "causal_100.rds")

causal_output <- readRDS(file = "causal_50k.rds")

sim_data_input <- causal_output

data_test <- sim_data[[4]][[1]][[1]]
testing_temp[[5]][[1]]

run_cau_forest <- function(data){

cau.forest.1 <- function(data, i) {
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1,num.trees = 100)
  Y.hat <- predict(forest.Y)$predictions

  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$T,num.trees = 100)
  W.hat <- predict(forest.W)$predictions

  c.forest <-
    causal_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, data[i]$T, Y.hat, W.hat,num.trees = 100)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}

cau.forest.2 <- function(data, i) {
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, clusters = data[i]$grp,num.trees = 100)
  Y.hat <- predict(forest.Y)$predictions

  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$T, clusters = data[i]$grp,num.trees = 100)
  W.hat <- predict(forest.W)$predictions

  c.forest <-
    causal_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, data[i]$T, Y.hat, W.hat, clusters = data[i]$grp,num.trees = 100)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <-  mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}

cau.forest.3 <- function(data, i) {
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1,num.trees = 100)
  Y.hat <- predict(forest.Y)$predictions

  # These are estimates of the propensity score E[W | X]
  forest.W <-
    glm(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 +z1*z2,
          data = data[i],
          family = binomial)
  W.hat <- predict(forest.W, type = "response")
  c.forest <-
    causal_forest(data[i][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i]$Y1, data[i]$T, Y.hat, W.hat,num.trees = 100)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <-  mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}
#load boot library
model.1 <- boot(data, cau.forest.1,R = 20)
model.2 <- boot(data, cau.forest.2,R = 20)
model.3 <- boot(data, cau.forest.3,R = 20)


# Results table
result_out <-
  c(
    model.1$t0,
    apply(model.1$t, 2, sd, na.rm = TRUE)[1],
    model.2$t0,
    apply(model.2$t, 2, sd, na.rm = TRUE)[1],
    model.3$t0,
    apply(model.3$t, 2, sd, na.rm = TRUE)[1]
  )



}

run_cau_forest_imp <- function(data){
  data <- sim_data[[2]][[2]][[1]]
  c.forest.1 <-
    causal_forest(data[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data$Y1, data$T,num.trees = 100)

  c.forest.2 <-
    causal_forest(data[, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data$Y1, data$T, clusters = data$grp ,num.trees = 100)

  vi1 <- t(variable_importance(c.forest.1))
  vi2 <- t(variable_importance(c.forest.2))



  # Results table
  result_out <- c(vi1,vi2)


}
testing_temp <- list()
testing_final <- list()

for (f in 1:1){

for (k in 1:2){
sim_data_input <- sim_data[[f]][[k]]
testing <- lapply(sim_data_input[1:100],run_cau_forest)
testing_final[[k]] <- testing
}

  testing_temp[[f]] <- testing_final
}

saveRDS(testing_temp, file = "causal_rem_model_50k.rds")

for (f in 2:2){

  for (k in 1:2){
    sim_data_input <- sim_data[[f]][[k]]
    testing <- lapply(sim_data_input[1:100],run_cau_forest)
    testing_final[[k]] <- testing
  }

  testing_temp[[f]] <- testing_final
}
saveRDS(testing_temp, file = "causal_rem_model_50k.rds")

for (f in 3:3){

  for (k in 1:5){
    sim_data_input <- sim_data[[f]][[k]]
    testing <- lapply(sim_data_input[1:100],run_cau_forest)
    testing_final[[k]] <- testing
  }

  testing_temp[[f]] <- testing_final
}
saveRDS(testing_temp, file = "causal_rem_model_50k.rds")

for (f in 4:4){

  for (k in 3:5){
    sim_data_input <- sim_data[[f]][[k]]
    testing <- lapply(sim_data_input[1:100],run_cau_forest)
    testing_final[[k]] <- testing
  }

  testing_temp[[f]] <- testing_final
}

saveRDS(testing_temp, file = "causal_rem_model_50k.rds")






saveRDS(testing_temp, file = "causal_rem_model.rds")
library(purrr)

testing_temp[[2]]

testing_temp_list1 <- list()
testing_temp_list <- list()

for (f in 1:5){

  for (k in 1:5){

    testing_temp_list1[[k]] <-  t(matrix(unlist(testing_temp[[f]][[k]]),ncol = 100))

  }

  testing_temp_list[[f]] <- testing_temp_list1

}


saveRDS(testing_temp_list, file = "causal_important.rds")

testing_temp_list <- readRDS(file = "causal_important.rds")

colMeans(testing_temp_list[[5]][[4]])

####################################

library(simstudy)

aa = list()#define result table
bb = list()#define result table
data_i = list()
for (k in 1:4){

  for (j in 1:5){

    for (i in 1:100){
    set.seed(seed[i])
gen.s <- defData(varname = "c0", formula = 0, variance = 0.5, id = "grp")
gen.s <- defData(gen.s, varname = "e0", dist = "normal", formula = 0, variance = 0.5)
gen.s <- defData(gen.s, varname = "z1", dist = "normal", formula = 0, variance = 1)
gen.s <- defData(gen.s, varname = "z2", dist = "binary", formula = "0.5", link = "identity")
gen.s <- defData(gen.s, varname = "cluster_size", dist = "noZeroPoisson", formula = cluster_size[j])
SL <- genData(cluster_number[j], gen.s)



#Adding patients level data
gen.p <- defDataAdd(varname = "x1", dist = "binary", formula = "0.4", link = "identity")
gen.p <- defDataAdd(gen.p,varname = "x2", dist = "binary", formula = "0.45", link = "identity")
gen.p <- defDataAdd(gen.p,varname = "x3", dist = "binary", formula = "0.5", link = "identity")
gen.p <- defDataAdd(gen.p,varname = "x4", dist = "normal", formula = 0, variance = 1)
gen.p <- defDataAdd(gen.p,varname = "x5", dist = "normal", formula = 0, variance = 1)
gen.p <- defDataAdd(gen.p,varname = "x6", dist = "binary", formula = "0.5", link = "identity")
gen.p <- defDataAdd(gen.p,varname = "x7", dist = "binary", formula = "0.5", link = "identity")
gen.p <-defDataAdd(gen.p,varname = "es",formula=effect_size[k])
cluster <- genCluster(dtClust = SL, cLevelVar = "grp",
                      numIndsVar = "cluster_size", level1ID = "id")#define patients in each cluster

cluster <- addColumns(gen.p, cluster)


#Treatment allocation variable
gen.p <- defDataAdd(varname = "log_T", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x6+es*(z1+z2)+0.4055*(z1*x1)+c0")
gen.p <- defDataAdd(gen.p, varname = "T_score", formula = "exp(log_T)/(1+exp(log_T))")
gen.p <- defData(gen.p, varname = "T", dist = "binary", formula = "log_T", link = "logit")
#Outcome d7 0.01 OR = 1.01
gen.p <- defDataAdd(gen.p, varname = "log_Y", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x7+0.4055*(z1+z2)+e0+0.4055*T")
gen.p <- defDataAdd(gen.p, varname = "Y1_score", formula = "exp(log_Y)/(1+exp(log_Y))")
gen.p <- defData(gen.p, varname = "Y1", dist = "binary", formula = "log_Y", link = "logit")


data <- addColumns(gen.p, cluster)

data_i[[i]] <- data

}

aa[[j]] <- data_i

  }

  bb[[k]] <- aa

}

saveRDS(bb,file = "simulation_data_allocation2.rds")




run_cau_forest(data_test)

run_cau_forest <- function(data){




#
  cau.forest.1 <- function(data, i) {
    # These are estimates of m(X) = E[Y | X]
    forest.Y <-
      regression_forest(data[i,][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i,]$Y1, clusters = data[i,]$grp,num.trees = 100)
    Y.hat <- predict(forest.Y)$predictions

    # These are estimates of the propensity score E[W | X]
    forest.W <-
      glm(T ~ x1 + x2 + x3 + x4 + x5 + z1 + z2 +z1*z2,
            data = data[i,],
            family = binomial)
    W.hat <- predict(forest.W, type = "response")
    c.forest <-
    causal_forest(data[i,][, c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "z1", "z2")], data[i,]$Y1, data[i,]$T, Y.hat, W.hat,num.trees = 100)
    tau.hat <- predict(c.forest)$predictions
    # E[Y | X, W = 0]
    mu.hat.0 <- Y.hat - W.hat * tau.hat
    # E[Y | X, W = 1]
    mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
    te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
    te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
    log(te.2 / te)
  }
  #load boot library
  model.1 <- boot(data, cau.forest.1,R = 20)


  # Results table
  result_out <- c(model.1$t0,apply(model.1$t,2,sd,na.rm=TRUE)[1])



}



#################plasmode code#######################################################################################
run_cau_forest_plasmode <- function(data){




  cau.forest.1 <- function(data, i) {

    # These are estimates of m(X) = E[Y | X]
    forest.Y <-
      regression_forest(data[i,][,confounders], data[i,]$EVENT1,num.trees = 100)
    Y.hat <- predict(forest.Y)$predictions

    # These are estimates of the propensity score E[W | X]
    forest.W <-
      regression_forest(data[i,][, confounders], data[i,]$EXPOSURE1,num.trees = 100)
    W.hat <- predict(forest.W)$predictions

    c.forest <-
      causal_forest(data[i,][, confounders], data[i,]$EVENT1, data[i,]$EXPOSURE1, Y.hat, W.hat,num.trees = 100)
    tau.hat <- predict(c.forest)$predictions
    # E[Y | X, W = 0]
    mu.hat.0 <- Y.hat - W.hat * tau.hat
    # E[Y | X, W = 1]
    mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
    te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
    te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
    log(te.2 / te)
  }
  cau.forest.2 <- function(data, i) {
    # These are estimates of m(X) = E[Y | X]

    forest.Y <-
      regression_forest(data[i,][, confounders], data[i,]$EVENT1, clusters = data[i,]$prov_id,num.trees = 100)
    Y.hat <- predict(forest.Y)$predictions

    # These are estimates of the propensity score E[W | X]
    forest.W <-
      regression_forest(data[i,][, confounders], data[i,]$EXPOSURE1, clusters = data[i,]$prov_id,num.trees = 100)
    W.hat <- predict(forest.W)$predictions

    c.forest <-
      causal_forest(data[i,][, confounders], data[i,]$EVENT1, data[i,]$EXPOSURE1, Y.hat, W.hat, clusters = data[i,]$prov_id,num.trees = 100)
    tau.hat <- predict(c.forest)$predictions
    # E[Y | X, W = 0]
    mu.hat.0 <- Y.hat - W.hat * tau.hat
    # E[Y | X, W = 1]
    mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
    te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
    te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
    log(te.2 / te)
  }
  cau.forest.3 <- function(data, i) {
    # These are estimates of m(X) = E[Y | X]
    forest.Y <-
      regression_forest(data[i,][, confounders], data[i,]$EXPOSURE1, clusters = data[i,]$prov_id, num.trees = 100)
    Y.hat <- predict(forest.Y)$predictions

    # These are estimates of the propensity score E[W | X]
    forest.W <-
      glm(ps_form,
            data = data[i,],
            family = binomial)
    W.hat <- predict(forest.W, type = "response")
    c.forest <-
      causal_forest(data[i,][, confounders], data[i,]$EVENT1, data[i,]$EXPOSURE1, Y.hat, W.hat, clusters = data[i,]$prov_id,num.trees = 100)
    tau.hat <- predict(c.forest)$predictions
    # E[Y | X, W = 0]
    mu.hat.0 <- Y.hat - W.hat * tau.hat

    # E[Y | X, W = 1]
    mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat


    te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
    te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
    log(te.2 / te)
  }
  #load boot library
  model.1 <- boot(data, cau.forest.1,R = 20)
  model.2 <- boot(data, cau.forest.1,R = 20)
  model.3 <- boot(data, cau.forest.1,R = 20)
  #ps weighting

  ps1 <- glm(ps_form, data = data, family = binomial)
  ps2 <- glm(ps_form2, data = data, family = binomial)

  ##stablise IPW calculation
  P_score1 <- predict(ps1, type = "response")
  P_score2 <- predict(ps2, type = "response")


  marginal_weight <- sum(data$EXPOSURE1)/length(data$EXPOSURE1)

  ##IPW for model 1
  data$P_score1 <- P_score1
  data$P_score1a <- marginal_weight/data$P_score1
  data$IPW1 <- (1 - marginal_weight)/(1-data$P_score1)
  data$IPW1[data$EXPOSURE1==1] <-  data$P_score1a[data$EXPOSURE1==1]
  ##IPW for model 2
  data$P_score2 <- P_score2
  data$P_score2a <- marginal_weight/data$P_score2
  data$IPW2 <- (1 - marginal_weight)/(1-data$P_score2)
  data$IPW2[data$EXPOSURE1==1] <-  data$P_score2a[data$EXPOSURE1==1]

  model1.1 <- glmer(EVENT1 ~ EXPOSURE1 + (1 | prov_id),
                  data = data,
                  family = binomial, weights = IPW1)
  ##d7 0.2231
  model1.2 <- glmer(EVENT1 ~ EXPOSURE1 + (1 | prov_id),
                  data = data,
                  family = binomial, weights = IPW2)
  # Results table
  result_out <-
    c(
      model.1$t0,
      apply(model.1$t, 2, sd, na.rm = TRUE)[1],
      model.2$t0,
      apply(model.2$t, 2, sd, na.rm = TRUE)[1],
      model.3$t0,
      apply(model.3$t, 2, sd, na.rm = TRUE)[1],
      (summary(model1.1)$coefficients)[2,1],
      (summary(model1.1)$coefficients)[2,2],
      (summary(model1.2)$coefficients)[2,1],
      (summary(model1.2)$coefficients)[2,2]
    )

}

run_psw_plasmode <- function(data){

  #ps weighting

  ps1 <- glm(ps_form, data = data, family = binomial)
  ps2 <- glm(ps_form2, data = data, family = binomial)

  ##stablise IPW calculation
  P_score1 <- predict(ps1, type = "response")
  P_score2 <- predict(ps2, type = "response")


  marginal_weight <- sum(data$EXPOSURE1)/length(data$EXPOSURE1)

  ##IPW for model 1
  data$P_score1 <- P_score1
  data$P_score1a <- marginal_weight/data$P_score1
  data$IPW1 <- (1 - marginal_weight)/(1-data$P_score1)
  data$IPW1[data$EXPOSURE1==1] <-  data$P_score1a[data$EXPOSURE1==1]
  ##IPW for model 2
  data$P_score2 <- P_score2
  data$P_score2a <- marginal_weight/data$P_score2
  data$IPW2 <- (1 - marginal_weight)/(1-data$P_score2)
  data$IPW2[data$EXPOSURE1==1] <-  data$P_score2a[data$EXPOSURE1==1]

  model1.1 <- glmer(EVENT1 ~ EXPOSURE1 + (1 | prov_id),
                    data = data,
                    family = binomial, weights = IPW1)
  ##d7 0.2231
  model1.2 <- glmer(EVENT1 ~ EXPOSURE1 + (1 | prov_id),
                    data = data,
                    family = binomial, weights = IPW2)
  # Results table
  result_out <-
    c(
      (summary(model1.1)$coefficients)[2,1],
      (summary(model1.1)$coefficients)[2,2],
      (summary(model1.2)$coefficients)[2,1],
      (summary(model1.2)$coefficients)[2,2]
    )

}

plasmode_result_cf <- list()

for (i in 1:1){

sim_data_input <- plasmode_data[[i]]
testing <- lapply(sim_data_input[1:1],run_cau_forest_plasmode)
plasmode_result_cf[[i]] <- testing

}
saveRDS(plasmode_result_cf,file = "plasmode_result_cf.rds")
for (i in 2:2){

  sim_data_input <- plasmode_data[[i]]
  testing <- lapply(sim_data_input[1:100],run_cau_forest_plasmode)
  plasmode_result_cf[[i]] <- testing

}
saveRDS(plasmode_result_cf,file = "plasmode_result_cf.rds")
for (i in 3:3){

  sim_data_input <- plasmode_data[[i]]
  testing <- lapply(sim_data_input[1:100],run_cau_forest_plasmode)
  plasmode_result_cf[[i]] <- testing

}
saveRDS(plasmode_result_cf,file = "plasmode_result_cf.rds")
for (i in 4:4){

  sim_data_input <- plasmode_data[[i]]
  testing <- lapply(sim_data_input[1:100],run_cau_forest_plasmode)
  plasmode_result_cf[[i]] <- testing

}
saveRDS(plasmode_result_cf,file = "plasmode_result_cf.rds")
plasmode_result_psw <- list()

for (i in 1:4){

  sim_data_input <- plasmode_data[[i]]
  testing <- lapply(sim_data_input[1:100],run_psw_plasmode)
  plasmode_result_psw[[i]] <- testing

}
saveRDS(plasmode_result_psw,file = "plasmode_result_psw.rds")

plasmode_result_psw
ps_form <-
  EXPOSURE1 ~ urban_flag + surg_vol + beds_flag + prov_region_MIDWEST +
  prov_region_NORTHEAST + prov_region_SOUTH + prov_region_WEST + gender_flag + cm_cci + cm_elix + los + patient_age +
                cci05_dem +               cci06_cpd +               cci07_ra +
  cci08_ulcer +              cci09_liv  +              cci10_diab +              cci11_diabc +
  cci12_para +              cci13_renal +             cci14_can +               cci15_livc +
  cci16_can2  +             cci17_hiv  +              elx01_chf +               elx02_arrhy +
  elx03_valv  +             elx04_pcd +               elx05_pvd +               elx06_htn +
  elx07_htnc +              elx08_para  +             elx09_neur  +             elx10_cpd +
  elx11_diab   +            elx12_diabc  +            elx13_thyr +              elx14_renal +
  elx15_liv  +              elx16_ulcer +             elx17_hiv  +              elx18_lym +
  elx19_can2  +             elx20_can  +              elx21_ra +                elx22_coag +
  elx23_obe  +             elx24_wtl   +             elx25_fed  +              elx26_bla +
  elx27_dfa +              elx28_alc   +             elx29_dabu   +            elx30_psy +
  elx31_depr +              cancer_c250pancreatric +   cancer_c251pancreatric +  cancer_c252pancreatric +
  cancer_c253pancreatric +   cancer_c254pancreatric +  cancer_c257pancreatric +  cancer_c258pancreatric +
  phy_spec_other + phy_spec_so

confounders <- Cs(urban_flag, surg_vol ,beds_flag ,prov_region_MIDWEST ,
  prov_region_NORTHEAST ,prov_region_SOUTH, prov_region_WEST , gender_flag , cm_cci , cm_elix , los , patient_age ,
  cci05_dem ,               cci06_cpd ,               cci07_ra ,
  cci08_ulcer ,              cci09_liv  ,              cci10_diab ,              cci11_diabc ,
  cci12_para ,              cci13_renal ,             cci14_can ,               cci15_livc ,
  cci16_can2  ,             cci17_hiv  ,              elx01_chf ,               elx02_arrhy ,
  elx03_valv  ,             elx04_pcd ,               elx05_pvd ,               elx06_htn ,
  elx07_htnc ,              elx08_para  ,             elx09_neur  ,             elx10_cpd ,
  elx11_diab   ,            elx12_diabc  ,            elx13_thyr ,              elx14_renal ,
  elx15_liv  ,              elx16_ulcer ,             elx17_hiv  ,              elx18_lym ,
  elx19_can2  ,             elx20_can  ,              elx21_ra ,                elx22_coag ,
  elx23_obe  ,             elx24_wtl   ,             elx25_fed  ,              elx26_bla ,
  elx27_dfa ,              elx28_alc   ,             elx29_dabu   ,            elx30_psy ,
  elx31_depr ,              cancer_c250pancreatric ,   cancer_c251pancreatric ,  cancer_c252pancreatric ,
  cancer_c253pancreatric ,   cancer_c254pancreatric ,  cancer_c257pancreatric ,  cancer_c258pancreatric ,
  phy_spec_other , phy_spec_so , phy_spec_tts)

ps_form2 <-
  EXPOSURE1 ~ gender_flag + cm_cci + cm_elix + los + patient_age +
  cci05_dem +               cci06_cpd +               cci07_ra +
  cci08_ulcer +              cci09_liv  +              cci10_diab +              cci11_diabc +
  cci12_para +              cci13_renal +             cci14_can +               cci15_livc +
  cci16_can2  +             cci17_hiv  +              elx01_chf +               elx02_arrhy +
  elx03_valv  +             elx04_pcd +               elx05_pvd +               elx06_htn +
  elx07_htnc +              elx08_para  +             elx09_neur  +             elx10_cpd +
  elx11_diab   +            elx12_diabc  +            elx13_thyr +              elx14_renal +
  elx15_liv  +              elx16_ulcer +             elx17_hiv  +              elx18_lym +
  elx19_can2  +             elx20_can  +              elx21_ra +                elx22_coag +
  elx23_obe  +             elx24_wtl   +             elx25_fed  +              elx26_bla +
  elx27_dfa +              elx28_alc   +             elx29_dabu   +            elx30_psy +
  elx31_depr +              cancer_c250pancreatric +   cancer_c251pancreatric +  cancer_c252pancreatric +
  cancer_c253pancreatric +   cancer_c254pancreatric +  cancer_c257pancreatric +  cancer_c258pancreatric +
  phy_spec_other + phy_spec_so
