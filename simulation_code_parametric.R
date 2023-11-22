#Import library
##install.packages("MatchIt")
library(lme4)
library(foreach)
library(doParallel)
library(simstudy)
library("cobalt")
library(MatchIt)
library(parallel)

##Setting cores for parallel computings
cores <- detectCores()
cl <- cores - 1 #not to overload your computer
registerDoParallel(cl)
##set seeds
set.seed(123)
seed<-sample(1:99999,1000)
##define cluster size and number
cluster_number <- c(10, 50, 100,200,500)
cluster_size <- c(1000, 200,100,50,20)
effect_size <- c(0.01,0.2231,0.4055,0.9163)
##effect_size <- c(1,5)
##create results table
##j=1
#1.6094

#varying allocation

aa = list()#define result table
bb = list()#define result table
cc = list()#define result table
for (k in 1:length(effect_size)){
  for (j in 1:length(cluster_size)){
    ##  summary<-matrix (0, nrow=5,ncol=40)
    ##nam <- paste("summary.results", j, sep = ".")
    ##    Output <- foreach(i=26:50, .combine=cbind) %dopar% {
    for (i in 1:1000){
      set.seed(seed[i])
      print(i)
      
      print (i)
      gen.s <- defData(varname = "c0", formula = 0, variance = 0.5, id = "grp")
      gen.s <- defData(gen.s, varname = "e0", dist = "normal", formula = 0, variance = 0.5)
      gen.s <- defData(gen.s, varname = "z1", dist = "normal", formula = 0, variance = 1)
      gen.s <- defData(gen.s, varname = "z2", dist = "binary", formula = "0.5", link = "identity")
      gen.s <- defData(gen.s, varname = "cluster_size", dist = "noZeroPoisson", formula = 5*cluster_size[j])
      SL <- genData(cluster_number[j], gen.s)
      
      
      
      #Adding patients level data
      gen.p <- defDataAdd(varname = "x1", dist = "binary", formula = "0.4", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x2", dist = "binary", formula = "0.45", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x3", dist = "binary", formula = "0.5", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x4", dist = "normal", formula = 0, variance = 1)
      gen.p <- defDataAdd(gen.p,varname = "x5", dist = "normal", formula = 0, variance = 1)
      gen.p <- defDataAdd(gen.p,varname = "cc0", dist = "normal", formula = 0, variance = 0.5)
      gen.p <- defDataAdd(gen.p,varname = "ee0", dist = "normal", formula = 0, variance = 0.5)
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
      
      aa[[i]] <- data
      
    }
    
    bb[[j]] <- aa
    
  }
  
  cc[[k]] <- bb
  
}


#varying outcome

aa_1 = list()#define result table
bb_1 = list()#define result table
cc_1 = list()#define result table
for (k in 1:length(effect_size)){
  for (j in 1:length(cluster_size)){
    ##  summary<-matrix (0, nrow=5,ncol=40)
    ##nam <- paste("summary.results", j, sep = ".")
    ##    Output <- foreach(i=26:50, .combine=cbind) %dopar% {
    for (i in 1:1000){
      set.seed(seed[i])
      print(i)
      
      print (i)
      gen.s <- defData(varname = "c0", formula = 0, variance = 0.5, id = "grp")
      gen.s <- defData(gen.s, varname = "e0", dist = "normal", formula = 0, variance = 0.5)
      gen.s <- defData(gen.s, varname = "z1", dist = "normal", formula = 0, variance = 1)
      gen.s <- defData(gen.s, varname = "z2", dist = "binary", formula = "0.5", link = "identity")
      gen.s <- defData(gen.s, varname = "cluster_size", dist = "noZeroPoisson", formula = 5*cluster_size[j])
      SL <- genData(cluster_number[j], gen.s)
      
      
      
      #Adding patients level data
      gen.p <- defDataAdd(varname = "x1", dist = "binary", formula = "0.4", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x2", dist = "binary", formula = "0.45", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x3", dist = "binary", formula = "0.5", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x4", dist = "normal", formula = 0, variance = 1)
      gen.p <- defDataAdd(gen.p,varname = "x5", dist = "normal", formula = 0, variance = 1)
      gen.p <- defDataAdd(gen.p,varname = "cc0", dist = "normal", formula = 0, variance = 0.5)
      gen.p <- defDataAdd(gen.p,varname = "ee0", dist = "normal", formula = 0, variance = 0.5)
      gen.p <- defDataAdd(gen.p,varname = "x6", dist = "binary", formula = "0.5", link = "identity")
      gen.p <- defDataAdd(gen.p,varname = "x7", dist = "binary", formula = "0.5", link = "identity")
      gen.p <-defDataAdd(gen.p,varname = "es",formula=effect_size[k])
      cluster <- genCluster(dtClust = SL, cLevelVar = "grp",
                            numIndsVar = "cluster_size", level1ID = "id")#define patients in each cluster
      
      cluster <- addColumns(gen.p, cluster)
      #Treatment allocation variable
      gen.p <- defDataAdd(varname = "log_T", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x6+0.4055*(z1+z2)+0.4055*(z1*x1)+c0")
      gen.p <- defDataAdd(gen.p, varname = "T_score", formula = "exp(log_T)/(1+exp(log_T))")
      gen.p <- defData(gen.p, varname = "T", dist = "binary", formula = "log_T", link = "logit")
      #Outcome d7 0.01 OR = 1.01
      gen.p <- defDataAdd(gen.p, varname = "log_Y", formula = "-0.5+0.35*x1+0.4*x2+0.45*x3+0.5*x4+0.55*x5+0.5*x7+es*(z1+z2)+e0+0.4055*T")
      gen.p <- defDataAdd(gen.p, varname = "Y1_score", formula = "exp(log_Y)/(1+exp(log_Y))")
      gen.p <- defData(gen.p, varname = "Y1", dist = "binary", formula = "log_Y", link = "logit")
      data <- addColumns(gen.p, cluster)
      
      aa_1[[i]] <- data
      
    }
    
    bb_1[[j]] <- aa
    
  }
  
  cc_1[[k]] <- bb
  
}