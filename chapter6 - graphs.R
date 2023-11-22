#graphs for chapter 6


a <- readRDS(file = "card_match_050822a.rds")

stru <- list("(10,1000)","(50,200)","(100,100)","(200,50)","(500,20)")
surg <- list("OR = 1.01","OR = 1.25","OR = 1.5"," OR = 2.5")
final_data <- list()
final_all <- list()
all <- list()


for (j in 1:length(stru)) {
  for (i in 1:length(surg)) {
    #data
    count <- c(1:1000)
    theta <- a[[i]][[j]][, 1]
    SE <- a[[i]][[j]][, 2]
    relative_bias <- abs(((a[[i]][[j]][, 1] - 0.4055) / 0.4055))
    retention <- (a[[i]][[j]][, 4]) / (a[[i]][[j]][, 3])
    a1 <- data.frame(count, theta, SE, relative_bias, retention)
    a1$Method <- "Ref"
    a1$Structure <- stru[[j]]
    a1$Surgeon <- surg[[i]]
    
    #data
    count <- c(1:1000)
    theta <- a[[i]][[j]][, 6]
    SE <- a[[i]][[j]][, 7]
    relative_bias <- abs(((a[[i]][[j]][, 6] - 0.4055) / 0.4055))
    retention <- (a[[i]][[j]][, 9]) / (a[[i]][[j]][, 8])
    a2 <- data.frame(count, theta, SE, relative_bias, retention)
    a2$Method <- "PSM-Across"
    a2$Structure <- stru[[j]]
    a2$Surgeon <- surg[[i]]
    
    #data
    count <- c(1:1000)
    theta <- a[[i]][[j]][, 11]
    SE <- a[[i]][[j]][, 12]
    relative_bias <- abs(((a[[i]][[j]][, 11] - 0.4055) / 0.4055))
    retention <- (a[[i]][[j]][, 14]) / (a[[i]][[j]][, 13])
    a3 <- data.frame(count, theta, SE, relative_bias, retention)
    a3$Method <- "PSM-Within"
    a3$Structure <- stru[[j]]
    a3$Surgeon <- surg[[i]]
    
    #data
    count <- c(1:1000)
    theta <- b[[i]][[j]][, 16]
    SE <- b[[i]][[j]][, 17]
    relative_bias <- abs(((b[[i]][[j]][, 16] - 0.4055) / 0.4055))
    retention <- (b[[i]][[j]][, 19]) / (b[[i]][[j]][, 18])
    a4 <- data.frame(count, theta, SE, relative_bias, retention)
    a4$Method <- "CM-Within"
    a4$Structure <- stru[[j]]
    a4$Surgeon <- surg[[i]]
    
    #data
    count <- c(1:1000)
    theta <- a[[i]][[j]][, 26]
    SE <- a[[i]][[j]][, 27]
    relative_bias <- abs(((a[[i]][[j]][, 26] - 0.4055) / 0.4055))
    retention <- (a[[i]][[j]][, 29]) / (a[[i]][[j]][, 28])
    a6 <- data.frame(count, theta, SE, relative_bias, retention)
    a6$Method <- "CM-Across"
    a6$Structure <- stru[[j]]
    a6$Surgeon <- surg[[i]]
    
    all[[i]] <- rbind(a1, a2, a3, a4, a6)
    
  }
  final_data[[j]] <- all
}



final_all <- final_data %>% purrr::reduce(union) %>% purrr::reduce(union)


final_all =  filter(final_all, theta != 99999)


#############

library(rsimsum)
library(purrr)
library(dplyr)
# Workaround:
res <- map_dfr(.x = unique(final_all$Method), .f = function(m) {
  tmp <- simsum(data = subset(final_all, Method == m), estvarname = "theta", by = c("Structure","Surgeon"), true = 0.4055, se = "SE")
  tmp <- tidy(summary(tmp, stats = c("nsim", "bias", "empse", "cover")))
  tmp[["method"]] <- m
  return(tmp)
})

res1 <- map_dfr(.x = unique(final_all$Method), .f = function(m) {
  tmp <- simsum(data = subset(final_all, Method == m), estvarname = "retention", by = c("Structure","Surgeon"), true = 0, se = "SE")
  tmp <- tidy(summary(tmp, stats = c("nsim", "bias", "empse", "cover")))
  tmp[["method"]] <- m
  return(tmp)
})

res2 <- map_dfr(.x = unique(final_all$Method), .f = function(m) {
  tmp <- simsum(data = subset(final_all, Method == m), estvarname = "relative_bias", by = c("Structure","Surgeon"), true = 0, se = "SE")
  tmp <- tidy(summary(tmp, stats = c("nsim", "bias", "empse", "cover")))
  tmp[["method"]] <- m
  return(tmp)
})


########
res_list <- list(res,res1,res2)

for (i in 1:length(res_list)){
  res_list[[i]]$Structure<-factor(res_list[[i]]$Structure, levels=c("(10,1000)",
                                                                    "(50,200)",
                                                                    "(100,100)",
                                                                    "(200,50)",
                                                                    "(500,20)"
  ))
  res_list[[i]]$Surgeon<-factor(res_list[[i]]$Surgeon, levels=c("OR = 1.01","OR = 1.25","OR = 1.5"," OR = 2.5"
  ))
  res_list[[i]]$method<-factor(res_list[[i]]$method, levels=c("Ref",
                                                              "PSM-Across",
                                                              "CM-Across",
                                                              "PSM-Within",
                                                              "CM-Within"
  ))
}

library(scales)
library(ggplot2)
summary(res2$Method)

test <- res_list[[3]]
#relative bias
ggplot(data=filter(test,stat == "bias"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= method, color=method)) +
  geom_line(lty=2, size =1) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)")+# Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias %  (95% CI)")+
  scale_y_continuous(limits = c(0.05,.85),labels = percent)+
  ggtitle("Treatment Allocation - cluster-level confounders effect on outcome OR = 1.5")+
  facet_wrap(~Structure,nrow=3)+theme(legend.position="bottom")

ggplot(data=filter(test,stat == "bias"), aes(x=Structure, y=est, ymin=lower, ymax=upper, group= method, colour=method)) +
  geom_line(lty=2, size =1) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster Structure (cluster number, cluster size)") + # Label on the Y axis (flipped specification do to coord_flip)
  ylab("Relative Bias % (95% CI)")+
  scale_y_continuous(limits = c(0.05,0.85),labels = percent)+
  ggtitle("Treatment Outcome - Cluster-level confounders effect on allocation OR = 1.5")+
  facet_wrap(~Surgeon,nrow=2)+theme(legend.position="bottom")


test <- res_list[[2]]

#retention
ggplot(data=filter(test,stat == "bias"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= method, color=method)) +
  geom_line(lty=2, size =1) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)")+# Label on the Y axis (flipped specification do to coord_flip)
  ylab("Post match sample retention %  (95% CI)")+
  scale_y_continuous(limits = c(0.2,1),labels = percent)+
  ggtitle("Treatment Allocation - cluster-level confounders effect on outcome OR = 1.5")+
  facet_wrap(~Structure,nrow=3)+theme(legend.position="bottom")


test <- res_list[[1]]

#empse
ggplot(data=filter(test,stat == "empse"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= method, color=method)) +
  geom_line(lty=2, size =1) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)")+# Label on the Y axis (flipped specification do to coord_flip)
  ylab("Empirical standard error  (95% CI)")+
  scale_y_continuous(limits = c(0,0.25))+
  ggtitle("Treatment Allocation - cluster-level confounders effect on outcome OR = 1.5")+
  facet_wrap(~Structure,nrow=3)+theme(legend.position="bottom")

#
ggplot(data=filter(test,stat == "cover"), aes(x=Surgeon, y=est, ymin=lower, ymax=upper, group= method, color=method)) +
  geom_line(lty=2, size =1) +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.05, cex=1) +
  xlab("Cluster-level confounders effect on allocation (Odd Ratio)")+# Label on the Y axis (flipped specification do to coord_flip)
  ylab("95% CI model coverage (95% CI)")+
  scale_y_continuous(limits = c(0,1),labels = percent)+
  ggtitle("Treatment Allocation - cluster-level confounders effect on outcome OR = 1.5")+
  facet_wrap(~Structure,nrow=3)+theme(legend.position="bottom")+ geom_hline(yintercept = 0.95,
                                                                            linetype = "dashed"
                                                                            ,
                                                                            size = 1)
