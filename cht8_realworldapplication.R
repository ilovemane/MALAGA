library(fastDummies)
library(dplyr)
library(Hmisc)
library(cobalt)
library(MatchIt)
library(WeightIt)
library(lme4)

mydata2_plasmode <- mydata %>% filter(procedure_type=="Pancreatic -Whipple" | procedure_type=="Pancreatic -Distal")  %>% filter(cancer_1 == 1) 
#setting exposure and outcome covariate
mydata2_plasmode$outcome<-mydata2_plasmode$any_complication#outcome
##mydata2_plasmode$exposure <- ifelse(mydata2_plasmode$procedure_type == "Pancreatic -Whipple",1,0)#whipple =1 distal=0
mydata2_plasmode$exposure <-  ifelse(mydata2_plasmode$robotics_new == 1,1,0)
#Setting hospital confounders
mydata2_plasmode$urban_flag <- ifelse(mydata2_plasmode$urban_rural == "URBAN",1,0)#urban =1 rural=0
mydata2_plasmode$teach_flag <- ifelse(mydata2_plasmode$teaching == "YES",1,0) # yes=1, No =0
mydata2_plasmode$beds_flag <- ifelse(mydata2_plasmode$beds_grp == "500+",1,0) # 500+ = 1 <500 = 0
mydata2_plasmode$surg_vol <- mydata2_plasmode$provider_yearly_surg_vol # continuous
#Setting patient confounders
mydata2_plasmode$gender_flag <- ifelse(mydata2_plasmode$gender=="M",1,0) # m = 1,0
mydata2_plasmode$patient_age <-mydata2_plasmode$age # age

mydata2_plasmode <- dummy_cols(mydata2_plasmode, select_columns = c('prov_region',"race","proc_phy_spec_desc_grp"),
                               remove_selected_columns = TRUE)

mydata2_plasmode <- mydata2_plasmode %>% 
  dplyr::rename("phy_spec_gs" = "proc_phy_spec_desc_grp_GENERAL SURGERY (GS)"
                ,
                "phy_spec_other" = "proc_phy_spec_desc_grp_Other",
                "phy_spec_so" = "proc_phy_spec_desc_grp_SURGICAL ONCOLOGY (SO)",
                "phy_spec_tts" = "proc_phy_spec_desc_grp_TRANSPLANT SURGERY (TTS)")

nrow(mydata2_plasmode)

confounders <- Cs(urban_flag, surg_vol ,beds_flag ,prov_region_MIDWEST , teach_flag,
                  prov_region_NORTHEAST ,prov_region_SOUTH, prov_region_WEST , gender_flag , cm_cci , cm_elix  , patient_age ,
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
                  phy_spec_other , phy_spec_so , phy_spec_tts , phy_spec_gs ,cci01_mi , cci02_chf,cci03_pvd ,
                    cci04_cvd , race_B , race_O , race_W , race_U , cancer_c259pancreatric)


ps_model <- exposure ~ urban_flag + surg_vol + beds_flag + prov_region_MIDWEST + 
  prov_region_NORTHEAST + prov_region_SOUTH + prov_region_WEST + teach_flag + gender_flag + cm_cci + cm_elix + patient_age +
              cci01_mi +                 cci02_chf +                cci03_pvd +
  cci04_cvd +                cci05_dem +               cci06_cpd +               cci07_ra +
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
  phy_spec_other + phy_spec_so + phy_spec_tts + phy_spec_gs + race_B + race_O + race_W + race_U +
  cancer_c259pancreatric

ps_model2 <- exposure ~ cm_cci + cm_elix + patient_age +
  cci01_mi +                 cci02_chf +                cci03_pvd +
  cci04_cvd +                cci05_dem +               cci06_cpd +               cci07_ra +
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
  phy_spec_other + phy_spec_so + phy_spec_tts + phy_spec_gs + race_B + race_O + race_W + race_U +
  cancer_c259pancreatric

ps_model3 <-  exposure ~ urban_flag + surg_vol + beds_flag + prov_region_MIDWEST + 
  prov_region_NORTHEAST + prov_region_SOUTH + prov_region_WEST + teach_flag + gender_flag + cm_cci + cm_elix + patient_age +
  cci01_mi +  cci02_chf + cci03_pvd + cci06_cpd +  cci09_liv + cci10_diab +  cci11_diabc + cci13_renal + 
  cci16_can2 + elx01_chf +               elx02_arrhy +
  elx03_valv +               elx05_pvd +               elx06_htn +
  elx07_htnc  +             elx09_neur  +             elx10_cpd +
  elx11_diab   +            elx12_diabc  +            elx13_thyr +              elx14_renal +
  elx15_liv  +              elx18_lym +
  elx19_can2  +              elx21_ra +                elx22_coag +
  elx23_obe  +             elx24_wtl   +             elx25_fed +              elx28_alc +
  elx31_depr +             cancer_c250pancreatric +   cancer_c251pancreatric +  cancer_c252pancreatric +
  cancer_c253pancreatric +   cancer_c254pancreatric +  cancer_c257pancreatric +  cancer_c258pancreatric +
  phy_spec_other + phy_spec_so + phy_spec_tts + phy_spec_gs + race_B + race_O + race_W + race_U +
  cancer_c259pancreatric



#propensity score model
p.score1 <- fitted(glm(ps_model, data = mydata2_plasmode, family = binomial))
p.score2 <- fitted(glm(ps_model2, data = mydata2_plasmode, family = binomial))

mydata2_plasmode$p.score1 <- p.score1
mydata2_plasmode$p.score2 <- p.score2



#Choose your solver; "gurobi" is best, "glpk" is free and
#easiest to install
solver <- "gurobi"


# 1:1 matching
m.out1 <- matchit(ps_model, data = mydata2_plasmode,
                           method = "nearest", distance = p.score1,
                           replace = FALSE, caliper = 0.1) # no cluster-level adjustment
m.out2 <- matchit(ps_model2, data = mydata2_plasmode,
                           method = "nearest", distance = p.score2,exact = "prov_id",
                           replace = FALSE, caliper = 0.1) # no cluster-level adjustment

m.out3 <- matchit(ps_model3, data = mydata2_plasmode, method = "cardinality",
                           estimand = "ATT", ratio = 1,
                           tols = 0.1, replace = FALSE, solver = solver,time = 2*60)


#IPW
m.out4 <- weightit(ps_model,
                       data = mydata2_plasmode,
                       method = "glm",
                       estimand = "ATE",
                       stabilize = TRUE,
                       ps = p.score1)
m.out4f <- trim(m.out4, at = .95)
m.out4f$weights

#summary
summary_1 <- summary(m.out1)
summary_2 <- summary(m.out2)
summary_3 <- summary(m.out3)
summary_4 <- summary(m.out4f)

#covariate balance graph
sum_1 <- bal.tab(m.out1,abs = TRUE, which = "both")
sum_2 <- bal.tab(m.out2,abs = TRUE)
sum_3 <- bal.tab(m.out3,abs = TRUE)
sum_4 <- bal.tab(m.out4, which = "unadjusted")
unadjust <- bal.tab(ps_model, data = mydata2_plasmode,abs = TRUE, which = "unadjusted")
unadjust$Balance
sum_1$Balance[,3]
summary_1$sum.all

match1_un <- abs( )
match1 <- abs(summary_1$sum.matched[,3])
match2 <- sum_2$Balance[,3]
match3 <- sum_3$Balance[,3]
m_data <- data.frame(match1_un,match1)
#ggplot2
count(match1)
ggplot(m_data, aes(x = match1_un, y = match1)) + geom_point() + geom_vline(xintercept = 0.1,
                                                                           linetype = "dotted",
                                                                           size = 1) + geom_hline(yintercept = 0.1,
                                                                                                  linetype = "dotted",
                                                                                                  size = 1) +xlim(0,0.5)


##IPW for model 1
marginal_weight <- sum(mydata2_plasmode$exposure)/length(mydata2_plasmode$exposure)
mydata2_plasmode$p.score1 <- p.score1
mydata2_plasmode$P_score1a <- marginal_weight/mydata2_plasmode$p.score1
mydata2_plasmode$IPW1 <- (1 - marginal_weight)/(1-mydata2_plasmode$p.score1)
mydata2_plasmode$IPW1[data$exposure==1] <-  mydata2_plasmode$P_score1a[mydata2_plasmode$exposure==1] 

v <- data.frame(old = colnames(m.out1$X),
                new = c(1:76))
bal.tab(m.out1,abs = TRUE,un)


#love plot
psa <- love.plot(
  m.out1,
  stats = c("m"),
  threshold = .1,
  var.order = NULL,
  abs = TRUE,
  limits = c(0, 0.35),
  sample.names = c("Unadjusted", "PSM-Across"),
  position = "right",
  var.names = v,
  title = "PS-Matching (across cluster)",drop.distance = TRUE
)

psw <- love.plot(
  m.out2,
  stats = c("m"),
  threshold = .1,
  var.order = NULL,
  abs = TRUE,
  limits = c(0, 0.35),
  sample.names = c("Unadjusted", "Unadjusted", "Adjusted"),
  position = "right",
  var.names = v,
  title = "PS-Matching (within cluster)",drop.distance = TRUE
)

card <- love.plot(
  m.out3,
  stats = c("m"),
  threshold = .1,
  var.order = NULL,
  abs = TRUE,
  limits = c(0, 0.35),
  sample.names = c("Unadjusted", "Adjusted"),
  position = "right",
  var.names = v,
  title = "Cardinality Matching",drop.distance = TRUE,
  
)

pswe <- love.plot(
  m.out4f,
  stats = c("m"),
  threshold = .1,
  var.order = NULL,
  abs = TRUE,
  limits = c(0, 0.35),
  sample.names = c("Unadjusted", "Adjusted"),
  position = "right",
  var.names = v,
  title = "PS-Weighting (IPW)",drop.distance = TRUE

)



library(ggpubr)
plot1 <- ggarrange(
  psw + theme(axis.title.x=element_blank()),
  psa + theme(axis.title.x=element_blank()),
  card + theme(axis.title.x=element_blank()),
  pswe + theme(axis.title.x=element_blank()),
  
  nrow = 1,
  ncol = 4,
  common.legend = TRUE,
   legend="right"

)

annotate_figure(plot1, bottom = text_grob("Absolute Standardise Mean Differences (ASMD)", 
                                      color = "black", face = "bold", size = 14), left = text_grob("Covariates", 
                color = "black", face = "bold", size = 14, rot = 90), top = text_grob("Covariates Balance Surgery Type", 
                                                                                      color = "black", face = "bold", size = 14))

#outcome estimation causal forest
library(boot)
out_a <- inpt_readm_0_30 ~ exposure
out_b <- inpt_readm_0_60 ~ exposure
out_c <- inpt_readm_0_90 ~ exposure
out_d <- expired ~ exposure
out_e <- comp_cat_bleeding_grp_poa ~ exposure
out_f <- comp_lev1_trans_bldovrall ~ exposure

out_ar <- inpt_readm_0_30 ~ exposure + (1|prov_id)
out_br <- inpt_readm_0_60 ~ exposure + (1|prov_id)
out_cr <- inpt_readm_0_90 ~ exposure + (1|prov_id)
out_dr <- expired ~ exposure + (1|prov_id)
out_er <- comp_cat_bleeding_grp_poa ~ exposure + (1|prov_id)
out_fr <- comp_lev1_trans_bldovrall ~ exposure + (1|prov_id)


outcome <- c(out_a, out_b, out_c, out_d, out_e, out_f)
outcome_r <- c(out_ar, out_br, out_cr, out_dr, out_er, out_fr)
#causal forest
cau.forest.1 <- function(data, i) { 
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i,][,confounders], data[i,]$inpt_readm_0_30,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$inpt_readm_0_30, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
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
    regression_forest(data[i,][,confounders], data[i,]$inpt_readm_0_60,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$inpt_readm_0_60, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
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
    regression_forest(data[i,][,confounders], data[i,]$inpt_readm_0_90,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$inpt_readm_0_90, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}
cau.forest.4 <- function(data, i) { 
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i,][,confounders], data[i,]$expired,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$expired, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}
cau.forest.5 <- function(data, i) { 
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i,][,confounders], data[i,]$comp_cat_bleeding_grp_poa,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$comp_cat_bleeding_grp_poa, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}
cau.forest.6 <- function(data, i) { 
  # These are estimates of m(X) = E[Y | X]
  forest.Y <-
    regression_forest(data[i,][,confounders], data[i,]$comp_lev1_trans_bldovrall,num.trees = 1000)
  Y.hat <- predict(forest.Y)$predictions
  
  # These are estimates of the propensity score E[W | X]
  forest.W <-
    regression_forest(data[i,][, confounders], data[i,]$exposure,num.trees = 1000)
  W.hat <- predict(forest.W)$predictions
  
  c.forest <-
    causal_forest(data[i,][, confounders], data[i,]$comp_lev1_trans_bldovrall, data[i,]$exposure, Y.hat, W.hat,num.trees = 1000)
  tau.hat <- predict(c.forest)$predictions
  # E[Y | X, W = 0]
  mu.hat.0 <- Y.hat - W.hat * tau.hat
  # E[Y | X, W = 1]
  mu.hat.1 <- Y.hat + (1 - W.hat) * tau.hat
  te <- mean(mu.hat.0) / mean(1 - mu.hat.0)
  te.2 <- mean(mu.hat.1) / mean(1 - mu.hat.1)
  log(te.2 / te)
}

cf_outcome <- c(cau.forest.1,cau.forest.2,cau.forest.3,cau.forest.4,cau.forest.5,cau.forest.6)
name <- c("inpt_readm_0_30","inpt_readm_0_60","inpt_readm_0_90","expired","comp_cat_bleeding_grp_poa","comp_lev1_trans_bldovrall")

logit_test <- function(d,indices) {  
  d <- d[indices,]  
  fit <- glmer(outcome_r[[j]], data = d, family = binomial, weights = m.out4$weights) 
  return(summary(fit)$coefficients[2,1])  
}

#outcome

out_a <- inpt_readm_0_30 ~ exposure
out_b <- inpt_readm_0_60 ~ exposure
out_c <- inpt_readm_0_90 ~ exposure
out_d <- expired ~ exposure
out_e <- comp_cat_bleeding_grp_poa ~ exposure
out_f <- comp_lev1_trans_bldovrall ~ exposure

out_ar <- inpt_readm_0_30 ~ exposure + (1|prov_id)
out_br <- inpt_readm_0_60 ~ exposure + (1|prov_id)
out_cr <- inpt_readm_0_90 ~ exposure + (1|prov_id)
out_dr <- expired ~ exposure + (1|prov_id)
out_er <- comp_cat_bleeding_grp_poa ~ exposure + (1|prov_id)
out_fr <- comp_lev1_trans_bldovrall ~ exposure + (1|prov_id)


outcome <- c(out_a, out_b, out_c, out_d, out_e, out_f)
outcome_r <- c(out_ar, out_br, out_cr, out_dr, out_er, out_fr)
out <-
  c(
    data[i,]$inpt_readm_0_30,
    data[i,]$inpt_readm_0_60,
    data[i,]$inpt_readm_0_90,
    data[i,]$expired,
    data[i,]$comp_cat_bleeding_grp_poa,
    data[i,]$comp_lev1_trans_bldovrall
  )


base_data <- list()
for (j in 1:6){
  
  ##
  logit_test <- function(d,indices) {  
    d <- d[indices,]  
    fit <- glmer(outcome_r[[j]], data = d, family = binomial, weights = m.out4$weights) 
    return(summary(fit)$coefficients[2,1])  
  }
#
model.0 <- glm(outcome[[j]],
                 data = mydata2_plasmode,
                 family = binomial)
model.1 <- boot(mydata2_plasmode, cf_outcome[[j]], R = 100)

model.2 <-
  glm(outcome[[j]],
      data = match.data(m.out1),
      family = binomial)

model.3 <-
  glm(outcome[[j]],
      data = match.data(m.out2),
      family = binomial)

model.4 <-
  glm(outcome[[j]],
      data = match.data(m.out3),
      family = binomial)

model.5 <- boot(data = mydata2_plasmode,
                statistic = logit_test,
                R = 100) 
t_effect <-
  c(
    summary(model.0)$coefficient[2, 1],
    summary(model.2)$coefficient[2, 1],
    summary(model.3)$coefficient[2, 1],
    summary(model.4)$coefficient[2, 1],
    model.5$t0,
    model.1$t0
  )
se <-
  c(
    summary(model.0)$coefficient[2, 2],
    summary(model.2)$coefficient[2, 2],
    summary(model.3)$coefficient[2, 2],
    summary(model.4)$coefficient[2, 2],
    summary(model.5)[1, 4],
    summary(model.1)[1, 4]
  )

ci_lower <- t_effect-1.96*se
ci_higher <- t_effect+1.96*se
outcome_name <- c(rep(name[j],length(name)))
odd_ratio <- exp(t_effect)
lower <- exp(ci_lower)
upper <- exp(ci_higher)
method <- c("Unadjusted","PS-Matching (across cluster)","PS-Matching (within cluster)","Cardinality Matching","PS-Weighting (IPW)","Causal Forest")
base_data[[j]] <-
  tibble::tibble(
    outcome_name = outcome_name,
    method = method,
    t_effect = t_effect,
    se = se,
    ci_lower = ci_lower,
    ci_higher = ci_higher,
    odd_ratio = odd_ratio,
    lower = lower,
    upper = upper
  )
}
base_data_f <- base_data %>% purrr::reduce(union)
outcome_name[2]
result_text <- paste(
  round(base_data_f$odd_ratio, digit = 2),
  " (",
  round(base_data_f$lower, digit = 2),
  ",",
  round(base_data_f$upper, digit = 2),
  ")", sep=""
)


bal.plot(
  exposure ~ p.score1,
  data = mydata2_plasmode,
  weights = list(
    "PS-Matching (across cluster)" = m.out1,
    "PS-Matching (within cluster)" = m.out2,
    "Cardinalty Matching" = m.out3,
    "PS-Weighting (IPW)" = m.out4
  ),
  which = "both",
  position = "bottom"
) + labs(title="Propensity score Disbribution Density (Robotic)",
       x ="Propensity score", y = "Density")+
  theme(plot.title = element_text(hjust = 0.5))



library(cobalt)
summary(m.out3)

love.plot

library(ggpubr)
library(reshape)
library(grid)
final_chart <-
  ggarrange(
    psa + rremove("ylab"),
    psw + rremove("ylab"),
    card + rremove("ylab"),
    
    common.legend = FALSE,
    legend = "right",
    font.label = list(color = "black", size = 9),
    ncol = 2,
    nrow = 2
  )

annotate_figure(final_chart,
                bottom = textGrob("Cluster-level confounders effect on Allocation (Odd Ratio)"),
                top = textGrob("Plasmode Simulation", gp = gpar(cex = 1.3)))

###############################################################################
#forest plot fpor outcome
library(forestplot)

base <- tibble::tibble(mean = base_data_f$odd_ratio, lower=base_data_f$lower, upper = base_data_f$upper, study = base_data_f$method,outcome = base_data_f$outcome_name, OR = result_text)




outcome_label <-
  c(
    inpt_readm_0_30 = "Readmission within 30 days",
    inpt_readm_0_60 = "Readmission within 60 days",
    inpt_readm_0_90 = "Readmission within 90 days",
    expired = "Death during admission",
    comp_cat_bleeding_grp_poa = "Complication during admission - Bleeding Related",
    comp_lev1_trans_bldovrall = "Complication during admission - Blood Transfusion Related"
  )
base$outcome_label <- as.character(outcome_label[base$outcome])

base %>% distinct(outcome_label)

study <- method
as.character(comp_1)
base |>
  forestplot(labeltext = c(outcome_label,study,OR),
             clip = c(0.1, 2.5),
             xlog = TRUE) |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue") |> 
  fp_add_header(outcome_label = c("Outcome"),
                study = c("Mehtod"),
                OR = c("OR (95% CI)")) |>
  fp_set_zebra_style("#EFEFEF")


base_data <- tibble::tibble(mean  = mean,
                            lower = lower,
                            upper = upper,
                            study = study,
                            deaths_steroid = as.character(comp_1),
                            deaths_placebo = as.character(comp_0),
                            OR = as.character(round(mean, digits = 2)))

base_data |>
  forestplot(labeltext = c(study, OR),
             clip = c(0.5, 3.5),
             xlog = TRUE) |>
  fp_add_lines()  |>
  fp_set_style(box = "royalblue",
               line = "darkblue",
               summary = "royalblue") |> 
  fp_add_header(study = c("", "Method"),
                OR = c("", "OR")) |>


  fp_set_zebra_style("#EFEFEF")

base |>
  group_by(study) |>
  forestplot(labeltext = c(outcome_label),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI, fpDrawCircleCI, fpDrawCircleCI,fpDrawDiamondCI,fpDrawDiamondCI),
             clip = c(0.5, 5),
             xlog = TRUE,
             vline = 1,
             boxsize = .1,
            ## graph.pos = 1,
             legend_args = fpLegend(),
             title = "Distal vs Whipple")|> 
  fp_set_style(box = c("blue", "darkred","green","pink","yellow","orange") |> lapply(function(x) gpar(fill = x, col = "#555555")),
               default = gpar(vertices = TRUE)) |> 
  fp_add_header(outcome_label = c("Outcome")) |>
  fp_set_zebra_style("#F5F9F9")

