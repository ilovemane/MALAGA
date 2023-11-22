
#plasmode simulation code

library(skimr)
library(DataExplorer)
library(tidyverse)
library(tableone)
library(kableExtra)
library(knitr)


filename <- "Pancreatic_ADS_Oxford.csv" ## input dataset
mydata <- read.csv(filename)




mydata2_sub <- mydata %>% filter(procedure_type=="Pancreatic -Whipple" | procedure_type=="Pancreatic -Distal") %>% filter(cancer_1 == 1)

mydata2_sub$comp_trans_all = rowSums(mydata2_sub[, c(
                                         "comp_lev1_trans_bldovrall",
                                         "comp_lev1_trans_prdovrall",
                                         "comp_lev1_transf_procovrall",
                                         "comp_lev2_trans_exovrall",
                                         "comp_lev2_trans_prdovrall",
                                         "comp_lev2_trans_procovrall",
                                         "comp_lev2_trans_periovrall",
                                         "comp_lev2_trans_coaovrall",
                                         "comp_lev2_trans_crovrall",
                                         "comp_lev2_trans_plaovrall",
                                         "comp_lev2_trans_serovrall",
                                         "comp_lev2_trans_subovrall",
                                         "comp_lev2_trans_pcelovrall",
                                         "comp_lev2_trans_pltovrall",
                                         "comp_lev2_trans_atblovrall")])

mydata2_sub <- mydata2_sub %>% mutate(comp_blood_trans = ifelse(comp_trans_all > 0,1,0))

cont_patient <- c("age","cm_cci","cm_elix","los","surgtime_mins")
discrete_patient <- c("cci01_mi",
                      "cci02_chf",
                      "cci03_pvd",
                      "cci04_cvd",
                      "cci05_dem",
                      "cci06_cpd",
                      "cci07_ra",
                      "cci08_ulcer",
                      "cci09_liv",
                      "cci10_diab",
                      "cci11_diabc",
                      "cci12_para",
                      "cci13_renal",
                      "cci14_can",
                      "cci15_livc",
                      "cci16_can2",
                      "cci17_hiv",
                      "elx01_chf",
                      "elx02_arrhy",
                      "elx03_valv",
                      "elx04_pcd",
                      "elx05_pvd",
                      "elx06_htn",
                      "elx07_htnc",
                      "elx08_para",
                      "elx09_neur",
                      "elx10_cpd",
                      "elx11_diab",
                      "elx12_diabc",
                      "elx13_thyr",
                      "elx14_renal",
                      "elx15_liv",
                      "elx16_ulcer",
                      "elx17_hiv",
                      "elx18_lym",
                      "elx19_can2",
                      "elx20_can",
                      "elx21_ra",
                      "elx22_coag",
                      "elx23_obe",
                      "elx24_wtl",
                      "elx25_fed",
                      "elx26_bla",
                      "elx27_dfa",
                      "elx28_alc",
                      "elx29_dabu",
                      "elx30_psy",
                      "elx31_depr",
                      "cancer_155liver",
                      "cancer_155_categoryliver",
                      "cancer_155_0liver",
                      "cancer_c220liver",
                      "cancer_c222liver",
                      "cancer_c227liver",
                      "cancer_c228liver",
                      "cancer_155_1liver",
                      "cancer_c221liver",
                      "cancer_155_2liver",
                      "cancer_c229liver",
                      "cancer_197_7liver",
                      "cancer_c787liver",
                      "cancer_230_8liver",
                      "cancer_d015liver",
                      "cancer_235_3liver",
                      "cancer_d376liver",
                      "cancer_157pancreatric",
                      "cancer_157_0pancreatric",
                      "cancer_c250pancreatric",
                      "cancer_157_1pancreatric",
                      "cancer_c251pancreatric",
                      "cancer_157_2pancreatric",
                      "cancer_c252pancreatric",
                      "cancer_157_3pancreatric",
                      "cancer_c253pancreatric",
                      "cancer_157_4pancreatric",
                      "cancer_c254pancreatric",
                      "cancer_157_8pancreatric",
                      "cancer_c257pancreatric",
                      "cancer_c258pancreatric",
                      "cancer_157_9pancreatric",
                      "cancer_c259pancreatric",
                      "cancer_1","procedure_type")

outcome <- c("inpt_readm_0_30",
             "inpt_readm_0_60",
             "inpt_readm_0_90",
             "expired",
             "comp_cat_bleeding_grp_poa",
             "comp_lev1_trans_bldovrall")


all_cov <- c("provider_yearly_surg_vol","urban_rural",
         "teaching",
         "beds_grp",
         "prov_region",
         "gender","race","proc_phy_spec_desc_grp",cont_patient,discrete_patient,"robotics_new")

all_outcome <- c(outcome,"procedure_type","robotics_new")

comm_index <- c("cci01_mi",
"cci02_chf",
"cci03_pvd",
"cci04_cvd",
"cci05_dem",
"cci06_cpd",
"cci07_ra",
"cci08_ulcer",
"cci09_liv",
"cci10_diab",
"cci11_diabc",
"cci12_para",
"cci13_renal",
"cci14_can",
"cci15_livc",
"cci16_can2",
"cci17_hiv",
"cm_cci",
"elx01_chf",
"elx02_arrhy",
"elx03_valv",
"elx04_pcd",
"elx05_pvd",
"elx06_htn",
"elx07_htnc",
"elx08_para",
"elx09_neur",
"elx10_cpd",
"elx11_diab",
"elx12_diabc",
"elx13_thyr",
"elx14_renal",
"elx15_liv",
"elx16_ulcer",
"elx17_hiv",
"elx18_lym",
"elx19_can2",
"elx20_can",
"elx21_ra",
"elx22_coag",
"elx23_obe",
"elx24_wtl",
"elx25_fed",
"elx26_bla",
"elx27_dfa",
"elx28_alc",
"elx29_dabu",
"elx30_psy",
"elx31_depr",
"cm_elix")


cancer_type <- c("cancer_155liver",
"cancer_155_categoryliver",
"cancer_155_0liver",
"cancer_c220liver",
"cancer_c222liver",
"cancer_c227liver",
"cancer_c228liver",
"cancer_155_1liver",
"cancer_c221liver",
"cancer_155_2liver",
"cancer_c229liver",
"cancer_197_7liver",
"cancer_c787liver",
"cancer_230_8liver",
"cancer_d015liver",
"cancer_235_3liver",
"cancer_d376liver",
"cancer_157pancreatric",
"cancer_157_0pancreatric",
"cancer_c250pancreatric",
"cancer_157_1pancreatric",
"cancer_c251pancreatric",
"cancer_157_2pancreatric",
"cancer_c252pancreatric",
"cancer_157_3pancreatric",
"cancer_c253pancreatric",
"cancer_157_4pancreatric",
"cancer_c254pancreatric",
"cancer_157_8pancreatric",
"cancer_c257pancreatric",
"cancer_c258pancreatric",
"cancer_157_9pancreatric",
"cancer_c259pancreatric",
"cancer_1")

new_data <- mydata2_sub[,all_cov]
new_data <- new_data[, colSums(new_data != 0) > 0]

new_data_out <- mydata2_sub[,all_outcome]
new_data_out <- new_data_out[, colSums(new_data_out != 0) > 0]

#new_data %>% create_report(output_file = "report_test3", report_title = "Pancreatic Surgery  Data Profiling Report")

discrete_var <- discrete_patient[-86]
select(new_data,-procedure_type)

t3 <- tableone::CreateTableOne(vars = colnames(new_data[, !names(new_data) %in% c("procedure_type")]),
                           strata = "procedure_type",
                           data = new_data, test = FALSE, factorVars = c("urban_rural",
                                                                         "teaching",
                                                                         "beds_grp",
                                                                         "prov_region",
                                                                         "prov_division",
                                                                         "gender","race","proc_phy_spec_desc_grp",discrete_var,"robotics_new"))

t4 <- tableone::CreateTableOne(vars = colnames(new_data[, !names(new_data) %in% c("robotics_new")]),
                               strata = "robotics_new",
                               data = new_data, test = FALSE, factorVars = c("urban_rural",
                                                                             "teaching",
                                                                             "beds_grp",
                                                                             "prov_region",
                                                                             "prov_division",
                                                                             "gender","race","proc_phy_spec_desc_grp",discrete_var))


t1 <- tableone::CreateCatTable(vars = colnames(new_data_out[, !names(new_data_out) %in% c("procedure_type","robotics_new")]),
                                     strata = c("procedure_type"),
                                     data = new_data_out, test = FALSE)

t2 <- tableone::CreateCatTable(vars = colnames(new_data_out[, !names(new_data_out) %in% c("procedure_type","robotics_new")]),
                               strata = c("robotics_new"),
                               data = new_data_out, test = FALSE)
output_tbl_1 <- print(t1, smd = TRUE)
output_tbl_2 <- print(t2, smd = TRUE)
output_tbl_3 <- print(t3, smd = TRUE)
output_tbl_4 <- print(t4, smd = TRUE)
kable(list(output_tbl_1,output_tbl_2))


print(output_tbl_1) %>%
  kbl(caption = "Possible outcome by procedure type") %>% footnote(
    general = "covariates definition",
    number = c(
      "inpt_readm_0_XX = readmission within XXdays; ",
      "comp_cat_bleeding_grp_poa = All bleeding related complication; ",
      "comp_lev1_trans_bldovrall = All blood transfusion related complication; "
    )
  ) %>%
  kable_paper("hover", full_width = F)

  print(output_tbl_2) %>%
  kbl(caption = "Possible outcome by robotic flag") %>% footnote(
    general = "covariates definition",
    number = c(
      "inpt_readm_0_XX = readmission within XXdays; ",
      "comp_cat_bleeding_grp_poa = All bleeding related complication; ",
      "comp_lev1_trans_bldovrall = All blood transfusion related complication; "
    )
  ) %>%
    kable_paper("hover", full_width = F)



print(output_tbl_3) %>%
  kbl(caption = "Covariates by procedure type") %>%
  kable_paper("hover", full_width = F)


print(output_tbl_4) %>%
  kbl(caption = "Covariates by robotic flag") %>%
  kable_paper("hover", full_width = F)

tableone::CreateCatTable()
print(tableOne, smd = TRUE)
##################################

mydata2_plasmode <- mydata %>% filter(procedure_type=="Pancreatic -Whipple" | procedure_type=="Pancreatic -Distal")

#setting exposure and outcome covariate
mydata2_plasmode$outcome<-mydata2_plasmode$any_complication#outcome
mydata2_plasmode$exposure <- ifelse(mydata2_plasmode$procedure_type == "Pancreatic -Whipple",1,0)#whipple =1 distal=0
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
  rename("phy_spec_gs" = "proc_phy_spec_desc_grp_GENERAL SURGERY (GS)",
         "phy_spec_other" = "proc_phy_spec_desc_grp_Other",
         "phy_spec_so" = "proc_phy_spec_desc_grp_SURGICAL ONCOLOGY (SO)",
         "phy_spec_tts" = "proc_phy_spec_desc_grp_TRANSPLANT SURGERY (TTS)")

mydata2_plasmode <- mydata2_plasmode %>%
  mutate(row_id=row_number()) %>%
  relocate(row_id)
colnames(new_data)
form1 <-
  outcome ~ exposure + urban_flag + surg_vol + beds_flag + prov_region_MIDWEST +
  prov_region_NORTHEAST + prov_region_SOUTH + prov_region_WEST + gender_flag + cm_cci + cm_elix + los + patient_age +
  surgtime_mins +            cci01_mi +                 cci02_chf +                cci03_pvd +
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
  phy_spec_other + phy_spec_so + phy_spec_tts + race_B + race_O + race_W + race_U +
  cancer_c259pancreatric

form2 <-
  exposure ~ urban_flag + surg_vol + beds_flag + prov_region_MIDWEST +
  prov_region_NORTHEAST + prov_region_SOUTH + prov_region_WEST + gender_flag + cm_cci + cm_elix + los + patient_age +
  surgtime_mins +            cci01_mi +                 cci02_chf +                cci03_pvd +
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
  phy_spec_other + phy_spec_so + phy_spec_tts + race_B + race_O + race_W + race_U +
  cancer_c259pancreatric

factor_hosp <- rep(1,each=7)
factor_pat1 <- rep(1, each=5)
factor_pat2 <- rep(1, each=64)
factor_pat3 <- rep(1, each = 58)

PlasmodeBin2<- function(formulaOut=NULL, objectOut=NULL,formulaExp=NULL,objectExp=NULL,data, idVar,
                        effectOR =1.5, MMOut=1,MMExp=1, nsim, size, eventRate=NULL, exposedPrev=NULL)

{

  ## Code for simulating data when the outcome is binary and data set is provided to estimated the outcome and exposure.
  if(is.null(formulaOut)==FALSE & is.null(formulaExp)==FALSE & is.null(objectOut)==TRUE & is.null(objectExp)==TRUE)

  {
    outcome<- all.vars(formulaOut)[1] ## selects the outcome variable
    exposure<- all.vars(formulaOut)[2] ##selects the exposure variable

    x <- data[order(data[,exposure]),] # order according to exposure status, unexposed first
    n <- nrow(x)
    n1 <- sum(x[,exposure])     # number of exposed in real data
    n0 <- n - n1
    size1 <- round(ifelse(is.null(exposedPrev), n1*(size/n), size*exposedPrev))  # desired number of exposed
    size0 <- size - size1
    if(size1 > n1 | size0 > n0) stop("Number of requested exposed or unexposed exceeds observed number -- reduce size")

    # estimate logit model for probability of outcome
    modOutBin<- glm2(formulaOut, family = "binomial", data=x,control=glm.control(trace=TRUE))
    ## Design Matrix used for outcome logistic regression
    X <- model.matrix(modOutBin)
    coef_out <- coef(modOutBin) %>% replace(is.na(.), 0)
    # find event rate in base cohort
    if(is.null(eventRate)) eventRate <- mean(x[,outcome])
    coef_out <- coef(modOutBin) %>% replace(is.na(.), 0)
    # find intercept value needed to get approximate desired event rate under new parameters
    bnew <- c(coef(modOutBin)[1], MMOut*coef_out[-1])
    bnew <- replace(bnew, names(coef(modOutBin)) == exposure, log(effectOR))
    Xbnew <- as.vector(X %*% bnew)
    fn <- function(d) mean(1 - 1/(1 + exp(d+Xbnew))) - eventRate
    delta <- uniroot(fn, lower = -20, upper = 20)$root

    ## Estimate logit model for probability of exposure
    modExp<- glm2(formulaExp, family = "binomial", data=x,control=glm.control(trace=TRUE))
    ## Design matrix used for exposure logistic regression
    XEXP<- model.matrix(modExp)

    # Finding the exposure prevalence in base cohort
    if(is.null(exposedPrev)) exposedPrev<- n1/n
    coef_Exp <- coef(modExp) %>% replace(is.na(.), 0)
    # Calculating intercept value needed to get approximate desired exposure prevalence under new parameters.
    bnewExp <- c(coef(modExp)[1], MMExp*coef_Exp[-1])
    XbnewExp <- as.vector(XEXP%*%bnewExp)
    fnExp <- function(d) mean(plogis(d+XbnewExp))-exposedPrev
    deltaExp <- uniroot(fnExp, lower=-20, upper=20)$root
    Probexp <- plogis(deltaExp+XbnewExp)

    #### sample and simulate
    ids <- ynew <- expnew<-data.frame(matrix(nrow = size, ncol = nsim))
    RR<-RD<- vector('numeric', length = nsim)
    ##datasim<-list()
    for(sim in 1:nsim) {
      idxs0 <- sample(1:n0, size0, replace = TRUE) # sample unexposed (located in rows 1:n0 of x)
      ids[1:size0,sim] <- x[idxs0, idVar]
      idxs1 <- sample(n0+1:n1, size1, replace = TRUE) # sample exposed (located in rows n0 + 1:n1 of x)
      ids[size0+1:size1,sim] <- x[idxs1, idVar]
      X2 <- X[c(idxs0,idxs1),]
      expnew[,sim]<- X2[,2] <- rbinom(size,1,Probexp[c(idxs0,idxs1)])
      pnew <- plogis(delta + X2%*%bnew)
      ynew[,sim] <- rbinom(size, 1, pnew)

      datasim<-X[c(idxs0,idxs1),]
      datasim[,2]<-1
      p_1<- 1 - 1/(1+exp(as.vector(datasim %*% bnew + delta)))
      datasim[,2]<-0
      p_0<- 1 - 1/(1+exp(as.vector(datasim %*% bnew + delta)))
      RR[sim]<-mean(p_1)/mean(p_0)
      RD[sim]<-mean(p_1)-mean(p_0)
    }
    ARR<-mean(RR)
    ARD<-mean(RD)
    ## Creating simulated data for the outcome variable
    names(ids) <- paste("ID", 1:nsim, sep = "")
    names(ynew) <- paste("EVENT", 1:nsim, sep = "")
    names(expnew)<-paste("EXPOSURE",1:nsim, sep = "")
    sim_out_bin<-data.frame(ids, ynew,expnew)


    ## Finding the simulated treatment at new exposure prevalence.

    return(list(TrueOutBeta = bnew, TrueExpBeta = bnewExp, RR=ARR,RD=ARD,Sim_Data = sim_out_bin))
  }
}


plasmode_data <- list()


for (i in 1:1000) {

hihi<- PlasmodeBin2(formulaOut=form1, objectOut=NULL,formulaExp=form2,objectExp=NULL,data = mydata2_plasmode, idVar="row_id",
                    effectOR =1.5, MMOut=c(1,2*factor_hosp,factor_pat1,0*factor_pat1,factor_pat3),MMExp=c(2*factor_hosp,factor_pat2,0*factor_pat1), nsim = 1, size = length(mydata2_plasmode), eventRate=NULL, exposedPrev=NULL)


plasmode_data[[i]] <- hihi$Sim_Data

}




