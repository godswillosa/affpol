library(haven)
anes_2012 <- read_dta("anes_timeseries_2012.dta")



#datacleaning
library(dplyr)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(interactions)
library(jtools)
library(visreg)

#cleanup survey response variables 
anes2012 <- as_tibble(anes_2012)

#subset anes_time series into variables of interest 
nes_total<-anes2012 %>% select(aidblack_self, aidblack_dpc, aidblack_rpc, inspre_self, inspre_dpc, inspre_rpc, inspre_dem, 
                               inspre_rep, guarpr_self, guarpr_dpc, guarpr_rpc, guarpr_dem, guarpr_rep, spsrvpr_ssself, spsrvpr_ssdpc, 
                               spsrvpr_ssrpc, spsrvpr_ssdem, spsrvpr_ssrep, abortpre_4point,abort_dpc4, abort_rpc4, abort_dem4, abort_rep4, 
                               abort_health, abort_fatal, abort_incest, abort_rape, abort_bd, abort_fin, abort_choice, gayrt_adopt, gayrt_marry, 
                               gayrt_discstd, gayrt_milstd, immig_checks, immigpo_level, immigpo_jobs,resent_workway, resent_slavery, resent_deserve, 
                               resent_try, ftcasi_asian, ftcasi_hisp, ftcasi_black, ftcasi_illegal, ftcasi_white, racecasi_infwhite, racecasi_infblacks, 
                               racecasi_infhisp, racecasi_sympblacks, racecasi_admblacks, stype_hwkwhite, stype_hwkblack, stype_hwkhisp, stype_hwkasian, 
                               stype_intwhite, stype_intblack, stype_inthisp, stype_intasian, discrim_blacks, discrim_hispanics, discrim_whites, discrim_gays, 
                               discrim_women, discrim_self, ft_dpc, ft_rpc, ft_dem, ft_rep, libcpre_self, pid_self, pid_strong, pid_lean, dem_hisp, 
                               dem_racecps_white, dem_racecps_black)
nes_total<- as_tibble(nes_total)



################# Ethnocentrism Creation####################
#Formula
  #Ingroup - avg outgroup score
################# Ethnocentrism Creation####################3

# intelligencewhites
for (val in anes_limited$dem_racecps_white){
if (anes_limited$dem_racecps_white==1) {
  anes_limited$eth_intell <- (anes_limited$stype_intwhite - mean(c(anes_limited$stype_intasian, anes_limited$stype_intblack, anes_limited$stype_inthisp)))
}
}

for (val in anes_limited$dem_racecps_white){
  ifelse(anes_limited$dem_racecps_white==1, (anes_limited$eth_intell <- (anes_limited$stype_intwhite - mean(c(anes_limited$stype_intasian, anes_limited$stype_intblack, anes_limited$stype_inthisp)))), 0)
  }

#intelligence blacks
if (anes_limited$dem_racecps_black==1) {
  anes_limited$eth_intell <- (anes_limited$stype_intblack - mean(c(anes_limited$stype_intwhite, anes_limited$stype_intasian, anes_limited$stype_inthisp)))
}

#intelligence hispanics
if (anes_limited$dem_hisp ==1) {
  anes_limited$eth_intell <- (anes_limited$stype_hwkhisp - mean(c(anes_limited$stype_intasian, anes_limited$stype_intblack, anes_limited$stype_intwhite)))
}

x <- mean(c(anes_limited$stype_intasian, anes_limited$stype_intblack, anes_limited$stype_inthisp))


#hardworking whites
if (anes_limited$dem_racecps_white==1) {
  anes_limited$eth_hw <- (anes_limited$stype_hwkwhite - mean(c(anes_limited$stype_hwkasian, anes_limited$stype_hwkblack, anes_limited$stype_hwkhisp)))
}


#hardworking blacks
if (anes_limited$dem_racecps_black==1) {
  anes_limited$eth_hw <- (anes_limited$stype_hwkblack - mean(c(anes_limited$stype_hwkasian, anes_limited$stype_hwkhisp, anes_limited$stype_hwkwhite)))
}

#hardworking hispanics
if (anes_limited$dem_hisp==1) {
  anes_limited$eth_hw <- (anes_limited$stype_hwkhisp - mean(c(anes_limited$stype_hwkasian, anes_limited$stype_hwkblack, anes_limited$stype_hwkwhite)))
}

#ethnocentrism variable
anes_limited$ethnocen <- ((anes_limited$eth_hw + anes_limited$eth_intell)/2)

summary(anes_limited$ethnocen)

summary(anes_limited$ethnocen[which(anes_limited$dem_racecps_black==1)])
summary(anes_limited$ethnocen[which(anes_limited$dem_racecps_white==1)])
################# Ethnocentrism Creation####################3
#removed missing data  (see p. 36 of 2012 ANES data code book)
anes_limited<-nes_total %>% filter(aidblack_self > -1, aidblack_dpc > -1, aidblack_rpc > -1, inspre_self > -1, inspre_dpc > -1, inspre_rpc > -1, inspre_dem > -1, inspre_rep > -1, guarpr_self > -1, guarpr_dpc > -1, guarpr_rpc > -1, guarpr_dem > -1, guarpr_rep > -1, spsrvpr_ssself > -1, spsrvpr_ssdpc > -1, spsrvpr_ssrpc > -1, spsrvpr_ssdem > -1, spsrvpr_ssrep > -1, abortpre_4point > -1,
                                abort_dpc4 > -1, abort_rpc4 > -1, abort_dem4 > -1, abort_rep4 > -1, abort_health > -1, abort_fatal > -1, abort_incest > -1, abort_rape > -1, abort_bd > -1, abort_fin > -1, abort_choice > -1, gayrt_adopt > -1, gayrt_marry > -1, gayrt_discstd > -1, gayrt_milstd > -1, immig_checks > -1, immigpo_level > -1, immigpo_jobs > -1,
                                resent_workway > -1, resent_slavery > -1, resent_deserve > -1, resent_try > -1, ftcasi_asian > -1, ftcasi_hisp > -1, ftcasi_black > -1, ftcasi_illegal > -1, ftcasi_white > -1, racecasi_infwhite > -1, racecasi_infblacks > -1, racecasi_infhisp > -1, racecasi_sympblacks > -1, racecasi_admblacks > -1, stype_hwkwhite > -1, stype_hwkblack > -1,
                                stype_hwkhisp > -1, stype_hwkasian > -1, stype_intwhite > -1, stype_intblack > -1, stype_inthisp > -1, stype_intasian > -1, discrim_blacks > -1, discrim_hispanics > -1, discrim_whites > -1, discrim_gays > -1, discrim_women > -1, discrim_self > -1, ft_dpc > -1, ft_rpc > -1, ft_dem > -1, ft_rep > -1, libcpre_self > -1, pid_self > 0, pid_strong > -2, pid_lean > -2, dem_hisp > -1, 
                                dem_racecps_white > -1, dem_racecps_black > -1)

summary(anes_limited$aidblack_self) #shows data cleaning worked 

#Recode partyId as democrats = -1, republicans = 1 and independents = 0 as separate variables
anes_limited <- anes_limited %>% mutate(democrat = case_when(pid_self==1~1, TRUE~0), republican = case_when(pid_self==2~1, TRUE~0), independent = case_when(pid_self==3~1, TRUE~0))
anes_limited <- anes_limited %>% mutate(partyid = case_when(pid_self==1~-1, pid_self==2~1, pid_self==3~0))
anes_limited <- anes_limited %>% filter(!is.na(partyid))
summary(anes_limited$partyid)

#recode strong partisan as 1 if strong 0 if not 
anes_limited <- anes_limited %>% mutate(pid_strong = case_when(pid_strong==1~1, TRUE~0))

#create affective polarization variable

# Affective Polarization for the Democratic vs. Republican Presidential Candidates
anes_limited$aff_polpc <- with(anes_limited, abs((anes_limited$ft_dpc - anes_limited$ft_rpc)))
summary(anes_limited$aff_polpc)

# Affective Polarization for Democratic vs. Republican Parties 
anes_limited$aff_polparty <- with(anes_limited, abs((anes_limited$ft_dem - anes_limited$ft_rep)))
summary(anes_limited$aff_polparty)

#create race dummy
anes_limited <- anes_limited %>% mutate(race = case_when(dem_hisp==1~2, dem_racecps_black==1~1, dem_racecps_white==1~3))
race_dummy_data <- anes_limited %>% mutate(race = case_when(dem_hisp==1~2, dem_racecps_black==1~1, dem_racecps_white==1~3))
#Create Race separated data
hispdata <- anes_limited %>% filter(dem_hisp == 1) #HISPANICS
blackdata <- anes_limited %>% filter(dem_racecps_black==1) #BLACKS
whitedata <- anes_limited %>% filter(dem_racecps_white==1) #WHITES


# Ajay Code Start ---------------------------------------------------------------

# Creating race dummy
anes_limited$race = ifelse(anes_limited$dem_racecps_black == 1, 1, 
                        ifelse(anes_limited$dem_hisp == 1, 2,
                               ifelse(anes_limited$dem_racecps_white == 1, 3, 0)))


# Remove non- white/blk/latinx
anes_limited_pure = subset(anes_limited, race != 0)

# Turn race var into factor 
class(anes_limited$race)
anes_limited$race = as.factor(anes_limited$race)
levels(race_dummy_data$race)

# Run regressions
testint <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                gayrt_marry + pid_strong + resent_try +  libcpre_self + 
                immigpo_jobs*race, data = anes_limited)


candidate_polarization_simple <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                       gayrt_marry + pid_strong + immigpo_jobs + resent_try +  
                                       libcpre_self + dem_hisp + dem_racecps_white, data = anes_limited)


candidate_polarization_w_immig_int <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                            gayrt_marry + pid_strong + resent_try +  libcpre_self + 
                                            immigpo_jobs*dem_racecps_white + dem_hisp, data = anes_limited)

candidate_polarization_w_resent_int <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                             gayrt_marry + pid_strong + immigpo_jobs + libcpre_self +
                                             resent_try*dem_racecps_white + dem_hisp, data = anes_limited)

candidate_polarization_3way <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                    gayrt_marry + pid_strong  + libcpre_self +
                                    immigpo_jobs*resent_try*dem_racecps_white + dem_hisp, data = anes_limited)



party_polarization_simple <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + 
                                  gayrt_marry + pid_strong + immigpo_jobs + 
                                  resent_try + libcpre_self + dem_hisp + dem_racecps_white, data = anes_limited)

party_polarization_int_w_immig <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + 
                                       gayrt_marry + pid_strong + resent_try + libcpre_self +  
                                       + immigpo_jobs*dem_racecps_white + dem_hisp, data = anes_limited)

party_polarization_int_w_resent <- lm( aff_polparty ~ spsrvpr_ssself + abortpre_4point + 
                                         gayrt_marry + pid_strong + immigpo_jobs + libcpre_self + 
                                         resent_try*dem_racecps_white + dem_hisp, data = anes_limited)

# Compare model performance
anova(candidate_polarization_w_resent_int, candidate_polarization_simple)

# Create summaries
summary(candidate_polarization_simple)
summary(candidate_polarization_w_immig_int)
summary(candidate_polarization_w_resent_int)
summary(party_polarization_simple)
summary(party_polarization_int_w_immig)
summary(party_polarization_int_w_resent)
summary(candidate_polarization_3way)


visreg(candidate_polarization_w_immig_int, "immigpo_jobs", by = "dem_racecps_white", overlay=T, partial = F, rug=F)
visreg(candidate_polarization_3way, "immigpo_jobs", by = 'dem_racecps_white', overlay = T, partial = F, rug = F)
visreg(party_polarization_int_w_resent, "resent_try", by = 'dem_racecps_white', overlay = T, partial = F, rug = F)

#GRAPHS
ggplot(anes_limited_pure, aes(x=libcpre_self, y=aff_polpc, fill = race)) + stat_summary(fun = mean, geom = "col")
ggplot(anes_limited_pure, aes(x=immigpo_jobs, y=aff_polpc, fill = race)) + stat_summary(fun = mean, geom = "col")
ggplot(anes_limited_pure, aes(x=resent_try, y=aff_polpc, fill = race)) + stat_summary(fun = mean, geom = "col") 
visreg(candidate_polarization_w_immig_int, "immigpo_jobs", by = "dem_racecps_white", overlay=T, partial = F, rug=F)
visreg(candidate_polarization_w_resent_int, "resent_try", by = "dem_racecps_white", overlay=T, partial = F, rug=F)


aff_polparty ~ spsrvpr_ssself + abortpre_4point + 
  gayrt_marry + pid_strong + immigpo_jobs + 
  resent_try + libcpre_self + dem_hisp + dem_racecps_white
variables <- c("aff_polparty", "aff_polpc", "spsrvpr_ssself", "abortpre_4point", "gayrt_marry", 
               "pid_strong", "immigpo_jobs", "resent_try", "libcpre_self", "dem_hisp",
               "dem_racecps_white", "dem_racecps_black", data =anes_limited)



cat_plot(testint, pred = immigpo_jobs, modx = race)


# Identify dominant variables (which variables are most important?)
dominanceAnalysis(pre_web_abram_cand_simple)
dominanceAnalysis(pre_web_abram_cand_plusvar)

# Do the same analyses in smaller subset of white sample (randomly sample 270 obs.)
small_blk_df = subset(anes_limited_pure, race == 1)
small_latinx_df = subset(anes_limited_pure, race == 2)
white_df = subset(anes_limited_pure, race == 3)
set.seed(123)
small_white_df = sample_n(white_df, 270)

small_anes_limited = rbind(small_blk_df, small_latinx_df, small_white_df)


pre_web_abram_cand_simple_small <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                         gayrt_marry + pid_strong + immigpo_jobs + race + 
                                         resent_try +  libcpre_self, data = small_anes_limited)


pre_web_abram_cand_plusvar_small <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                          gayrt_marry + pid_strong + immigpo_jobs*race + 
                                          resent_try +  libcpre_self, data = small_anes_limited)

anova(pre_web_abram_cand_plusvar_small, pre_web_abram_cand_simple_small)

summary(pre_web_abram_cand_plusvar_small)
summary(pre_web_abram_cand_simple_small)

dominanceAnalysis(pre_web_abram_cand_simple_small)
dominanceAnalysis(pre_web_abram_cand_plusvar_small)



############## run similar analyses for racial resentment (probably ignore) ############## /

pre_web_abram_cand_resent <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                   gayrt_marry + pid_strong + immigpo_jobs + race + 
                                   resent_try +  libcpre_self, data = anes_limited_pure)


pre_web_abram_cand_resentint <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + 
                                      gayrt_marry + pid_strong + immigpo_jobs + resent_try*race + 
                                      resent_try +  libcpre_self, data = anes_limited_pure)

anova(pre_web_abram_cand_resent, pre_web_abram_cand_resentint)

summary(pre_web_abram_cand_resent)
summary(pre_web_abram_cand_resentint)

# Ajay Code End -----------------------------------------------------------




###Preliminary Regressions###

# Webster & Abramowtiz Regressions

pre_web_abram_cand <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + libcpre_self, anes_limited) #removed campaign exposure because it was insignificant in their model
summary(pre_web_abram_cand)
stargazer(pre_web_abram_cand)

pre_web_abram_party <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + libcpre_self, anes_limited)
summary(pre_web_abram_party)
stargazer(pre_web_abram_party)

#webster & abramowitz regression with my added variables
pre_web_abram_cand_plusvar <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + immigpo_jobs + resent_try +  partyid +  libcpre_self, anes_limited)
summary(pre_web_abram_cand_plusvar)
stargazer(pre_web_abram_cand_plusvar)

pre_web_abram_party_plusvar <-  lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +  immigpo_jobs + resent_try +  partyid +  libcpre_self, anes_limited)
summary(pre_web_abram_party_plusvar)
stargazer(pre_web_abram_party_plusvar)

#webster & abramowitz regresson with my variable interactions 
pre_web_abram_cand_plusvarint <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong  +  partyid +  libcpre_self + immigpo_jobs * resent_try, anes_limited)
summary (pre_web_abram_cand_plusvarint)
stargazer(pre_web_abram_cand_plusvarint)

pre_web_abram_party_plusvarint <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +   partyid +  libcpre_self + immigpo_jobs * resent_try, anes_limited)
summary(pre_web_abram_party_plusvarint)
stargazer(pre_web_abram_party_plusvarint)


#RACE REGRESSIONS

#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC#HISPANIC

# Webster & Abramowtiz Regressions

hisp_pre_web_abram_cand <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + libcpre_self, hispdata) #removed campaign exposure because it was insignificant in their model
summary(hisp_pre_web_abram_cand)
stargazer(hisp_pre_web_abram_cand)

hisp_pre_web_abram_party <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong+ libcpre_self, hispdata)
summary(hisp_pre_web_abram_party)
stargazer(hisp_pre_web_abram_party)

#webster & abramowitz regression with my added variables
hisp_pre_web_abram_cand_plusvar <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + immigpo_jobs + resent_try +  partyid +  libcpre_self, hispdata)
summary(hisp_pre_web_abram_cand_plusvar)
stargazer(hisp_pre_web_abram_cand_plusvar)

hisp_pre_web_abram_party_plusvar <-  lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +  immigpo_jobs + resent_try +  partyid +  libcpre_self, hispdata)
summary(hisp_pre_web_abram_party_plusvar)
stargazer(hisp_pre_web_abram_party_plusvar)

#webster & abramowitz regresson with my variable interactions 
hisp_pre_web_abram_cand_plusvarint <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong  +  partyid +  libcpre_self + immigpo_jobs * resent_try, hispdata)
summary(hisp_pre_web_abram_cand_plusvarint)
stargazer(hisp_pre_web_abram_cand_plusvarint)

hisp_pre_web_abram_party_plusvarint <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +   partyid +  libcpre_self + immigpo_jobs * resent_try, hispdata)
summary(hisp_pre_web_abram_party_plusvarint)
stargazer(hisp_pre_web_abram_party_plusvarint)


#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK#BLACK

# Webster & Abramowtiz Regressions

black_pre_web_abram_cand <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong+ libcpre_self, blackdata) #removed campaign exposure because it was insignificant in their model
summary(black_pre_web_abram_cand)
stargazer(black_pre_web_abram_cand)

black_pre_web_abram_party <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong+ libcpre_self, blackdata)
summary(black_pre_web_abram_party)
stargazer(black_pre_web_abram_party)

#webster & abramowitz regression with my added variables
black_pre_web_abram_cand_plusvar <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + immigpo_jobs + resent_try +  partyid +  libcpre_self, blackdata)
summary(black_pre_web_abram_cand_plusvar)
stargazer(black_pre_web_abram_cand_plusvar)

black_pre_web_abram_party_plusvar <-  lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +  immigpo_jobs + resent_try +  partyid +  libcpre_self, blackdata)
summary(black_pre_web_abram_party_plusvar)
stargazer(black_pre_web_abram_party_plusvar)

#webster & abramowitz regresson with my variable interactions 
black_pre_web_abram_cand_plusvarint <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong  +  partyid +  libcpre_self + immigpo_jobs * resent_try, blackdata)
summary(black_pre_web_abram_cand_plusvarint)
stargazer(black_pre_web_abram_cand_plusvarint)

black_pre_web_abram_party_plusvarint <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +  pid_self + libcpre_self + immigpo_jobs * resent_try, blackdata)
summary(black_pre_web_abram_party_plusvarint)
stargazer(black_pre_web_abram_party_plusvarint)


#WHITE #WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE#WHITE

# Webster & Abramowtiz Regressions

white_pre_web_abram_cand <- lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong+ libcpre_self, whitedata) #removed campaign exposure because it was insignificant in their model
summary(white_pre_web_abram_cand)
stargazer(white_pre_web_abram_cand)

white_pre_web_abram_party <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong+ libcpre_self, whitedata)
summary(white_pre_web_abram_party)
stargazer(white_pre_web_abram_party)

#webster & abramowitz regression with my added variables
white_pre_web_abram_cand_plusvar <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong + immigpo_jobs + resent_try +  partyid +  libcpre_self, whitedata)
summary(white_pre_web_abram_cand_plusvar)
stargazer(white_pre_web_abram_cand_plusvar)

white_pre_web_abram_party_plusvar <-  lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +  immigpo_jobs + resent_try +  partyid +  libcpre_self, whitedata)
summary(white_pre_web_abram_party_plusvar)
stargazer(white_pre_web_abram_party_plusvar)

#webster & abramowitz regresson with my variable interactions 
white_pre_web_abram_cand_plusvarint <-  lm(aff_polpc ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong  +  partyid +  libcpre_self + immigpo_jobs * resent_try, whitedata)
summary (white_pre_web_abram_cand_plusvarint)
stargazer(white_pre_web_abram_cand_plusvarint)

white_pre_web_abram_party_plusvarint <- lm(aff_polparty ~ spsrvpr_ssself + abortpre_4point + gayrt_marry + pid_strong +   partyid +  libcpre_self + immigpo_jobs * resent_try, whitedata)
summary(white_pre_web_abram_party_plusvarint)
stargazer(white_pre_web_abram_party_plusvarint)


