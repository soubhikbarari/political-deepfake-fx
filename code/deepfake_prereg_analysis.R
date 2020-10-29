############################################################
# Run through pre-registered analyses on deepfake studies.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1 min
# 
# Input:
# - deepfake.RData:
#       `DAT` object made from `deepfake_make_data`       
#
############################################################

library(tidyverse)
library(ggplot2)

load("deepfake.RData")

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit ####
#####------------------------------------------------------#

## @SB: of the people who responded to the question (~60% of sample),
##      audio was the most deceptive medium
summary(lm(believed_true ~ treat, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

## @SB: mobile OS contributes to deception
summary(lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

DAT %>% 
    filter(!(treat%in%c("ad","control")), !is.na(treat)) %>%
    group_by(treat) %>% 
    summarise(y=mean(believed_true,na.rm=T), 
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") +
    geom_errorbar(position=position_dodge(.9), width=.2) +
    xlab("treatment condition") + ylab("level of belief in scandal") +
    theme_minimal()

#####------------------------------------------------------#
##### H2: Deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#

## @SB: video seems to produce less favorability without controls,
##      but with all controls audio produces less favorability

## "Hearing is Believing, but Seeing is Feeling (Sometimes)"

## real outcome
DAT$post_favor_ <- DAT$post_favor_Warren
## manipulation/placebo checks
DAT$post_favor_ <- DAT$post_favor_Biden
DAT$post_favor_ <- DAT$post_favor_Klobuchar
DAT$post_favor_ <- DAT$post_favor_Sanders
DAT$post_favor_ <- DAT$post_favor_Bloomberg

DAT %>%
    filter(!is.na(treat)) %>%
    group_by(treat) %>% 
    summarise(y=mean(post_favor_,na.rm=T), 
              ymax=mean(post_favor_,na.rm=T)+1.96*sd(post_favor_,na.rm=T)/sqrt(n()),
              ymin=mean(post_favor_,na.rm=T)-1.96*sd(post_favor_,na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") +
    geom_errorbar(position=position_dodge(.9), width=.2) +
    xlab("treatment condition") + ylab("favorability") +
    theme_minimal()  

summary(lm(post_favor_ ~ treat, DAT %>% filter(!is.na(treat))))
summary(lm(post_favor_ ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(post_favor_ ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(post_favor_ ~ treat, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

summary(lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

## other affective responses
## @SB: skit was found to be most funny, least informative, and most offensive (ad also found offensive)
summary(lm(believed_funny ~ treat, DAT %>% filter(!is.na(treat))))
summary(lm(believed_informative ~ treat, DAT %>% filter(!is.na(treat))))
summary(lm(believed_offensive ~ treat, DAT %>% filter(!is.na(treat))))

summary(lm(believed_funny ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(believed_informative ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(believed_offensive ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))

#####------------------------------------------------------#
##### H3: Deepfake salience effect on media trust/FPR ####
#####------------------------------------------------------#

## 3a/I: information prompt decreases media trust
## (overall, offline, online-only, social media)

## @SB: negative but statistically insignificant effect
summary(lm(post_media_trust ~ exp_1_prompt, DAT))
summary(lm(post_media_trust ~ exp_1_prompt + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

summary(lm(post_media_trust1 ~ exp_1_prompt, DAT))
summary(lm(post_media_trust1 ~ exp_1_prompt + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

summary(lm(post_media_trust2 ~ exp_1_prompt, DAT))
summary(lm(post_media_trust2 ~ exp_1_prompt + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

summary(lm(post_media_trust3 ~ exp_1_prompt, DAT))
summary(lm(post_media_trust3 ~ exp_1_prompt + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

## 3a/II: seeing and recognizing a deepfake decreases media trust
## @SB: believing something is true decreases trust in media ... but mostly
##      offline media, NOT online media
summary(lm(post_media_trust1 ~ treat*believed_true, DAT %>% filter(!is.na(treat))))
summary(lm(post_media_trust1 ~ treat*believed1_true, DAT %>% filter(!is.na(treat))))
summary(lm(post_media_trust1 ~ treat*believed_true + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(post_media_trust1 ~ treat*believed1_true + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))

## 3b/III: debrief increases false detection rate of deepfakes
## @SB: additional debrief slightly increases FDR, but its the environment/mode itself massively
##      increases FPR (i.e. low/no deepfakes)
summary(lm(exp_2_pct_false_fake ~ exp_2_after_debrief, DAT))
summary(lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2, DAT))
summary(lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2 + exp_2_prompt + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

## 3b/IV: information prompts increase false detection rate of deepfakes
## @SB: no effect of information prompt on FDR
summary(lm(exp_2_pct_false_fake ~ exp_1_prompt, DAT))
summary(lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2, DAT))
summary(lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2 + exp_2_after_debrief + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

#####------------------------------------------------------#
##### H4: Heterogeneity in deception effect by info ####
#####------------------------------------------------------#

## @SB: EXTREMELY consistent effect negative effect on deception
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

#####------------------------------------------------------#
##### H5: Heterogeneity in deception effect by cognition ####
#####------------------------------------------------------#

DAT %>%
    ggplot(aes(y=believed_true)) + 
    geom_histogram() + 
    facet_wrap(~ crt)

## @SB: no effect of CRT
summary(lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

summary(lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat))))
summary(lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","text"))))
summary(lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","audio"))))
summary(lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT %>% filter(!is.na(treat),treat %in% c("video","skit"))))

#####------------------------------------------------------#
##### H6: Heterogeneities by partisanship ####
#####------------------------------------------------------#

DAT %>%
    ggplot(aes(y=believed1_true)) + 
    geom_histogram() + 
    facet_wrap(~ PID)

## 6a: on deception
## @SB: HUGE effects of partisanship in all cases, but no heterogeneties by video type or CRT
summary(lm(believed_true ~ PID*treat, DAT))
summary(lm(believed_true ~ PID*treat*crt, DAT))
summary(lm(believed_true ~ PID*treat + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

## 6b: on favorability
## @SB: HUGE effects of partisanship, some effects that CRT x Republican partisanship is 
##      driving down favorability (actual "motivated" reasoning)
summary(lm(post_favor_Warren ~ PID*treat, DAT))
summary(lm(post_favor_Warren ~ PID*treat*crt, DAT))
summary(lm(post_favor_Warren ~ PID*treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

#####------------------------------------------------------#
##### H7: Heterogeneities by ambivalent sexism ####
#####------------------------------------------------------#

## @SB: (manipulation check) Ambivalent sexism seems to predict 
##      Warren being underfavored relative to men, but less so for Klobuchar;
##      weirdly positive interaction with Republican PID
summary(glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID, DAT, family=binomial(link="logit")))

summary(lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID, DAT))

summary(glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT, family=binomial(link="logit")))

summary(lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, DAT))

## @SB: no evidence of interaction effects
summary(lm(post_favor_Warren ~ ambivalent_sexism*treat, DAT))
summary(lm(post_favor_Warren ~ ambivalent_sexism*treat + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, DAT))
summary(lm(post_favor_Warren ~ ambivalent_sexism*treat + response_wave_ID + agegroup + educ + PID*ambivalent_sexism + polknow + internet_usage, DAT))

#####------------------------------------------------------#
##### H8: Accuracy salience and detection accuracy ####
#####------------------------------------------------------#

## @SB: accuracy prompts don't increase accuracy
summary(lm(exp_2_pct_correct ~ exp_2_prompt_accuracy, DAT))
summary(lm(exp_2_pct_correct ~ exp_2_prompt_accuracy + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, DAT))

#####------------------------------------------------------#
##### H9: Digital literacy and detection accuracy ####
#####------------------------------------------------------#

## @SB: statistically significant and positive effect of digital literacy
##      on performance
summary(lm(exp_2_pct_correct ~ post_dig_lit, DAT))
summary(lm(exp_2_pct_correct ~ post_dig_lit + exp_2 + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, DAT))
summary(lm(exp_2_pct_correct ~ post_dig_lit*agegroup + exp_2 + response_wave_ID + educ + PID + polknow + internet_usage, DAT))

## @SB: also seriously decreases false positive rate
summary(lm(exp_2_pct_false_fake ~ post_dig_lit*agegroup + exp_2 + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, DAT))
