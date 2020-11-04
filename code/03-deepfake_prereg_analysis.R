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
# - code/deepfake.Rdata:
#       `dat` object made from `deepfake_make_data`       
#
# Output:
# - results/
############################################################

library(tidyverse)
library(ggplot2)
library(broom)

rm(list=ls())
load("code/deepfake.Rdata")

# dat <- dat[as.numeric(dat$quality) == 1,] ## cond on low quality
dat <- dat[as.numeric(dat$quality) == 2,] ## cond on hi quality

#####------------------------------------------------------#
#####  Helpers ####
#####------------------------------------------------------#

coefviz <- function(df, ylab_="y", title_="") {
    df %>% 
        mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                      "black", "gray"))) %>%
        ggplot(aes(x=term, 
                   y=estimate, 
                   ymin=estimate-1.96*std.error, 
                   ymax=estimate+1.96*std.error,
                   color=sig)) + 
        geom_pointrange() + 
        coord_flip() + 
        scale_color_identity() + 
        xlab("") + ylab(ylab_) +
        theme_bw() + geom_hline(yintercept=0, lty=2, alpha=0.5) + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
        ) + ggtitle(title_)
}

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit ####
#####------------------------------------------------------#

## @SB: of the people who responded to the question (~60% of sample),
##      audio was the most deceptive medium
(m <- lm(believed_true ~ treat, dat)); summary(m); 
tidy(m) %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "fake text headline")) %>%
    mutate(term = replace(term, term == "treataudio", "fake audio leak")) %>%
    mutate(term = replace(term, term == "treatskit", "satirical skit")) %>%
    mutate(term = fct_relevel(term, "fake audio leak", after=1)) %>%
    coefviz(ylab = "effect on level of deception\n(relative to full video deepfake)", title = "no other controls")
ggsave(file = "results/firststage_treatfx.pdf", width=6, height=3)
system("open results/firststage_treatfx.pdf")

(m <- lm(believed1_true ~ treat, dat)); summary(m); 
tidy(m) %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "fake text headline")) %>%
    mutate(term = replace(term, term == "treataudio", "fake audio leak")) %>%
    mutate(term = replace(term, term == "treatskit", "satirical skit")) %>%
    mutate(term = fct_relevel(term, "fake audio leak", after=1)) %>%
    coefviz(ylab = "effect on deception\n(relative to full deepfake video)", title = "no other controls")
ggsave(file = "results/firststage_treatfx_binary.pdf", width=6, height=3)
system("open results/firststage_treatfx.pdf")


## @SB: holds after controls
(m <- lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat)))); summary(m);
tidy(m) %>%
    filter(grepl("treat", term)) %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "fake text headline")) %>%
    mutate(term = replace(term, term == "treataudio", "fake audio leak")) %>%
    mutate(term = replace(term, term == "treatskit", "satirical skit")) %>%
    mutate(term = fct_relevel(term, "fake audio leak", after=1)) %>%
    coefviz(ylab = "effect on deception\n(relative to full video deepfake)", title = "adjusted for controls")
ggsave(file = "results/firststage_treatfx_controlled.pdf", width=5, height=3)
system("open results/firststage_treatfx_controlled.pdf")

(m <- lm(believed1_true ~ treat + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat)))); summary(m);
tidy(m) %>%
    filter(grepl("treat", term)) %>%
    filter(term != "(Intercept)", term != "treatad") %>%
    mutate(term = replace(term, term == "treattext", "fake text headline")) %>%
    mutate(term = replace(term, term == "treataudio", "fake audio leak")) %>%
    mutate(term = replace(term, term == "treatskit", "satirical skit")) %>%
    mutate(term = fct_relevel(term, "fake audio leak", after=1)) %>%
    coefviz(ylab = "effect on deception\n(relative to full video deepfake)", title = "adjusted for controls")
ggsave(file = "results/firststage_treatfx_binary_controlled.pdf", width=5, height=3)


dat %>% 
    mutate(treat = as.character(treat)) %>%
    filter(!(treat%in%c("ad","control")), !is.na(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "fake text\nheadline")) %>%
    mutate(treat = replace(treat, treat == "audio", "fake audio\nleak")) %>%
    mutate(treat = replace(treat, treat == "video", "full deepfake\nvideo")) %>%
    mutate(treat = replace(treat, treat == "skit", "satirical\nskit")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=4)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=4)) %>%
    group_by(treat) %>% 
    summarise(y=mean(believed_true,na.rm=T), 
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    xlab("presentation of scandal") + ylab("level of deception") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave(file = "results/firststage_deception.pdf", width=5, height=4)

dat %>% 
    mutate(treat = as.character(treat)) %>%
    filter(!(treat%in%c("ad","control")), !is.na(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "fake text\nheadline")) %>%
    mutate(treat = replace(treat, treat == "audio", "fake audio\nleak")) %>%
    mutate(treat = replace(treat, treat == "video", "full deepfake\nvideo")) %>%
    mutate(treat = replace(treat, treat == "skit", "satirical\nskit")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=4)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=4)) %>%
    group_by(treat) %>% 
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ggplot(aes(x=treat, y=y)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    xlab("presentation of scandal") + ylab("% believe clipping is real") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave(file = "results/firststage_deception_binary.pdf", width=5, height=4)
system("open results/firststage_deception_binary.pdf")

#####------------------------------------------------------#
##### H2: Deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#

## @SB: no difference in medium in unfavorability effect

## real outcome
dat$post_favor_ <- dat$post_favor_Warren
## manipulation/placebo checks
dat$post_favor_ <- dat$post_favor_Biden
dat$post_favor_ <- dat$post_favor_Klobuchar
dat$post_favor_ <- dat$post_favor_Sanders
dat$post_favor_ <- dat$post_favor_Bloomberg

dat %>%
    filter(!is.na(treat)) %>%
    group_by(treat) %>% 
    summarise(y=mean(post_favor_,na.rm=T), 
              ymax=mean(post_favor_,na.rm=T)+1.96*sd(post_favor_,na.rm=T)/sqrt(n()),
              ymin=mean(post_favor_,na.rm=T)-1.96*sd(post_favor_,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "fake text\nheadline")) %>%
    mutate(treat = replace(treat, treat == "audio", "fake audio\nleak")) %>%
    mutate(treat = replace(treat, treat == "video", "full deepfake\nvideo")) %>%
    mutate(treat = replace(treat, treat == "skit", "satirical\nskit")) %>%
    mutate(treat = replace(treat, treat == "ad", "campaign\nattack ad")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "campaign\nattack ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=5)) %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(limits=c(30,50),oob = scales::rescale_none) +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    geom_vline(xintercept=2.5, lty=2, size=1) +
    xlab("      reference stimuli                   presentation of scandal") + ylab("Elizabeth Warren feeling thermometer") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18, hjust=0),
            axis.title.y = element_text(size=18)
        )  
ggsave("results/firststage_feelings.pdf", width=8, height=5)
system("open results/firststage_feelings.pdf")

(m <- lm(post_favor_ ~ treat, dat)); summary(m);
(m <- lm(post_favor_ ~ I(treat=="control"), dat %>% filter(!is.na(treat),!(treat %in% c("skit","ad"))))); summary(m); 

(m <- lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male"), dat)); summary(m); 
(m <- lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","text")))); summary(m); 
(m <- lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","audio")))); summary(m); 
(m <- lm(post_favor_ ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","skit")))); summary(m); 
(m <- lm(post_favor_ ~ I(treat=="control") + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat),!(treat %in% c("skit","ad"))))); summary(m); 


## other affective responses
## @SB: skit was found to be most funny, least informative, and most offensive (ad also found offensive)
(m <- lm(believed_funny ~ treat, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_informative ~ treat, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_offensive ~ treat, dat %>% filter(!is.na(treat)))); summary(m); 

(m <- lm(believed_funny ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_informative ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_offensive ~ treat + response_wave_ID + meta_OS + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 

#####------------------------------------------------------#
##### H3: Deepfake salience effect on media trust/FPR ####
#####------------------------------------------------------#

## 3a/I: information prompt decreases media trust
## (overall, offline, online-only, social media)

## @SB: no effect
(m <- lm(post_media_trust ~ exp_1_prompt, dat)); summary(m); 
(m <- lm(post_media_trust ~ exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 

(m <- lm(post_media_trust1 ~ exp_1_prompt, dat)); summary(m); 
(m <- lm(post_media_trust1 ~ exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 

(m <- lm(post_media_trust2 ~ exp_1_prompt, dat)); summary(m); 
(m <- lm(post_media_trust2 ~ exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 

(m <- lm(post_media_trust3 ~ exp_1_prompt, dat)); summary(m); 
(m <- lm(post_media_trust3 ~ exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 

## 3a/II: seeing and recognizing a deepfake decreases media trust

## @SB: believing scandal is true decreases trust in media ... but mostly
##      offline media, NOT online media; may not be moving social media trust 
##      becauase there's a floor there -- no heterogeneity by medium


(m <- lm(post_media_trust1 ~ believed_true, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust2 ~ believed_true, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust3 ~ believed_true, dat %>% filter(!is.na(treat)))); summary(m); 

(m <- lm(post_media_trust ~ treat*believed_true, dat %>% filter(treat %in% c("control","video","audio","text")))); summary(m); 
(m <- lm(post_media_trust ~ treat*believed1_true, dat %>% filter(treat %in% c("control","video","audio","text")))); summary(m); 
(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed_true, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed1_true, dat %>% filter(!is.na(treat)))); summary(m); 

(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed1_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ I(treat=="video"|treat=="audio")*believed1_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 

(m <- lm(post_media_trust ~ believed_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ believed1_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ believed_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(post_media_trust ~ believed1_true + exp_1_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 

## 3b/III: debrief increases false detection rate of deepfakes
## @SB: additional debrief definitely increases FDR; environment/mode itself also massively
##      increases FPR (i.e. low/no deepfakes)
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief, dat)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2, dat)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief + exp_2 + exp_2_prompt + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 

## 3b/IV: information prompts increase false detection rate of deepfakes
## @SB: no effect of information prompt on FDR
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt, dat)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2, dat)); summary(m); 
(m <- lm(exp_2_pct_false_fake ~ exp_1_prompt + exp_2 + exp_2_after_debrief + agegroup + educ + PID + gender + polknow + internet_usage, dat)); summary(m); 

#####------------------------------------------------------#
##### H4: Heterogeneity in deception effect by info ####
#####------------------------------------------------------#

## @SB: EXTREMELY consistent effect negative effect on deception
(m <- lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m);
(m <- lm(believed1_true ~ treat*exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed1_true ~ treat*exp_1_prompt_info + response_wave_ID + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m);


### visualise just by info condition
dat %>% 
    group_by(exp_1_prompt) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "information on\ndeepfakes provided")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "no information\nprovided")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "no information\nprovided", after=0)) %>%
    ggplot(aes(x=exp_1_prompt, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("information condition") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byinfo.pdf", width=4, height=4.5)
system("open results/firststage_belief_byinfo.pdf")

dat %>% 
    group_by(exp_1_prompt) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "information on\ndeepfakes provided")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "no information\nprovided")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "no information\nprovided", after=0)) %>%
    ggplot(aes(x=exp_1_prompt, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("information condition") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byinfo_binary.pdf", width=4, height=4.5)
system("open results/firststage_belief_byinfo_binary.pdf")

### visualise by info condition and medium
dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    group_by(exp_1_prompt, treat) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "information on\ndeepfakes provided")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "no information\nprovided")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "no information\nprovided", after=0)) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "fake text\nheadline")) %>%
    mutate(treat = replace(treat, treat == "audio", "fake audio\nleak")) %>%
    mutate(treat = replace(treat, treat == "video", "full deepfake\nvideo")) %>%
    mutate(treat = replace(treat, treat == "skit", "satirical\nskit")) %>%
    mutate(treat = replace(treat, treat == "ad", "campaign\nattack ad")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "campaign\nattack ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("presentation of scandal") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(2,3.75),oob = scales::rescale_none) +
    facet_wrap(~exp_1_prompt) +
    geom_hline(data = dat.viz %>% group_by(exp_1_prompt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byinfo2.pdf", width=8, height=4.5)
system("open results/firststage_belief_byinfo2.pdf")

dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    group_by(exp_1_prompt, treat) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "information on\ndeepfakes provided")) %>%
    mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "no information\nprovided")) %>%
    mutate(exp_1_prompt = fct_relevel(as.factor(exp_1_prompt), "no information\nprovided", after=0)) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = replace(treat, treat == "text", "fake text\nheadline")) %>%
    mutate(treat = replace(treat, treat == "audio", "fake audio\nleak")) %>%
    mutate(treat = replace(treat, treat == "video", "full deepfake\nvideo")) %>%
    mutate(treat = replace(treat, treat == "skit", "satirical\nskit")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "satirical\nskit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("presentation of scandal") +
    # scale_y_continuous(limits=c(2,3.75),oob = scales::rescale_none) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    facet_wrap(~exp_1_prompt) +
    geom_hline(data = dat.viz %>% group_by(exp_1_prompt) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byinfo_binary2.pdf", width=8, height=4.5)
system("open results/firststage_belief_byinfo_binary2.pdf")

#####------------------------------------------------------#
##### H5: Heterogeneity in deception effect by cognition ####
#####------------------------------------------------------#

## @SB: no effect of CRT
(m <- lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat),treat %in% c("video","text")))); summary(m); 
(m <- lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat),treat %in% c("video","audio")))); summary(m); 
(m <- lm(believed_true ~ treat*crt + exp_1_prompt_info + response_wave_ID, dat %>% filter(!is.na(treat),treat %in% c("video","skit")))); summary(m); 

(m <- lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat %>% filter(!is.na(treat)))); summary(m); 
(m <- lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","text")))); summary(m); 
(m <- lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","audio")))); summary(m); 
(m <- lm(believed_true ~ treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat %>% filter(!is.na(treat),treat %in% c("video","skit")))); summary(m); 

#####------------------------------------------------------#
##### H6a: Heterogeneities in deception by partisanship ####
#####------------------------------------------------------#

dat %>%
    ggplot(aes(y=believed1_true)) + 
    geom_histogram() + 
    facet_wrap(~ PID)

## @SB: HUGE effects of partisanship in all cases, but no heterogeneity by video type or CRT
(m <- lm(believed_true ~ PID*treat, dat)); summary(m); 
(m <- lm(believed_true ~ PID*treat*crt, dat)); summary(m); 
(m <- lm(believed_true ~ PID*treat + response_wave_ID + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat)); summary(m); 
(m <- lm(believed_true ~ PID*treat + response_wave_ID + agegroup + educ + PID + I(gender=="Male") + polknow + internet_usage, dat %>% filter( !(treat %in% c("control","skit","ad")) ))); summary(m); 


### visualise just by PID
dat %>% 
    group_by(PID) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    ggplot(aes(x=PID, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byPID.pdf", width=4.75, height=4.5)
system("open results/firststage_belief_byPID.pdf")

dat %>% 
    group_by(PID) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    ggplot(aes(x=PID, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byPID_binary.pdf", width=4.75, height=4.5)
system("open results/firststage_belief_byPID_binary.pdf")

### visualise by PID and medium
dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    group_by(PID, treat) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("presentation of scandal") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(2,4),oob = scales::rescale_none) +
    facet_wrap(~PID) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byPID2.pdf", width=7, height=4.5)
system("open results/firststage_belief_byPID2.pdf")

dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    group_by(PID, treat) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("presentation of scandal") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    facet_wrap(~PID) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_byPID_binary2.pdf", width=7, height=4.5)
system("open results/firststage_belief_byPID_binary2.pdf")


#####------------------------------------------------------#
##### H6b: Heterogeneities in favorability by partisanship ####
#####------------------------------------------------------#

## @SB: HUGE effects of partisanship, some effects that CRT x Republican partisanship is 
##      driving down favorability (actual "motivated" reasoning)
(m <- lm(post_favor_Warren ~ PID*treat, dat)); summary(m); 
(m <- lm(post_favor_Warren ~ PID*treat*crt, dat)); summary(m); 
(m <- lm(post_favor_Warren ~ PID*treat*crt + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat)); summary(m); 


### visualise by PID and medium
dat.viz <- dat %>% 
    group_by(PID, treat) %>%
    summarise(y=mean(post_favor_Warren,na.rm=T),
              ymin=mean(post_favor_Warren,na.rm=T)-1.96*sd(post_favor_Warren,na.rm=T)/sqrt(n()),
              ymax=mean(post_favor_Warren,na.rm=T)+1.96*sd(post_favor_Warren,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(PID = as.character(PID)) %>%
    mutate(PID = replace(PID, PID == "Democrat", "Democrats")) %>%
    mutate(PID = replace(PID, PID == "Independent", "Independents")) %>%
    mutate(PID = replace(PID, PID == "Republican", "Republicans")) %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "control", after=0)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "ad", after=1)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))
    
dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("Elizabeth Warren feeling thermometer") + xlab("") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    geom_hline(data = dat.viz %>% group_by(PID) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    geom_vline(xintercept=2.5, size=0.5, lty=2) +
    facet_wrap(~PID) + 
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_feelings_byPID.pdf", width=8.25, height=4.5)
system("open results/firststage_feelings_byPID.pdf")


#####------------------------------------------------------#
##### H7: Heterogeneities by ambivalent sexism ####
#####------------------------------------------------------#

## @SB: (manipulation check) Ambivalent sexism seems to predict 
##      Warren being underfavored relative to men, but less so for Klobuchar;
##      weirdly positive interaction with Republican PID
(m <- glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID, dat, family=binomial(link="logit"))); summary(m); 

(m <- lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID, dat)); summary(m); 

(m <- glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 
(m <- glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + I(gender=="male") + polknow + internet_usage, dat, family=binomial(link="logit"))); summary(m); 

(m <- lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID + response_wave_ID + agegroup + educ + PID + gender + polknow + internet_usage, dat)); summary(m); 

## @SB: no evidence of interaction effects
(m <- lm(post_favor_Warren ~ ambivalent_sexism*treat, dat)); summary(m); 
(m <- lm(post_favor_Warren ~ ambivalent_sexism*treat + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, dat)); summary(m); 
(m <- lm(post_favor_Warren ~ ambivalent_sexism*treat + response_wave_ID + agegroup + educ + PID*ambivalent_sexism + polknow + internet_usage, dat)); summary(m); 
(m <- lm(post_favor_Warren ~ ambivalent_sexism*I(treat=="control") + response_wave_ID + agegroup + educ + PID*ambivalent_sexism + polknow + internet_usage,  dat %>% filter(!is.na(treat),!(treat %in% c("skit","ad"))))); summary(m); 



### visualise just by sexism
dat %>% 
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(ambivalent_sexism) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    ggplot(aes(x=ambivalent_sexism, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("level of ambivalent sexism") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_bysexism.pdf", width=4, height=4.5)
system("open results/firststage_belief_bysexism.pdf")

dat %>% 
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low", "moderate", "high"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(ambivalent_sexism) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    ggplot(aes(x=ambivalent_sexism, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    xlab("level of ambivalent sexism") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_bysexism_binary.pdf", width=4, height=4.5)
system("open results/firststage_belief_bysexism_binary.pdf")

### visualise by sexism and medium
dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low\nsexism", "moderate\nsexism", "high\nsexism"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(treat, ambivalent_sexism) %>%
    summarise(y=mean(believed_true,na.rm=T),
              ymin=mean(believed_true,na.rm=T)-1.96*sd(believed_true,na.rm=T)/sqrt(n()),
              ymax=mean(believed_true,na.rm=T)+1.96*sd(believed_true,na.rm=T)/sqrt(n())) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y, ymin=ymin, ymax=ymax)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("presentation of scandal") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_y_continuous(limits=c(1,4),oob = scales::rescale_none) +
    facet_wrap(~ambivalent_sexism) +
    geom_hline(data = dat.viz %>% group_by(ambivalent_sexism) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_bysexism2.pdf", width=7, height=4.5)
system("open results/firststage_belief_bysexism2.pdf")


dat.viz <- dat %>% 
    filter(!is.na(treat), treat != "ad", treat != "control") %>%
    mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                 labels=c("low\nsexism", "moderate\nsexism", "high\nsexism"))) %>%
    filter(!is.na(ambivalent_sexism), !is.nan(ambivalent_sexism)) %>%
    group_by(treat, ambivalent_sexism) %>%
    summarise(y=mean(believed1_true,na.rm=T)) %>%
    ungroup() %>%
    mutate(treat = as.character(treat)) %>%
    mutate(treat = fct_relevel(as.factor(treat), "skit", after=5))

dat.viz %>%
    ggplot(aes(x=treat, y=y)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("presentation of scandal") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    facet_wrap(~ambivalent_sexism) +
    geom_hline(data = dat.viz %>% group_by(ambivalent_sexism) %>% summarise(yint=mean(y)),
               aes(yintercept = yint), size=1, lty=1, color="red") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=18),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/firststage_belief_bysexism_binary2.pdf", width=7, height=4.5)
system("open results/firststage_belief_bysexism_binary2.pdf")

#####------------------------------------------------------#
##### H8: Accuracy salience and detection accuracy ####
#####------------------------------------------------------#

## @SB: accuracy prompts don't increase accuracy
(m <- lm(exp_2_pct_correct ~ exp_2_prompt_accuracy, dat)); summary(m); 
(m <- lm(exp_2_pct_correct ~ exp_2_prompt_accuracy + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, dat)); summary(m); 

#####------------------------------------------------------#
##### H9: Digital literacy and detection accuracy ####
#####------------------------------------------------------#

## @SB: statistically significant and positive effect of digital literacy
##      on performance
(m <- lm(exp_2_pct_correct ~ post_dig_lit, dat)); summary(m); 
(m <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2 + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, dat)); summary(m); 
(m <- lm(exp_2_pct_correct ~ post_dig_lit*agegroup + exp_2 + response_wave_ID + educ + PID + polknow + internet_usage, dat)); summary(m); 
(m <- lm(exp_2_pct_correct ~ post_dig_lit*PID + exp_2 + response_wave_ID + educ + PID + polknow + internet_usage, dat)); summary(m); 

## @SB: also seriously decreases false positive rate
(m <- lm(exp_2_pct_false_fake ~ post_dig_lit*agegroup + exp_2 + response_wave_ID + agegroup + educ + PID + polknow + internet_usage, dat)); summary(m);

#####------------------------------------------------------#
##### Plots for second-stage ####
#####------------------------------------------------------#

## Second stage hypotheses are a bit scattered so collecting
## all the plots for them here

## === Error decomposition ===
dat %>%
    select(exp_2, exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    tidyr::gather(key="metric",value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    filter(!is.na(exp_2)) %>%
    group_by(exp_2, metric) %>%
    summarise(
        y=mean(val,na.rm=T),
        sd=sd(val,na.rm=T)/sqrt(n())
    ) %>%
    ungroup() %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "accuracy")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "false positive\nrate")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "false negative\nrate")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low\nfake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high\nfake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no\nfake")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high\nfake", after=0)) %>%
    mutate(y = replace(y, metric == "false negative\nrate" & exp_2 == "no\nfake", 0.002)) %>%
    mutate(sd = replace(sd, metric == "false negative\nrate" & exp_2 == "no\nfake", 0)) %>%
    ggplot(aes(x=exp_2, y=y, ymin=y-1.96*sd, ymax=y+1.96*sd)) +
    facet_grid(.~ metric, scales="free_x", space="free_x") +
    geom_bar(stat="identity") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) + 
    ylab("rate (%)") + xlab("detection environment") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/secondstage_detectionbyenv.pdf", width=5.5, height=3)
system("open results/secondstage_detectionbyenv.pdf")

dat %>%
    select(exp_2, exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    tidyr::gather(key="metric",value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    filter(!is.na(exp_2)) %>%
    group_by(exp_2, metric) %>%
    summarise(
        y=mean(val,na.rm=T),
        sd=sd(val,na.rm=T)/sqrt(n())
    ) %>%
    ungroup() %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "Acc.")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "FPR")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "FNR")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake\nenvironment")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake\nenvironment")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake\nenvironment")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high-fake\nenvironment", after=0)) %>%
    mutate(y = replace(y, metric == "FNR" & exp_2 == "no-fake\nenvironment", 0.002)) %>%
    mutate(sd = replace(sd, metric == "FNR" & exp_2 == "no-fake\nenvironment", 0)) %>%
    ggplot(aes(x=metric, y=y, ymin=y-1.96*sd, ymax=y+1.96*sd)) +
    facet_grid(.~ exp_2, scales="free_x", space="free_x") +
    geom_bar(stat="identity") + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) + 
    ylab("rate (%)") + xlab("detection metrics") +
    theme_minimal() + 
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=14),
            strip.text = element_text(size=14),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/secondstage_detectionbyenv2.pdf", width=6, height=3)
system("open results/secondstage_detectionbyenv2.pdf")

## === Basic linear models ===
(m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                              exp_2 + response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m);
(m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m);
(m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 exp_2 + response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat)); summary(m);

dat.viz <- bind_rows(tidy(m1) %>% mutate(metric="accuracy"), 
                     tidy(m2) %>% mutate(metric="false\npositive rate"),
                     tidy(m3) %>% mutate(metric="false\nnegative rate"))
dat.viz %>% 
    filter(term %in% c("post_dig_lit", "exp_2_prompt_accuracy", "exp_1_prompt_infoTRUE", "exp_2_after_debrief", "polknow", "crt")) %>%
    mutate(term = replace(term, term == "post_dig_lit", "digital\nliteracy")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "exposure\ndebrief")) %>%
    mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "first-stage\ninformation provision")) %>%
    mutate(term = replace(term, term == "polknow", "political\nknowledge")) %>%
    mutate(term = replace(term, term == "crt", "cognitive\nreflection")) %>%
    mutate(term = fct_relevel(as.factor(term), "digital\nliteracy", after=3)) %>%
    mutate(term = fct_relevel(as.factor(term), "cognitive\nreflection", after=2)) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, "black", "grey"))) %>%
    ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, col=sig)) + 
    geom_pointrange(position=position_dodge(width=0.2)) + 
    xlab("") + ylab("effect on detection metric (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    geom_hline(yintercept=0, lty=2, alpha=0.5) +
    scale_color_identity() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    coord_flip() +
    theme_bw() +
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=14),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/secondstage_treatfx.pdf", width=8, height=4)
system("open results/secondstage_treatfx.pdf")

## === Linear models interacted with environment ===
dat.viz <- data.frame()
for (e in levels(dat$exp_2)) {
    (m1 <- lm(exp_2_pct_correct ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e) )); summary(m);
    (m2 <- lm(exp_2_pct_false_fake ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                 response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e))); summary(m);
    if (e != "nofake") {
        (m3 <- lm(exp_2_pct_false_real ~ post_dig_lit + exp_2_prompt_accuracy + exp_1_prompt_info + exp_2_after_debrief + polknow + crt +
                                         response_wave_ID + agegroup + educ + PID + internet_usage + ambivalent_sexism, dat %>% filter(exp_2 %in% e))); summary(m);
    }
    dat.viz <- bind_rows(dat.viz, tidy(m1) %>% mutate(metric="accuracy", exp_2=e)) 
    dat.viz <- bind_rows(dat.viz, tidy(m2) %>% mutate(metric="false\npositive rate", exp_2=e))
    if (e != "nofake") {
        dat.viz <- bind_rows(dat.viz, tidy(m3) %>% mutate(metric="false\nnegative rate", exp_2=e)) 
    }
}

dat.viz %>% 
    filter(term %in% c("post_dig_lit", "exp_2_prompt_accuracy", "exp_1_prompt_infoTRUE", "exp_2_after_debrief", "polknow", "crt")) %>%
    mutate(term = replace(term, term == "post_dig_lit", "digital\nliteracy")) %>%
    mutate(term = replace(term, term == "exp_2_after_debrief", "exposure\ndebrief")) %>%
    mutate(term = replace(term, term == "exp_1_prompt_infoTRUE", "first-stage\ninformation provision")) %>%
    mutate(term = replace(term, term == "polknow", "political\nknowledge")) %>%
    mutate(term = replace(term, term == "crt", "cognitive\nreflection")) %>%
    mutate(term = fct_relevel(as.factor(term), "digital\nliteracy", after=3)) %>%
    mutate(term = fct_relevel(as.factor(term), "cognitive\nreflection", after=2)) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake")) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "no-fake", after=0)) %>%
    mutate(exp_2 = fct_relevel(as.factor(exp_2), "high-fake", after=2)) %>%
    mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 1, 0.8))) %>%
    ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error, color=exp_2, alpha=sig)) + 
    geom_pointrange(position=position_dodge(width=0.2)) + 
    xlab("") + ylab("effect on detection metric (%)") +
    facet_wrap(~ metric, scales = "free_x") +
    geom_hline(yintercept=0, lty=2, alpha=0.5) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_alpha_discrete(guide=FALSE) + 
    scale_color_manual(values=c("black", "blue", "red"), name="environment") +
    coord_flip() +
    theme_bw() +
        theme(
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            strip.text = element_text(size=14),
            panel.spacing = unit(2, "lines"),
            axis.title.x = element_text(size=18),
            axis.title.y = element_text(size=18)
        )
ggsave("results/secondstage_treatfx_byenv.pdf", width=8, height=4)
system("open results/secondstage_treatfx_byenv.pdf")
