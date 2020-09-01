library(ggplot2)

source("deepfake_make_data.R")

#####------------------------------------------------------#
##### H1: deepfakes more deceptive than text/audio/skit ####
#####------------------------------------------------------#

{ ## text: diff-in-mean + one-sided t-test
tt <- t.test(dfsurvdat$believed1_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed1_true[dfsurvdat$treat=="text"], "greater")
cat("binary ** E(Believe|video) - E(Believe|text): ",
    mean(dfsurvdat$believed1_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed1_true[dfsurvdat$treat=="text"],na.rm=T),
    "( p=",tt$p.value,")\n")

tt <- t.test(dfsurvdat$believed_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed_true[dfsurvdat$treat=="text"], "greater")
cat("ordin ** E(Believe|video) - E(Believe|text): ",
    mean(dfsurvdat$believed_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="text"],na.rm=T),
    "( p=",tt$p.value,")\n")
}

{ ## audio: diff-in-mean + one-sided t-test
tt <- t.test(dfsurvdat$believed1_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed1_true[dfsurvdat$treat=="audio"], "greater")
cat("binary ** E(Believe|video) - E(Believe|audio): ",
    mean(dfsurvdat$believed1_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed1_true[dfsurvdat$treat=="audio"],na.rm=T),
    "( p=",tt$p.value,")\n")

tt <- t.test(dfsurvdat$believed_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed_true[dfsurvdat$treat=="audio"], "greater")
cat("ordin ** E(Believe|video) - E(Believe|audio): ",
    mean(dfsurvdat$believed_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="audio"],na.rm=T),
    "( p=",tt$p.value,")\n")
}

{ ## skit: diff-in-mean + one-sided t-test
tt <- t.test(dfsurvdat$believed1_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed1_true[dfsurvdat$treat=="skit"], "greater")
cat("binary ** E(Believe|video) - E(Believe|skit): ",
    mean(dfsurvdat$believed1_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed1_true[dfsurvdat$treat=="skit"],na.rm=T),
    "( p=",tt$p.value,")\n")

tt <- t.test(dfsurvdat$believed_true[dfsurvdat$treat=="video"], 
       dfsurvdat$believed_true[dfsurvdat$treat=="skit"], "greater")
cat("ordin ** E(Believe|video) - E(Believe|skit): ",
    mean(dfsurvdat$believed_true[dfsurvdat$treat=="video"],na.rm=T) - 
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="skit"],na.rm=T),
    "( p=",tt$p.value,")\n")
}

{ ## original ordinal belief response
barplot(
    c(
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="video"],na.rm=T),
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="text"],na.rm=T),
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="audio"],na.rm=T),
        mean(dfsurvdat$believed_true[dfsurvdat$treat=="skit"],na.rm=T)
    ), names.arg=c("video","text","audio","skit"), ylab="belief in scandal (ordinal)",ylim=c(0,5)
)
segments(
    x0=p, 
    y0=c(
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_video],na.rm=T) - 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_video],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_video)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_text],na.rm=T) -
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_text],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_text)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_audio],na.rm=T) - 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_audio],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_audio)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_skit],na.rm=T) - 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_skit],na.rm=T)/sqrt(sum(dfsurvdat$treat_skit))
    ), 
    y1=c(
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_video],na.rm=T) + 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_video],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_video)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_text],na.rm=T) + 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_text],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_text)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_fake_audio],na.rm=T) + 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_fake_audio],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_audio)),
mean(dfsurvdat$believed_true[dfsurvdat$treat_skit],na.rm=T) + 
    1.96*sd(dfsurvdat$believed_true[dfsurvdat$treat_skit],na.rm=T)/sqrt(sum(dfsurvdat$treat_skit))
    )
)
}

# { ## collapsed binary belief response
# barplot(
#     c(
#         mean(dfsurvdat$believed1_true[dfsurvdat$treat_fake_video],na.rm=T),
#         mean(dfsurvdat$believed1_true[dfsurvdat$treat_fake_text],na.rm=T),
#         mean(dfsurvdat$believed1_true[dfsurvdat$treat_fake_audio],na.rm=T),
#         mean(dfsurvdat$believed1_true[dfsurvdat$treat_skit],na.rm=T)
#     ), names.arg=c("video","text","audio","skit"), ylab="belief in scandal (collapsed binary)"
# )
# }

#####------------------------------------------------------#
##### H2: deepfakes make target more unfavorable than text/audio/skit  ####
#####------------------------------------------------------#
{
p <- barplot(
    c(
        mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat=="video"],na.rm=T),
        mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat=="text"],na.rm=T),
        mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat=="audio"],na.rm=T),
        mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat=="skit"],na.rm=T),
        mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat=="ad"],na.rm=T)
    ), names.arg=c("video","text","audio","skit","ad"), ylab="Warren favorability",ylim=c(0,80)
)
segments(
    x0=p, 
    y0=c(
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_video],na.rm=T) - 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_video],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_video)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_text],na.rm=T) -
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_text],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_text)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_audio],na.rm=T) - 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_audio],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_audio)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_skit],na.rm=T) - 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_skit],na.rm=T)/sqrt(sum(dfsurvdat$treat_skit))
    ), 
    y1=c(
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_video],na.rm=T) + 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_video],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_video)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_text],na.rm=T) + 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_text],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_text)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_audio],na.rm=T) + 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_fake_audio],na.rm=T)/sqrt(sum(dfsurvdat$treat_fake_audio)),
mean(dfsurvdat$post_favor_Warren[dfsurvdat$treat_skit],na.rm=T) + 
    1.96*sd(dfsurvdat$post_favor_Warren[dfsurvdat$treat_skit],na.rm=T)/sqrt(sum(dfsurvdat$treat_skit))
    )
)
}

#####------------------------------------------------------#
##### H3: H1 effect lower for high-diglit ####
#####------------------------------------------------------#

##TODO: need to revise this in the PAP and here
h3a <- lm(believed_true ~ exp_1_prompt*treat, dfsurvdat)
summary(h3a)
h3a <- lm(believed_true ~ exp_1_prompt*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h3a)
h3a <- glm(believed_true ~ exp_1_prompt*treat, dfsurvdat, family=binomial(link="logit"))
summary(h3a)

h3b <- lm(believed_true ~ post_dig_lit*treat, dfsurvdat)
summary(h3b)
h3b <- lm(believed_true ~ post_dig_lit*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h3b)
h3b <- glm(believed_true ~ post_dig_lit*treat, dfsurvdat, family=binomial(link="logit"))
summary(h3b)

#####------------------------------------------------------#
##### H4: H1 effect lower for high-cognition ####
#####------------------------------------------------------#

##TODO: need to revise this in the PAP and here

##high-cognition via CRT score
h4a <- lm(believed_true ~ exp_2_prompt*treat, dfsurvdat)
summary(h4a)
h4a <- lm(believed_true ~ exp_2_prompt*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h4a)
h4a <- glm(believed1_true ~ exp_2_prompt*treat, dfsurvdat, family=binomial(link="logit"))
summary(h4a)

h4b <- lm(believed_true ~ post_crt*treat, dfsurvdat)
summary(h4b)
h4b <- lm(believed_true ~ post_crt*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h4b)
h4b <- glm(believed1_true ~ post_crt*treat, dfsurvdat, family=binomial(link="logit"))
summary(h4b)

#####------------------------------------------------------#
##### H5a: H1 effect higher for republicans ####
#####------------------------------------------------------#

h5a <- lm(believed_true ~ PID*treat, dfsurvdat)
summary(h5a) ##baseline
h5a <- lm(believed_true ~ PID*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h5a) ##with controls
 
#####------------------------------------------------------#
##### H5b: H2 effect higher for republicans ####
#####------------------------------------------------------#

h5b <- lm(post_favor_Warren ~ PID*treat, dfsurvdat)
summary(h5b) ##baseline
h5b <- lm(post_favor_Warren ~ PID*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h5b) ##with controls

#####------------------------------------------------------#
##### H6: H2 effect higher for ambivalent sexists ####
#####------------------------------------------------------#

##ambivalent sexism concept check
### - each female candidate systematically scored less than men (baseline)
summary(glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID, dfsurvdat, family=binomial(link="logit")))
### - all female candidates systematically scored less than all men (baseline)
summary(lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID, dfsurvdat))

### - each female candidate systematically scored less than men (with controls)
summary(glm(I(post_favor_Warren > post_favor_Sanders) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Biden) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Warren > post_favor_Bloomberg) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Sanders) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Biden) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
summary(glm(I(post_favor_Klobuchar > post_favor_Bloomberg) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat, family=binomial(link="logit")))
### - all female candidates systematically scored less than all men (with controls)
summary(lm(I(0.5*post_favor_Klobuchar + 0.5*post_favor_Warren - 0.33*post_favor_Biden - 0.33*post_favor_Bloomberg - 0.33*post_favor_Sanders) ~ ambivalent_sexism*PID + age + educ + PID + polknow + internet_usage, dfsurvdat))



h6 <- lm(post_favor_Warren ~ ambivalent_sexism*treat, dfsurvdat)
summary(h6) ##baseline
h6 <- lm(post_favor_Warren ~ ambivalent_sexism*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h6) ##with controls

#####------------------------------------------------------#
##### H7a: increased salience -> less trust in media ####
#####------------------------------------------------------#

##salience via stage 1 info prompt, baseline:
h7a_i <- lm(post_media_trust ~ exp_1_prompt, dfsurvdat)
summary(h7a_i) ##general trust in media
h7a_i <- lm(post_media_trust1 ~ exp_1_prompt, dfsurvdat)
summary(h7a_i) ##trust in offline media
h7a_i <- lm(post_media_trust2 ~ exp_1_prompt, dfsurvdat)
summary(h7a_i) ##trust in online media
h7a_i <- lm(post_media_trust3 ~ exp_1_prompt, dfsurvdat)
summary(h7a_i) ##trust in social media

##salience via stage 1 info prompt, with controls:
h7a_i <- lm(post_media_trust ~ exp_1_prompt + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_i) ##general trust in media
h7a_i <- lm(post_media_trust1 ~ exp_1_prompt + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_i) ##trust in offline media
h7a_i <- lm(post_media_trust2 ~ exp_1_prompt + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_i) ##trust in online media
h7a_i <- lm(post_media_trust2 ~ exp_1_prompt + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_i) ##trust in social media

##salience via exposure-and-detection, baseline:
h7a_ii <- lm(post_media_trust ~ I(treat == "video")*believed_true, dfsurvdat)
summary(h7a_ii) ##general trust in media
h7a_ii <- lm(post_media_trust1 ~ I(treat == "video")*believed_true, dfsurvdat)
summary(h7a_ii) ##trust in offline media
h7a_ii <- lm(post_media_trust2 ~ I(treat == "video")*believed_true, dfsurvdat)
summary(h7a_ii) ##trust in online media
h7a_ii <- lm(post_media_trust3 ~ I(treat == "video")*believed_true, dfsurvdat)
summary(h7a_ii) ##trust in social media

##salience via exposure-and-detection, with controls:
h7a_ii <- lm(post_media_trust ~ I(treat == "video")*believed_true + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_ii)
h7a_ii <- lm(post_media_trust1 ~ I(treat == "video")*believed_true + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_ii)
h7a_ii <- lm(post_media_trust2 ~ I(treat == "video")*believed_true + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_ii)
h7a_ii <- lm(post_media_trust3 ~ I(treat == "video")*believed_true + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7a_ii) 


#####------------------------------------------------------#
##### H7b: increased salience -> increased FPR in ID task ####
#####------------------------------------------------------#

##salience via stage 1 info prompt:
h7b_i <- lm(exp_2_pct_false_fake ~ exp_1_prompt, dfsurvdat)
summary(h7b_i) ##baseline
h7b_i <- lm(exp_2_pct_false_fake ~ exp_1_prompt + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7b_i) ##with controls

##salience via exposure-and-detection: 
h7b_ii <- lm(exp_2_pct_false_fake ~ I(treat == "video")*believed_true*treat, dfsurvdat)
summary(h7b_ii) ##baseline
h7b_ii <- lm(exp_2_pct_false_fake ~ I(treat == "video")*believed_true*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7b_ii) ##with controls

##salience via debriefing:
h7b_iii <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief, dfsurvdat)
summary(h7b_iii) ##baseline
h7b_iii <- lm(exp_2_pct_false_fake ~ exp_2_after_debrief*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7b_iii) ##with controls

##salience via stage 2 acc prompt:
h7b_iv <- lm(exp_2_pct_false_fake ~ exp_2_prompt, dfsurvdat)
summary(h7b_iv) ##baseline
h7b_ii <- lm(exp_2_pct_false_fake ~ believed_true*treat + age + educ + PID + polknow + internet_usage, dfsurvdat)
summary(h7b_iv) ##with controls
