library(tidyverse)

## can find API key in Qualtrics > Account Settings > Qualtrics ID
qualtrics_api_credentials(api_key = "GTY6dlP9xnuMaaXUq364fHJkPFIZAKbLn6jYHRuN",
                          base_url = "harvard.az1.qualtrics.com")

dfsurv_id <- "SV_8wA0COb8ufL6Xf7"
dfsurvdat <- fetch_survey(surveyID = dfsurv_id,
                     force_request = TRUE,
                     verbose = TRUE)
dfsurvqs <- survey_questions(dfsurv_id)

#####------------------------------------------------------#
##### check response quality ####
#####------------------------------------------------------#

table(dfsurvdat$Finished) ## half of people didn't finish? (these are Chris's trial runs)

hist(dfsurvdat$Progress[!dfsurvdat$Finished]) ## most are people who never start
mean(dfsurvdat$Progress[!dfsurvdat$Finished]) ## .. or don't even get halfway

sort(dfsurvdat$RecordedDate[!dfsurvdat$Finished])
sort(dfsurvdat$RecordedDate[dfsurvdat$Finished])


## takes ~10ish minutes typically?
mean(dfsurvdat$`Duration (in seconds)`/60)
median(dfsurvdat$`Duration (in seconds)`/60)
quantile(dfsurvdat$`Duration (in seconds)`/60, probs=0:10/10)

# View(dfsurvdat[!dfsurvdat$Finished,])

dfsurvdat <- dfsurvdat[dfsurvdat$Finished,]

#####------------------------------------------------------
##### clean and combine ####
#####------------------------------------------------------
{
agree_lvls <- c(NA, 
                "Strongly disagree",
                "Somewhat disagree",
                "Neither agree nor disagree",
                "Somewhat agree",
                "Strongly agree")

## age
dfsurvdat$age <- as.numeric(dfsurvdat$age)
dfsurvdat$age <- ifelse(
    dfsurvdat$age %in% 18:30, "18-30", ifelse(
        dfsurvdat$age %in% 31:40, "31-40", ifelse(
            dfsurvdat$age %in% 41:50, "41-50", ifelse(
                dfsurvdat$age %in% 51:60, "51-60", ifelse(
                    dfsurvdat$age %in% 61:100, "61+", "N/A"
                )
            )
        )
    )
)

## education
dfsurvdat$educ <- as.character(dfsurvdat$educ)
dfsurvdat$educ <- ifelse(is.na(dfsurvdat$educ), "N/A", dfsurvdat$educ)
dfsurvdat$educ <- factor(
    dfsurvdat$educ,
    levels = c(
        "N/A", "Have not finished high school",
        "High school", "College", "Postgraduate degree"
    )
)

## gender
dfsurvdat$gender <- as.character(dfsurvdat$gender)
dfsurvdat$gender <- ifelse(is.na(dfsurvdat$gender), "N/A", dfsurvdat$gender)

## partisanship
dfsurvdat$PID_main <- as.character(dfsurvdat$PID_main)
dfsurvdat$PID_leaners <- as.character(dfsurvdat$PID_leaners)

dfsurvdat$PID[dfsurvdat$PID_main=="Democrat"|dfsurvdat$PID_leaners=="Democrat"] <- "Democrat"
dfsurvdat$PID[dfsurvdat$PID_main=="Republican"|dfsurvdat$PID_leaners=="Republican"] <- "Republican"
dfsurvdat$PID[dfsurvdat$PID_main=="Independent"&!(dfsurvdat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
dfsurvdat$PID[is.na(dfsurvdat$PID)] <- "N/A"

dfsurvdat$PID <- factor(dfsurvdat$PID, levels=c("N/A","Democrat","Independent","Republican"))
    
table(dfsurvdat$PID)

### - check weird cases
table(dfsurvdat$PID_main == "Democrat" & dfsurvdat$PID_leaners == "Republican")
table(dfsurvdat$PID_main == "Republican" & dfsurvdat$PID_leaners == "Democrat")

ifelse(is.na(as.character(dfsurvdat$PID_leaners))
       & !is.na(as.character(dfsurvdat$PID_main)),
       as.character(dfsurvdat$PID_main), as.character(dfsurvdat$PID_leaners))

## sexism
dfsurvdat$ambivalent_sexism_1 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_1, 
       levels=agree_lvls))
dfsurvdat$ambivalent_sexism_2 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_2, 
       levels=agree_lvls))
dfsurvdat$ambivalent_sexism_3 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_3, 
       levels=agree_lvls))
dfsurvdat$ambivalent_sexism_4 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_4, 
       levels=agree_lvls))
# dfsurvdat$ambivalent_sexism_5 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_5, 
#       levels=agree_lvls)) ##TODO: uncomment after fielding
dfsurvdat$ambivalent_sexism <- rowMeans(dfsurvdat[grepl("ambivalent_sexism", colnames(dfsurvdat))], na.rm=T)
# hist(dfsurvdat$ambivalent_sexism) #shows mean at 3

## political knowledge
dfsurvdat$polknow1_speaker   <- as.numeric(dfsurvdat$polknow_speaker == "Nancy Pelosi")
dfsurvdat$polknow1_medicare  <- as.numeric(dfsurvdat$polknow_medicare == "A program run by the US federal government to pay for old people’s health care")
dfsurvdat$polknow1_house     <- as.numeric(dfsurvdat$polknow_house == "Democrats")
dfsurvdat$polknow1_senate    <- as.numeric(dfsurvdat$polknow_senate == "Republicans")
dfsurvdat$polknow1_veto      <- as.numeric(dfsurvdat$polknow_veto == "Two-thirds")
dfsurvdat$polknow1_justices  <- as.numeric(dfsurvdat$polknow_justices == "3")
dfsurvdat$polknow1_warren    <- as.numeric(dfsurvdat$polknow_warren == "Elizabeth Warren")
# dfsurvdat$polknow1_corbyn    <- as.numeric(as.numeric(dfsurvdat$polknow_corbyn) == "Former U.K. Labour party leader") ##TODO: uncomment after fielding
dfsurvdat$polknow1_buttigieg <- as.numeric(dfsurvdat$polknow_buttigieg == "Pete Buttigieg")
dfsurvdat$polknow1_boris     <- as.numeric(dfsurvdat$polknow_boris == "Boris Johnson")
dfsurvdat$polknow <- rowMeans(dfsurvdat[grepl("polknow1_", colnames(dfsurvdat))], na.rm=T)
# hist(dfsurvdat$polknow) #shows mean at 0.4

## media stimuli
dfsurvdat$treat_fake_text  <- as.logical(!is.na(dfsurvdat$FL_92_DO_FL_125))
dfsurvdat$treat_fake_audio <- as.logical(!is.na(dfsurvdat$FL_92_DO_FL_111))
dfsurvdat$treat_fake_video <- as.logical(!is.na(dfsurvdat$FL_92_DO_FL_124))
dfsurvdat$treat_skit       <- as.logical(!is.na(dfsurvdat$FL_92_DO_FL_129))
dfsurvdat$treat_attackad   <- as.logical(!is.na(dfsurvdat$`FL_92_DO_Experimentalstimulus:video_attack`))
dfsurvdat$treat_control    <- as.logical(!is.na(dfsurvdat$`FL_92_DO_Experimentalstimulus:control`))

### - sanity check: no respondent got randomized into two conditions
dfsurvdat$treat_fake_text + dfsurvdat$treat_fake_video + 
    dfsurvdat$treat_fake_audio + dfsurvdat$treat_skit + 
        dfsurvdat$treat_attackad + dfsurvdat$treat_control

dfsurvdat$treat <- factor(ifelse(
    dfsurvdat$treat_fake_text, "text", ifelse(
        dfsurvdat$treat_fake_audio, "audio", ifelse(
            dfsurvdat$treat_fake_video, "video", ifelse(
                dfsurvdat$treat_attackad, "ad", ifelse(
                    dfsurvdat$treat_skit, "skit", ifelse(
                        dfsurvdat$treat_control, "none", NA
                    )
                )
            )
        )
    )
), levels=c("none", "video", "audio", "text", "skit", "ad"))
table(dfsurvdat$treat)

### - sanity check: those who didn't get assigned dropped out
dfsurvdat$Finished[is.na(dfsurvdat$treat)]

dfsurvdat$believed_true <- 
    rowSums(cbind(
        as.numeric(factor(dfsurvdat$bidenshit_fake_3, levels=agree_lvls)),
        as.numeric(factor(dfsurvdat$trumpshit_fake_3, levels=agree_lvls)),
        as.numeric(factor(dfsurvdat$cherokee_fake_3, levels=agree_lvls)),
        as.numeric(factor(dfsurvdat$lgbtq_fake_3, levels=agree_lvls)),
        as.numeric(factor(dfsurvdat$loans_fake_3, levels=agree_lvls))
), na.rm=T)
hist(dfsurvdat$believed_true)
dfsurvdat$believed_attackad_true <- as.numeric(factor(dfsurvdat$attackad_fake_3, levels=agree_lvls))

dfsurvdat$believed1_true <- as.numeric(dfsurvdat$believed_true > 3)
dfsurvdat$believed1_attackad_true <- as.numeric(dfsurvdat$believed_attackad_true > 3)

## info treatment
dfsurvdat$exp_1_prompt_control <- !is.na(dfsurvdat$`FL_80_DO_Experimentalprompt:control` == 1)
dfsurvdat$exp_1_prompt_info <- !is.na(dfsurvdat$`FL_80_DO_Experimentalprompt:information` == 1)
dfsurvdat$exp_1_prompt <- factor(ifelse(
    dfsurvdat$exp_1_prompt_control, "control", ifelse(
        dfsurvdat$exp_1_prompt_info, "info", NA)
), levels=c("control", "info"))

### - sanity check: those who didn't get assigned dropped out
dfsurvdat$Finished[is.na(dfsurvdat$exp_1_prompt)]


## debrief before ID task
dfsurvdat$exp_2_prompt_accuracy <- !is.na(dfsurvdat$FL_103_DO_preIDaccuracy)
dfsurvdat$exp_2_prompt_control <- !is.na(dfsurvdat$FL_103_DO_preIDcontrol)
dfsurvdat$exp_2_prompt <- factor(ifelse(
    dfsurvdat$exp_2_prompt_control, "control", ifelse(
        dfsurvdat$exp_2_prompt_accuracy, "accuracy", NA
    )
))

### - sanity check: those who didn't get assigned dropped out
dfsurvdat$Finished[is.na(dfsurvdat$exp_2_prompt)]

## feelings thermometer
dfsurvdat$post_favor_Klobuchar <- dfsurvdat$post_favor_1
dfsurvdat$post_favor_Sanders   <- dfsurvdat$post_favor_2
dfsurvdat$post_favor_Warren    <- dfsurvdat$post_favor_3
dfsurvdat$post_favor_Biden     <- dfsurvdat$post_favor_4
dfsurvdat$post_favor_Bloomberg <- dfsurvdat$post_favor_5

## ID task
### - annotate which condition, corrections/FPRs
dfsurvdat$exp_2_nofake <- !is.na(apply(dfsurvdat[,grepl("VideoIDnofake_", colnames(dfsurvdat))], 1, any))
dfsurvdat$exp_2_lofake <- !is.na(apply(dfsurvdat[,grepl("VideoIDlowfakes_", colnames(dfsurvdat))], 1, any))
dfsurvdat$exp_2_hifake <- !is.na(apply(dfsurvdat[,grepl("VideoIDhighfakes_", colnames(dfsurvdat))], 1, any))
dfsurvdat$exp_2 <- factor(
    ifelse(
        dfsurvdat$exp_2_nofake, "nofake", ifelse(
               dfsurvdat$exp_2_lofake, "lofake", ifelse(
                   dfsurvdat$exp_2_hifake, "hifake", NA
               )
           )
    )
)
### - sanity check
as.numeric(dfsurvdat$exp_2_nofake) + as.numeric(dfsurvdat$exp_2_lofake) + as.numeric(dfsurvdat$exp_2_hifake)
dfsurvdat$Finished[is.na(dfsurvdat$exp_2)]

isfake <- "This video has been manipulated"
isreal <- "This video has not been manipulated"

### - grade `no fakes` respondents
dfsurvdat$exp_2_pct_correct[dfsurvdat$exp_2_nofake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_nofake, c("Q350","Q373","real_biden_stumble","Q352","Q353","Q354","Q355","Q356")],
    1,
    function(r) { 
        r[is.na(r)] <- "idk"
        mean(r == isreal,na.rm=T)
    }
)
dfsurvdat$exp_2_pct_false_real[dfsurvdat$exp_2_nofake] <- NA
dfsurvdat$exp_2_pct_false_fake[dfsurvdat$exp_2_nofake] <- 1-dfsurvdat$exp_2_pct_correct[dfsurvdat$exp_2_nofake]

### - grade `low fakes` respondents
dfsurvdat$exp_2_pct_correct[dfsurvdat$exp_2_lofake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_lofake, c("Q349","Q357","Q358","Q359","Q360","Q361","Q362","Q363")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r == c(isfake,isfake,isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T)
    }
)
dfsurvdat$exp_2_pct_false_real[dfsurvdat$exp_2_lofake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_lofake, c("Q349","Q357","Q358","Q359","Q360","Q361","Q362","Q363")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r[1:2] == c(isreal,isreal), na.rm=T)
    }
)
dfsurvdat$exp_2_pct_false_fake[dfsurvdat$exp_2_lofake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_lofake, c("Q349","Q357","Q358","Q359","Q360","Q361","Q362","Q363")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r[3:8] == c(isfake,isfake,isfake,isfake,isfake,isfake), na.rm=T)
    }
)

### - grade `high fakes` respondents
dfsurvdat$exp_2_pct_correct[dfsurvdat$exp_2_hifake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_hifake, c("Q364","Q365","Q366","Q371","Q367","Q368","Q369","Q370")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r == c(isfake,isfake,isfake,isfake,isfake,isfake,isreal,isreal), na.rm=T)
    }
)
dfsurvdat$exp_2_pct_false_real[dfsurvdat$exp_2_hifake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_hifake, c("Q364","Q365","Q366","Q371","Q367","Q368","Q369","Q370")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r[1:6] == c(isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T)
    }
)
dfsurvdat$exp_2_pct_false_fake[dfsurvdat$exp_2_hifake] <- apply(
    dfsurvdat[dfsurvdat$exp_2_hifake, c("Q364","Q365","Q366","Q371","Q367","Q368","Q369","Q370")],
    1,
    function(r) {
        r[is.na(r)] <- "idk"
        mean(r[7:8] == c(isfake,isfake), na.rm=T)
    }
)

## media trust
dfsurvdat$post_media_trust1 <- as.numeric(factor(dfsurvdat$post_media_trust1, 
                                            levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
dfsurvdat$post_media_trust2 <- as.numeric(factor(dfsurvdat$post_media_trust2, 
                                            levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
dfsurvdat$post_media_trust3 <- as.numeric(factor(dfsurvdat$post_media_trust3, 
                                            levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
dfsurvdat$post_media_trust  <- rowMeans(cbind(dfsurvdat$post_media_trust1, dfsurvdat$post_media_trust2, dfsurvdat$post_media_trust3))

## digital literacy
dfsurvdat$post_dig_lit_1 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_1))
dfsurvdat$post_dig_lit_2 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_2))
dfsurvdat$post_dig_lit_3 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_3))
dfsurvdat$post_dig_lit_4 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_4))
dfsurvdat$post_dig_lit_5 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_5))
dfsurvdat$post_dig_lit_6 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_6))
dfsurvdat$post_dig_lit_7 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_7))
dfsurvdat$post_dig_lit   <- rowMeans(cbind(
    dfsurvdat$post_dig_lit_1, dfsurvdat$post_dig_lit_2, 
    dfsurvdat$post_dig_lit_3, dfsurvdat$post_dig_lit_4, 
    dfsurvdat$post_dig_lit_5, dfsurvdat$post_dig_lit_6, 
    dfsurvdat$post_dig_lit_7)
)

dfsurvdat$internet_usage <- as.numeric(factor(dfsurvdat$internet_usage,
                                         levels=rev(levels(dfsurvdat$internet_usage))))

## CRT
dfsurvdat$post_crt1_1 <- (dfsurvdat$post_crt_1==0.05|dfsurvdat$post_crt_1==5)&!is.na(dfsurvdat$post_crt_1) ##TODO: spot-check this
dfsurvdat$post_crt1_2 <- dfsurvdat$post_crt_2==5&!is.na(dfsurvdat$post_crt_2 == 5)
dfsurvdat$post_crt1_3 <- dfsurvdat$post_crt_3==47&!is.na(dfsurvdat$post_crt_3)
dfsurvdat$post_crt <- dfsurvdat$post_crt1_1 + dfsurvdat$post_crt1_2 + dfsurvdat$post_crt1_3
}
