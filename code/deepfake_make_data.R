############################################################
# Make data from deepfake surveys.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: ~10 seconds
# 
# Instructions:
# 1. Set `dfsurv_id` to a desired Qualtrics survey ID 
# 2. Run script
# 3. Repeat 1-2 until all survey outputs appended to `datlist`
#
# Input:
# - respondent_demographics*.csv
# - id_location.csv
#
# Output:
# - deepfake.RData:
#       saved `DAT` output
# - id.csv:
#       personalized ID info of respondents (destroy immediately)
############################################################

#####------------------------------------------------------#
##### settings ####
#####------------------------------------------------------#

SAVE <- TRUE

#####------------------------------------------------------#
##### pre-amble ####
#####------------------------------------------------------#

library(tidyverse)
library(qualtRics)

## can find API key in Qualtrics > Account Settings > Qualtrics ID
qualtrics_api_credentials(api_key = "GTY6dlP9xnuMaaXUq364fHJkPFIZAKbLn6jYHRuN",
                          base_url = "harvard.az1.qualtrics.com")

dfsurv_ids <- c("SV_0xlqWlOfO10wuYl", "SV_eyxdeXOuISXzakt")

if (!("datlist" %in% ls())) {
    datlist <- list()
}

if (!("idlist" %in% ls())) {
    idlist <- list()
}

for (dfsurv_id in dfsurv_ids) {
  cat("\n",dfsurv_id,"\n")
  
  dfsurvdat <- fetch_survey(surveyID = dfsurv_id,
                     force_request = TRUE,
                     verbose = TRUE)
  dfsurvqs <- survey_questions(dfsurv_id)

  id_locations <- read_csv("id_location.csv")
  
  ## drop non-Americans and tests
  ## 5,982 --> 5494 (SV_eyxdeXOuISXzakt, CLEAN, Oct 9)
  ## 11,519 --> 11,466 (SV_0xlqWlOfO10wuYl, $2, Oct 29)
  IP_blacklist <- c("128.252.199.225") ## Chris's IP
  outofUS <- sapply(id_locations$location, function(s) {
    ss <- strsplit(s, split=",")[[1]]
    if (length(ss) == 1 | all(is.na(ss))) {
      return(TRUE)
    } else {
      if (grepl("United States", ss)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  })
  IP_blacklist <- c(IP_blacklist, id_locations$IPAddress[outofUS])
  dfsurvdat <- dfsurvdat[!(dfsurvdat$IPAddress %in% IP_blacklist),]
  
#####------------------------------------------------------#
##### filter on response quality ####
#####------------------------------------------------------#

  ## consent: 
  ## 5,982 --> 5,130 (SV_eyxdeXOuISXzakt, CLEAN, Oct 9)
  ## 11,466 --> 11,254 (SV_0xlqWlOfO10wuYl, $2, Oct 29)
  dfsurvdat <- dfsurvdat[dfsurvdat$consent == "Yes",]
  
  ## attention check (front-end): 
  ## 5,130  --> 1,476 (SV_eyxdeXOuISXzakt, CLEAN, Oct 9)
  ## 11,254 --> 4,260 (SV_0xlqWlOfO10wuYl, $2, Oct 29)
  failed1 <- dfsurvdat$easy!="Quick and easy"|dfsurvdat$wikihow!="wikiHow"|dfsurvdat$careful!="I have a question"
  failed2 <- !(!is.na(dfsurvdat$bothand_1) & !is.na(dfsurvdat$bothand_4))
  table(!failed1 & !failed2)
  dfsurvdat <- dfsurvdat[!failed1 & !failed2,]
  
  ## attention check (back-end): 
  ## 1,476 --> 1,384 (SV_eyxdeXOuISXzakt, CLEAN, Oct 9)
  ## 4,260 --> 4,061 (SV_0xlqWlOfO10wuYl, $2, Oct 29)
  failed3 <- dfsurvdat$attn_check1 != "blue"
  failed4 <- dfsurvdat$attn_check2 != 2

  table(!failed1 & !failed2 & !failed3 & !failed4)

  dfsurvdat$quality_failed_backend_attncheck <- failed3|failed4
  
  if (FALSE) {
      ## survey time: 
      ## 1,384 --> 1,180 (SV_eyxdeXOuISXzakt, CLEAN, Oct 9)
      ## 4,061 --> 3,409 (SV_0xlqWlOfO10wuYl, $2, Oct 29)
      timeq <- quantile(dfsurvdat$`Duration (in seconds)`/60, probs=0:10/10)
      tooquick <- dfsurvdat$`Duration (in seconds)`/60 < timeq[2]
      tooslow <- dfsurvdat$`Duration (in seconds)`/60 > timeq[10]
      table(tooquick | tooslow)
      dfsurvdat$quality_duration_tooquick <- as.numeric(tooquick)
      dfsurvdat$quality_duration_tooslow <- as.numeric(tooslow)
  }

#####------------------------------------------------------
##### clean and combine ####
#####------------------------------------------------------

  ## merge in demographics provided 
  dem1 <- read_csv("respondent_demographics1.csv")
  dem2 <- read_csv("respondent_demographics1.csv")
  
  dem <- bind_rows(dem1, dem2)
  dem$rid <- dem$RID
  dfsurvdat <- dfsurvdat %>% 
    left_join(dem %>%
                select(rid, Age, Gender, Zip, Region, Hispanic, Ethnicity, Education, `Political party`) %>%
                distinct(rid, .keep_all = TRUE), 
              by=c("rid")) 
  
  age_nonna <- !is.na(dfsurvdat$age) & !is.na(dfsurvdat$Age)
  table(as.numeric(dfsurvdat$age[age_nonna]) == dfsurvdat$Age[age_nonna])
  age_mismatch <- abs(as.numeric(dfsurvdat$age[age_nonna]) - dfsurvdat$Age[age_nonna]) > 10
  table(age_mismatch)

  gender_nonna <- !is.na(dfsurvdat$gender) & !is.na(dfsurvdat$Gender)
  gender_mismatch <- dfsurvdat$gender[gender_nonna] == dfsurvdat$Gender[gender_nonna]
  table(gender_mismatch)
  
  # dfsurvdat$Education_lvl <- 
  # as.numeric(dfsurvdat$educ) ##TODO
  
  # party_nonna <- !is.na(dfsurvdat$PID_main) & !is.na(dfsurvdat$`Political party`)
  # party_mismatch <- dfsurvdat$PID_main[party_nonna] == dfsurvdat$`Political party`[party_nonna]
  # table(party_mismatch)
  
  dat <- dfsurvdat[,c(
      "zip", "comments", "age", "gender", "hhi",
      "ethnicity", "hispanic", "education", "region",
      "political_party", "rid", "gender", "StartDate", 
      "EndDate"
  )]
  
  id <- dfsurvdat[,c(
    "IPAddress","ResponseId","RecipientLastName",
    "RecipientFirstName","RecipientEmail",
    "LocationLatitude","LocationLongitude","rid"
  )]
  id$survey.id <- dfsurv_id
  
  ## META >>>>>>>>>>>
  dat$meta_OS <- dfsurvdat$`meta_Operating System`
  dat$meta_OS <- ifelse(grepl("Android|iPhone|iPod|iPad", dat$meta_OS), "mobile", "desktop")
  
  quantile(as.numeric(gsub("x.*", "", dat$meta_resolution))) # 320 # 414 # 1366 # 1536 # 3840
  quantile(as.numeric(gsub(".*x", "", dat$meta_resolution))) # 317 # 768 # 812 # 900 # 2160
  
  dat$meta_resolution <- dfsurvdat$meta_Resolution
  w <- as.numeric(gsub("x.*", "", dat$meta_resolution))
  h <- as.numeric(gsub(".*x", "", dat$meta_resolution))
  w_x_h <- w*h
  w_x_h <- factor(as.numeric(cut(w_x_h, quantile(w_x_h), include.lowest=TRUE)))
  levels(w_x_h) <- c("XS","S","M","L")
  
  dat$meta_screenres <- w_x_h
  
  dat$response_wave_ID <- dfsurv_id
  dat$duration_secs <- dfsurvdat$`Duration (in seconds)`
  dat$progress <- dfsurvdat$Progress
  dat$Finished <- dfsurvdat$Finished
  
  agree_lvls <- c(NA, 
                  "Strongly disagree",
                  "Somewhat disagree",
                  "Neither agree nor disagree",
                  "Somewhat agree",
                  "Strongly agree")
  
  ## AGE >>>>>>>>>>>
  dat$agegroup <- as.numeric(dfsurvdat$age)
  dat$agegroup <- ifelse(
      dat$age %in% 18:30, "18-30", ifelse(
          dat$age %in% 31:40, "31-40", ifelse(
              dat$age %in% 41:50, "41-50", ifelse(
                  dat$age %in% 51:60, "51-60", ifelse(
                      dat$age %in% 61:100, "61+", "N/A"
                  )
              )
          )
      )
  )
  table(dat$agegroup)
  
  ## EDUCATION >>>>>>>>>>>
  dat$educ <- as.character(dfsurvdat$educ)
  dat$educ <- ifelse(is.na(dat$educ), "N/A", dat$educ)
  dat$educ <- factor(
      dat$educ,
      levels = c(
          "N/A", "Have not finished high school",
          "High school", "College", "Postgraduate degree"
      )
  )
  table(dat$educ)
  
  ## PARTISANSHIP >>>>>>>>>>>
  dat$PID_main <- as.character(dfsurvdat$PID_main)
  dat$PID_leaners <- as.character(dfsurvdat$PID_leaners)
  dat$PID[dat$PID_main=="Democrat"|dat$PID_leaners=="Democrat"] <- "Democrat"
  dat$PID[dat$PID_main=="Republican"|dat$PID_leaners=="Republican"] <- "Republican"
  dat$PID[dat$PID_main=="Independent"&!(dat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
  dat$PID[is.na(dat$PID)] <- "N/A"
  
  dat$PID <- factor(dat$PID, levels=c("N/A","Democrat","Independent","Republican"))
      
  table(dat$PID)
  table(dat$PID_leaners)
  
  ### sanity check: make sure no contradictory PIDs (should all be FALSE)
  table(dfsurvdat$PID_main == "Democrat" & dfsurvdat$PID_leaners == "Republican")
  table(dfsurvdat$PID_main == "Republican" & dfsurvdat$PID_leaners == "Democrat")
  
  ## SEXISM >>>>>>>>>>>
  dat$ambivalent_sexism_1 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_1, 
         levels=agree_lvls))
  dat$ambivalent_sexism_2 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_2, 
         levels=agree_lvls))
  dat$ambivalent_sexism_3 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_3, 
         levels=agree_lvls))
  dat$ambivalent_sexism_4 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_4, 
         levels=agree_lvls))
  dat$ambivalent_sexism_5 <- as.numeric(factor(dfsurvdat$ambivalent_sexism_5,
        levels=agree_lvls))
  dat$ambivalent_sexism <- rowMeans(dat[grepl("ambivalent_sexism", colnames(dat))], na.rm=T)
  hist(dat$ambivalent_sexism) #shows mean at 3
  
  ## POL KNOWLEDGE >>>>>>>>>>>
  dat$polknow_speaker   <- as.numeric(dfsurvdat$polknow_speaker == "Nancy Pelosi")
  dat$polknow_medicare  <- as.numeric(dfsurvdat$polknow_medicare == "A program run by the US federal government to pay for old people’s health care")
  dat$polknow_house     <- as.numeric(dfsurvdat$polknow_house == "Democrats")
  dat$polknow_senate    <- as.numeric(dfsurvdat$polknow_senate == "Republicans")
  dat$polknow_veto      <- as.numeric(dfsurvdat$polknow_veto == "Two-thirds")
  dat$polknow_warren    <- as.numeric(dfsurvdat$polknow_warren == "Elizabeth Warren")
  dat$polknow_boris     <- as.numeric(dfsurvdat$polknow_boris == "Boris Johnson")
  dat$polknow           <- rowMeans(dat[grepl("polknow_", colnames(dat))], na.rm=T)
  hist(dat$polknow)
  
  ## MEDIA STIMULI >>>>>>>>>>>
  dat$treat_control    <- replace_na(dfsurvdat$`FL_92_DO_Experimentalstimulus:control`, 0)
  dat$treat_attackad   <- replace_na(dfsurvdat$`FL_92_DO_Experimentalstimulus:video_attack`, 0)
  dat$treat_fake_text  <- replace_na(apply(dfsurvdat[,grepl("FL_125_DO_Experimentalstimulus:text", colnames(dfsurvdat))], 1, any), 0)
  dat$treat_fake_audio <- replace_na(apply(dfsurvdat[,grepl("FL_111_DO_Experimentalstimulus:audio", colnames(dfsurvdat))], 1, any), 0)
  dat$treat_fake_video <- replace_na(apply(dfsurvdat[,grepl("FL_124_DO_Experimentalstimulus:video", colnames(dfsurvdat))], 1, any), 0)
  dat$treat_skit       <- replace_na(apply(dfsurvdat[,grepl("FL_129_DO_skit", colnames(dfsurvdat))], 1, any), 0)
  ### sanity check: make sure each respondent is in one condition
  ### (should be 1 with some 0's for drop outs)
  table(
    dat$treat_control + dat$treat_attackad + 
    dat$treat_fake_text + dat$treat_fake_audio + 
    dat$treat_fake_video + dat$treat_skit
  )
  
  dat$treat <- factor(ifelse(
      dat$treat_fake_text, "text", ifelse(
          dat$treat_fake_audio, "audio", ifelse(
              dat$treat_fake_video, "video", ifelse(
                  dat$treat_attackad, "ad", ifelse(
                      dat$treat_skit, "skit", ifelse(
                          dat$treat_control, "control", NA
                      )
                  )
              )
          )
      )
  ), levels=c("control", "video", "audio", "text", "skit", "ad"))
  table(dat$treat)
  
  ## BELIEF >>>>>>>>>>>
  X <- cbind( ## disagree that is fake --> believed is real
      as.numeric(factor(dfsurvdat$bidenshit_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$trumpshit_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$cherokee_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$lgbtq_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$loans_fake_3, levels=rev(agree_lvls))),
      as.numeric(factor(dfsurvdat$attackad_fake_3, levels=rev(agree_lvls)))
  )
  dat$believed_true <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
  dat$believed1_true <- as.numeric(dat$believed_true > 3)
  
  table(is.na(dat$believed_true)); hist(dat$believed_true)
  
  dat$believed_attackad_true  <- as.numeric(factor(dfsurvdat$attackad_fake_3, levels=rev(agree_lvls)))
  dat$believed1_attackad_true <- as.numeric(dat$believed_attackad_true > 3)
  
  belief_q_idxs <- c(1,2,4)
  belief_q_names <- c("offensive","funny","informative")
  for (i in 1:3) {
      q_name <- belief_q_names[i]
      q_idx <- belief_q_idxs[i]
      X <- cbind(
          as.numeric(factor(dfsurvdat[[paste0("bidenshit_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("trumpshit_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("cherokee_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("lgbtq_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("loans_fake_",q_idx)]], levels=(agree_lvls))),
          as.numeric(factor(dfsurvdat[[paste0("attackad_fake_",q_idx)]], levels=(agree_lvls)))
      )
      dat[paste0("believed_",q_name)] <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
      dat[paste0("believed_",q_name,"1")] <- as.numeric(dat[paste0("believed_",q_name)] > 3)
  }
  rm(i)
  
  ## EXP 1 INFO TREATMENT >>>>>>>>>>>
  dat$exp_1_prompt_control <- !is.na(dfsurvdat$`FL_80_DO_Experimentalprompt:control` == 1)
  dat$exp_1_prompt_info <- !is.na(dfsurvdat$`FL_80_DO_Experimentalprompt:information` == 1)
  dat$exp_1_prompt <- factor(ifelse(
      dat$exp_1_prompt_control, "control", ifelse(
          dat$exp_1_prompt_info, "info", NA)
  ), levels=c("control", "info"))
  ### sanity check: those who didn't get assigned mostly dropped out
  dat$Finished[is.na(dat$exp_1_prompt)]
  table(is.na(dat$exp_1_prompt))
  
  ## debrief before ID task
  dat$exp_2_prompt_accuracy <- !is.na(dfsurvdat$FL_103_DO_preIDaccuracy)
  dat$exp_2_prompt_control <- !is.na(dfsurvdat$FL_103_DO_preIDcontrol)
  dat$exp_2_prompt <- factor(ifelse(
      dat$exp_2_prompt_control, "control", ifelse(
          dat$exp_2_prompt_accuracy, "accuracy", NA
      )
  ))
  ### sanity check: those who didn't get assigned mostly dropped out
  table(
  dat$Finished[is.na(dat$exp_2_prompt)]
  )
  
  ## FEELINGS THERMOMETER >>>>>>>>>>>
  dat$post_favor_Klobuchar <- dfsurvdat$post_favor_1
  dat$post_favor_Sanders   <- dfsurvdat$post_favor_2
  dat$post_favor_Warren    <- dfsurvdat$post_favor_3
  dat$post_favor_Biden     <- dfsurvdat$post_favor_4
  dat$post_favor_Bloomberg <- dfsurvdat$post_favor_5
  
  ## ID TASK >>>>>>>>>>>
  
  dat$exp_2_after_debrief <- dfsurvdat$exp_2_after_debrief
  
  ### annotate which condition, corrections/FPRs
  dat$exp_2_nofake <- !is.na(apply(dfsurvdat[,grepl("VideoIDnofake_", colnames(dfsurvdat))], 1, any))
  dat$exp_2_lofake <- !is.na(apply(dfsurvdat[,grepl("VideoIDlowfakes_", colnames(dfsurvdat))], 1, any))
  dat$exp_2_hifake <- !is.na(apply(dfsurvdat[,grepl("VideoIDhighfakes_", colnames(dfsurvdat))], 1, any))
  dat$exp_2 <- factor(
      ifelse(
          dat$exp_2_nofake, "nofake", ifelse(
                 dat$exp_2_lofake, "lofake", ifelse(
                     dat$exp_2_hifake, "hifake", NA
                 )
             )
      )
  )
  ### sanity check
  table(
  as.numeric(dat$exp_2_nofake) + as.numeric(dat$exp_2_lofake) + as.numeric(dat$exp_2_hifake)
  )
  dat$Finished[is.na(dat$exp_2)]
  
  isfake <- "This video is fake or doctored"
  isreal <- "This video is not fake or doctored"
  
  ### -- grade `no fakes` respondents
  dat$exp_2_pct_correct <- NULL
  dat$exp_2_pct_correct[dat$exp_2_nofake] <- apply(
      dfsurvdat[dat$exp_2_nofake, c("real_trump_soup","real_biden_fight",
                                    "real_biden_stumble","real_biden_stumble",
                                    "real_trump_covid","real_obama_missile",
                                    "real_obama_smoking","real_warrenbeer")],
      1,
      function(r) { 
          r[is.na(r)] <- "idk"
          mean(r == isreal,na.rm=T)
      }
  )
  table(is.na(dat$exp_2_pct_correct[dat$exp_2_nofake]))
  hist(dat$exp_2_pct_correct[dat$exp_2_nofake])
  dat$exp_2_pct_false_real[dat$exp_2_nofake] <- NA
  dat$exp_2_pct_false_fake[dat$exp_2_nofake] <- 1-dat$exp_2_pct_correct[dat$exp_2_nofake]
  
  ### -- grade `low fakes` respondents
  dat$exp_2_pct_correct[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, c("fake_trump_aids","fake_obama_buzzfeed",
                                    "real_trump_covid","real_biden_stumble",
                                    "real_trump_apple","real_obama_missile",
                                    "real_warrenbeer","real_warrenliar")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r == c(isfake,isfake,isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T)
      }
  )
  dat$exp_2_pct_false_real[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, c("fake_trump_aids","fake_obama_buzzfeed",
                                          "real_trump_covid","real_biden_stumble",
                                          "real_trump_apple","real_obama_missile",
                                          "real_warrenbeer","real_warrenliar")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r[1:2] == c(isreal,isreal), na.rm=T)
      }
  )
  dat$exp_2_pct_false_fake[dat$exp_2_lofake] <- apply(
      dfsurvdat[dat$exp_2_lofake, c("fake_trump_aids","fake_obama_buzzfeed",
                                          "real_trump_covid","real_biden_stumble",
                                          "real_trump_apple","real_obama_missile",
                                          "real_warrenbeer","real_warrenliar")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r[3:8] == c(isfake,isfake,isfake,isfake,isfake,isfake), na.rm=T)
      }
  )
  
  ### -- grade `high fakes` respondents
  dat$exp_2_pct_correct[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, c("fake_bernie1","fake_boris",
                                          "fake_trump_resign","fake_hilary2",
                                          "fake_obama_buzzfeed","fake_trump_aids",
                                          "real_bidenfight","real_warrenbeer")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r == c(isfake,isfake,isfake,isfake,isfake,isfake,isreal,isreal), na.rm=T)
      }
  )
  dat$exp_2_pct_false_real[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, c("fake_bernie1","fake_boris",
                                          "fake_trump_resign","fake_hilary2",
                                          "fake_obama_buzzfeed","fake_trump_aids",
                                          "real_bidenfight","real_warrenbeer")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r[1:6] == c(isreal,isreal,isreal,isreal,isreal,isreal), na.rm=T)
      }
  )
  dat$exp_2_pct_false_fake[dat$exp_2_hifake] <- apply(
      dfsurvdat[dat$exp_2_hifake, c("fake_bernie1","fake_boris",
                                          "fake_trump_resign","fake_hilary2",
                                          "fake_obama_buzzfeed","fake_trump_aids",
                                          "real_bidenfight","real_warrenbeer")],
      1,
      function(r) {
          r[is.na(r)] <- "idk"
          mean(r[7:8] == c(isfake,isfake), na.rm=T)
      }
  )
  
  ## MEDIA TRUST >>>>>>>>>>>
  dat$post_media_trust1 <- as.numeric(factor(dfsurvdat$post_media_trust1, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
  dat$post_media_trust2 <- as.numeric(factor(dfsurvdat$post_media_trust2, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
  dat$post_media_trust3 <- as.numeric(factor(dfsurvdat$post_media_trust3, 
                                              levels=c("None at all", "Not very much", "A fair amount", "A great deal")))
  X <- cbind(dat$post_media_trust1, dat$post_media_trust2, dat$post_media_trust3)
  
  dat$post_media_trust <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))
  
  ## DIG LIT >>>>>>>>>>>
  dat$post_dig_lit_1 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_1))
  dat$post_dig_lit_2 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_2))
  dat$post_dig_lit_3 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_3))
  dat$post_dig_lit_4 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_4))
  dat$post_dig_lit_5 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_5))
  dat$post_dig_lit_6 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_6))
  dat$post_dig_lit_7 <- 6-as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_7))
  X <- cbind(
      dat$post_dig_lit_1, dat$post_dig_lit_2, 
      dat$post_dig_lit_3, dat$post_dig_lit_4, 
      dat$post_dig_lit_5, dat$post_dig_lit_6, 
      dat$post_dig_lit_7)
  dat$post_dig_lit <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))
  
  
  dat$internet_usage <- as.numeric(factor(dfsurvdat$internet_usage,
                                          levels=rev(levels(dfsurvdat$internet_usage))))
  
  ## CRT >>>>>>>>>>>
  dat$crt1 <- as.numeric(gsub("\\$| |\\¢", "", dfsurvdat$pre_crt_1)) %in% c(5, 0.05)
  dat$crt2 <- as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_2)) %in% c(100)
  dat$crt3 <- as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_3)) %in% c(47)
  dat$crt  <- dat$crt1 + dat$crt2 + dat$crt3
  dat$crt <- ifelse(
      is.na(dfsurvdat$pre_crt_2) | is.na(dfsurvdat$pre_crt_2) | is.na(dfsurvdat$pre_crt_3), NA,
      dat$crt
  )
  hist(dat$crt)
  
  
  datlist[[dfsurv_id]] <- dat
  idlist[[dfsurv_id]] <- id
}


#####------------------------------------------------------#
##### finalize and save ####
#####------------------------------------------------------#

rm(w,h,w_x_h,failed1,failed2,isreal,isfake,agree_lvls)

## bind and save
DAT <- do.call(rbind, datlist)
DAT <- DAT[!duplicated(DAT$rid),]
colnames(DAT) <- make.unique(names(DAT))

if (SAVE) {
  save(DAT, file = "deepfake.RData")
}

ID <- do.call(rbind, idlist)

if (SAVE) {
  write.csv(ID, "id.csv", row.names = FALSE)
}
