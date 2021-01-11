# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run through exploratory analyses on deepfake studies.
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
#       contains `dat` object with weights appended to a 
#       column from step 1.     
#
# Output:
# - figures_exploratory/*
# - tables_exploratory/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(optparse)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)

rm(list=ls())
setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/deepfakes_project")
load("code/deepfake.Rdata")

if (!file.exists("tables_exploratory")) {
    system("mkdir tables_exploratory")
}
if (!file.exists("figures_exploratory")) {
    system("mkdir figures_exploratory")
}

COVARS <- c("educ", "meta_OS", "age_65", "PID", "crt", "gender", "polknow", 
            "internet_usage", "ambivalent_sexism")

#####------------------------------------------------------#
##### Settings ####
#####------------------------------------------------------#

arg_list <- list(     
    make_option(c("--response_quality"), type="character", default="all", 
        help="Which quality of responses to condition on.",
        metavar="response_quality"),
    # make_option(c("--weight"), type="numeric", default=0,
    #             help="Use weights?",
    #             metavar="weight"),
    make_option(c("--show_pdfs"), type="numeric", default=0,
                help="Show PDFs in real time?",
                metavar="show_pdfs")
)
ARGS <- parse_args(OptionParser(option_list=arg_list))

SHOW_PDFS <- ARGS$show_pdfs

dat$lowq <- FALSE
dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE

# if (ARGS$response_quality == "low") {
#     dat <- dat[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch,] ## cond on low quality    
# }
# 
# if (ARGS$response_quality == "high") {
#     dat <- dat[!dat$quality_pretreat_duration_tooquick & !dat$quality_pretreat_duration_tooslow & !dat$quality_demographic_mismatch,] ## cond on high quality
# }

# if (ARGS$weight == 0 | !("weight" %in% colnames(dat))) {
#     dat$weight <- 1
# } 

#####------------------------------------------------------#
##### Helpers ####
#####------------------------------------------------------#

weighted.sd <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    sqrt((sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2)))
}

#####------------------------------------------------------#
##### Heterogeneity by specific clip ####
#####------------------------------------------------------#

script_lbls <- c("in-party\nincivility", "out-party\nincivility", "past\ncontroversy", "novel\ncontroversy", "political\ninsincerity")

## categorical belief
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control) %>%
    group_by(script) %>% 
    summarise(believed_true_mean = mean(believed_true, na.rm=T),
              believed_true_sd = sd(believed_true, na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=script, y=believed_true_mean, 
               ymax=believed_true_mean+1.96*believed_true_sd, 
               ymin=believed_true_mean-1.96*believed_true_sd)) +
    geom_bar(stat="identity") +
    ylab("level of deception") + xlab("script condition") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +
    scale_x_discrete(labels=script_lbls) +
    theme_bw() + 
        theme(title = element_text(size=5),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript.pdf", width=6, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript.pdf")

## binary belief
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control==T) %>%
    group_by(script) %>% 
    summarise(believed_true_mean = mean(believed1_true, na.rm=T)) %>%
    ggplot(aes(x=script, y=believed_true_mean)) +
    geom_bar(stat="identity") +
    ylab("% believe clipping is real") + xlab("script condition") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(labels=script_lbls) +
    theme_bw() + 
        theme(title = element_text(size=5),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript_binary.pdf", width=6, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_binary.pdf")

## belief by condition
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control) %>%
    group_by(script, treat) %>% 
    summarise(believed_true_mean = mean(believed_true, na.rm=T),
              believed_true_sd = sd(believed_true, na.rm=T)/sqrt(n())) %>%
    ggplot(aes(x=script, y=believed_true_mean, 
               ymax=believed_true_mean+1.96*believed_true_sd,
               ymin=believed_true_mean-1.96*believed_true_sd,
               fill=treat, label=treat)) +
    geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
    ylab("level of deception") + xlab("script condition") +
    geom_errorbar(position=position_dodge(.9), width=.2, size=1) +    
    scale_x_discrete(labels=script_lbls) +
    geom_text(aes(y=believed_true_mean+0.4), position=position_dodge(width=1.05), size=3) +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript_bycond.pdf", width=6.5, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_bycond.pdf")

## binary belief by condition
dat %>%
    filter(!is.na(script)) %>%
    filter(exp_1_prompt_control) %>%
    group_by(script, treat) %>% 
    summarise(believed_true_mean = mean(believed1_true, na.rm=T)) %>%
    ggplot(aes(x=script, y=believed_true_mean, fill=treat, label=treat)) +
    geom_bar(stat="identity", position=position_dodge(width=.9), color="black") +
    ylab("% believe clipping is real") + xlab("script condition") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(labels=script_lbls) +
    geom_text(aes(y=believed_true_mean+0.04), position=position_dodge(width=1.05), size=3) +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/firststage_belief_byscript_binary_bycond.pdf", width=6.5, height=3.05)
if(SHOW_PDFS) system("open figures_exploratory/firststage_belief_byscript_binary_bycond.pdf")


#####------------------------------------------------------#
##### Second-stage clip performance ####
#####------------------------------------------------------#

# ## code NAs as 0
# scores_na <- dfsurvdat[,c(nofake_vids, lowfake_vids, hifake_vids)] %>%
#     gather(key="video", value="response") %>%
#     mutate(is_real=grepl("real", video)) %>%
#     mutate(video=gsub("_\\d$", "", video)) %>%
#     mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
#                           ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
#     mutate(correct=replace_na(correct, 0)) %>%
#     group_by(video) %>%
#     summarise(pct_correct=mean(correct, na.rm=T)) %>% 
#     as.data.frame()

scores <- dfsurvdat[,c(nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response") %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("_\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    mutate(is_logo=case_when(video == "fake_hilary2" ~ TRUE,
                             video == "fake_obama_buzzfeed" ~ FALSE,
                             video == "real_obama_missile" ~ TRUE,
                             video == "fake_bernie1" ~ TRUE,
                             video == "real_obama_smoking" ~ FALSE,
                             video == "real_warrenbeer" ~ TRUE,
                             video == "real_trump_soup" ~ TRUE,
                             video == "real_trump_apple" ~ TRUE,
                             video == "real_biden_fight" ~ FALSE,
                             video == "fake_boris" ~ FALSE,
                             video == "real_warrenliar" ~ TRUE,
                             video == "real_biden_stumble" ~ TRUE,
                             video == "real_trump_covid" ~ TRUE,
                             video == "fake_trump_aids" ~ TRUE,
                             video == "fake_trump_resign" ~ TRUE)) %>%
    as.data.frame()

## overall
scores %>%
    arrange(-pct_correct) %>%
    mutate(video=as_factor(video)) %>%
    mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    xlab("video clip in detection task") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ggsave("figures_exploratory/secondstage_byclip.pdf", width=6.5, height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip.pdf")

## overall by clip
p_scores_fake <- scores %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="fake clips") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[grepl("fake_", scores$video)]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real <- scores %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="real clips") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[grepl("real_", scores$video)]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
pp <- cowplot::plot_grid(p_scores_fake, p_scores_real, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byfake.pdf", plot=pp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byfake.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by partisanship ####
#####------------------------------------------------------#

dfsurvdat$PID_main <- as.character(dfsurvdat$PID_main)
dfsurvdat$PID_leaners <- as.character(dfsurvdat$PID_leaners)
dfsurvdat$PID[dfsurvdat$PID_main=="Democrat"|dfsurvdat$PID_leaners=="Democrat"] <- "Democrat"
dfsurvdat$PID[dfsurvdat$PID_main=="Republican"|dfsurvdat$PID_leaners=="Republican"] <- "Republican"
dfsurvdat$PID[dfsurvdat$PID_main=="Independent"&!(dfsurvdat$PID_leaners %in% c("Democrat","Republican"))] <- "Independent"
dfsurvdat$PID[is.na(dfsurvdat$PID)] <- "N/A"
dfsurvdat$PID <- factor(dfsurvdat$PID, levels=c("N/A","Democrat","Independent","Republican"))

scores_byPID <- dfsurvdat[,c("PID", nofake_vids, lowfake_vids, hifake_vids)] %>%
    gather(key="video", value="response", -PID) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("_\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(PID, video) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

p_scores_fake2 <- scores_byPID %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="fake clips") %>%
    arrange(desc(PID), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    mutate(PID=case_when(PID == "Democrat" ~ "D",
                         PID == "Republican" ~ "R",
                         PID == "Independent" ~ "I")) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=PID, label=PID)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.02), position = position_dodge(width = 0.8), size=2.5) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_manual(values=c("blue","grey","red")) +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real2 <- scores_byPID %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="real clips") %>%
    mutate(PID=case_when(PID == "Democrat" ~ "D",
                         PID == "Republican" ~ "R",
                         PID == "Independent" ~ "I")) %>%
    arrange(desc(PID), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=PID, label=PID)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.02), position = position_dodge(width = 0.8), size=2.5) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_manual(values=c("blue","grey","red")) +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp2 <- cowplot::plot_grid(p_scores_fake2, p_scores_real2, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byPID.pdf", plot=pp2, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byPID.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by knowledge ####
#####------------------------------------------------------#

dfsurvdat$polknow_speaker   <- as.numeric(dfsurvdat$polknow_speaker == "Nancy Pelosi")
dfsurvdat$polknow_medicare  <- as.numeric(dfsurvdat$polknow_medicare == "A program run by the US federal government to pay for old people’s health care")
dfsurvdat$polknow_house     <- as.numeric(dfsurvdat$polknow_house == "Democrats")
dfsurvdat$polknow_senate    <- as.numeric(dfsurvdat$polknow_senate == "Republicans")
dfsurvdat$polknow_veto      <- as.numeric(dfsurvdat$polknow_veto == "Two-thirds")
dfsurvdat$polknow_warren    <- as.numeric(dfsurvdat$polknow_warren == "Elizabeth Warren")
dfsurvdat$polknow_boris     <- as.numeric(dfsurvdat$polknow_boris == "Boris Johnson")
dfsurvdat$polknow           <- rowMeans(dfsurvdat[grepl("polknow_", colnames(dfsurvdat))], na.rm=T)

scores_byknow <- dfsurvdat[,c("polknow",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(polknow=cut(polknow, breaks=c(-1, 0.5, 1.1), 
                       labels=c("Less pol. know.", "More pol. know."))) %>%
    gather(key="video", value="response", -polknow) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("_\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,polknow) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

p_scores_fake3 <- scores_byknow %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="fake clips") %>%
    arrange(desc(polknow), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=polknow, label=polknow)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.2), position = position_dodge(width = 0.8), size=2.5) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.15)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlGnBu") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real3 <- scores_byknow %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="real clips") %>%
    arrange(desc(polknow), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=polknow, label=polknow)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.2), position = position_dodge(width = 0.8), size=2.5) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlGnBu") +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp2 <- cowplot::plot_grid(p_scores_fake3, p_scores_real3, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bypolknow.pdf", plot=pp2, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bypolknow.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by digital literacy ####
#####------------------------------------------------------#

dfsurvdat$post_dig_lit_1 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_1))
dfsurvdat$post_dig_lit_2 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_2))
dfsurvdat$post_dig_lit_3 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_3))
dfsurvdat$post_dig_lit_4 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_4))
dfsurvdat$post_dig_lit_5 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_5))
dfsurvdat$post_dig_lit_6 <- as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_6))
dfsurvdat$post_dig_lit_7 <- 6-as.numeric(gsub("[^1-5]", "", dfsurvdat$post_dig_lit_7))
X <- cbind(
      dfsurvdat$post_dig_lit_1, dfsurvdat$post_dig_lit_2, 
      dfsurvdat$post_dig_lit_3, dfsurvdat$post_dig_lit_4, 
      dfsurvdat$post_dig_lit_5, dfsurvdat$post_dig_lit_6, 
      dfsurvdat$post_dig_lit_7
)
dfsurvdat$post_dig_lit <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, sum(r,na.rm=T)))
dfsurvdat$post_dig_lit <- dfsurvdat$post_dig_lit/35
 
diglit_quantile <- quantile(dfsurvdat$post_dig_lit, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
diglit_quantile[1] <- -Inf
diglit_quantile[4] <- Inf


scores_bydiglit <- dfsurvdat[,c("post_dig_lit",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(post_dig_lit=cut(post_dig_lit, breaks=diglit_quantile, 
                            labels=c("Low dig. lit.", "Moderate dig. lit.","High dig. lit."))) %>%
    gather(key="video", value="response", -post_dig_lit) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("_\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,post_dig_lit) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

p_scores_fake4 <- scores_bydiglit %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="fake clips") %>%
    arrange(desc(post_dig_lit), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=post_dig_lit, label=post_dig_lit)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.07), position = position_dodge(width = 0.8), size=2.5, hjust=0) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlOrRd") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real4 <- scores_bydiglit %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="real clips") %>%
    arrange(desc(post_dig_lit), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=post_dig_lit, label=post_dig_lit)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.05), position = position_dodge(width = 0.8), size=2.5, hjust=0) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="YlOrRd") +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp4 <- cowplot::plot_grid(p_scores_fake4, p_scores_real4, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bydiglit.pdf", plot=pp4, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bydiglit.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by cognitive reflection ####
#####------------------------------------------------------#

dfsurvdat$crt1 <- suppressWarnings(as.numeric(gsub("(\\$| |\\¢|[a-zA-Z])", "", dfsurvdat$pre_crt_1)) %in% c(5, 0.05))
dfsurvdat$crt2 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_2)) %in% c(100))
dfsurvdat$crt3 <- suppressWarnings(as.numeric(gsub("[a-zA-Z]", "", dfsurvdat$pre_crt_3)) %in% c(47))
dfsurvdat$crt  <- dfsurvdat$crt1 + dfsurvdat$crt2 + dfsurvdat$crt3
X <- cbind(dfsurvdat$crt1, dfsurvdat$crt2, dfsurvdat$crt3)

dfsurvdat$crt <- apply(X, 1, function(r) ifelse(all(is.na(r)), NA, mean(r,na.rm=T)))

scores_byCR <- dfsurvdat[,c("crt",nofake_vids, lowfake_vids, hifake_vids)] %>%
    mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low CR", "Moderate CR", "High CR"))) %>%
    gather(key="video", value="response", -crt) %>%
    filter(!is.na(as.character(response))) %>%
    mutate(is_real=grepl("real", video)) %>%
    mutate(video=gsub("_\\d$", "", video)) %>%
    mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
    mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                          ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
    mutate(correct=replace_na(correct, 0)) %>%
    group_by(video,crt) %>%
    summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
    mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                               video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                               video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                               video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                               video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                               video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                               video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                               video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                               video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                               video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                               video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                               video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                               video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                               video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                               video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
    as.data.frame()

p_scores_fake5 <- scores_byCR %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
    mutate(is_fake="fake clips") %>%
    arrange(desc(crt), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=crt, label=crt)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.07), position = position_dodge(width = 0.8), size=2.5, hjust=0) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="PuRd") +
    coord_flip() +
    facet_grid(is_fake ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

p_scores_real5 <- scores_byCR %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
    mutate(is_real="real clips") %>%
    arrange(desc(crt), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct, fill=crt, label=crt)) + 
    geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
    geom_text(aes(y=pct_correct+0.05), position = position_dodge(width = 0.8), size=2.5, hjust=0) + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0, 1.1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    scale_fill_brewer(palette="PuRd") +
    coord_flip() +
    facet_grid(is_real ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp5 <- cowplot::plot_grid(p_scores_fake5, p_scores_real5, nrow=2, rel_heights = c(1,1.4), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_byCR.pdf", plot=pp5, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_byCR.pdf")

#####------------------------------------------------------#
##### Second-stage clip performance by source cue ####
#####------------------------------------------------------#

pp_scores_logo <- scores %>% 
    filter(is_logo) %>% 
    mutate(is_logo="source logo") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[scores$is_logo]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_logo ~ .) +
    xlab("") + ylab("") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))

pp_scores_nologo <- scores %>% 
    filter(!is_logo) %>% 
    mutate(is_logo="no logo") %>%
    arrange(-pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
    ggplot(aes(x=video_lbl, y=pct_correct)) + 
    geom_bar(stat="identity") +
    geom_hline(yintercept=mean(scores$pct_correct[!scores$is_logo]), size=1, lty=1, color="red") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1)) + 
    scale_x_discrete(expand=c(-0.1, 0)) +
    coord_flip() +
    facet_grid(is_logo ~ .) +
    xlab("") + ylab("% of correct detections") +
    theme_bw() + 
        theme(title = element_text(size=5),
              legend.position = "none",
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12),
              strip.text = element_text(size=16),
              axis.title.x = element_text(size=14),
              axis.title.y = element_text(size=14))
ppp <- cowplot::plot_grid(pp_scores_logo, pp_scores_nologo, nrow=2, rel_heights = c(1.5,0.62), align="v")

cowplot::save_plot("figures_exploratory/secondstage_byclip_bylogo.pdf", plot=ppp, base_width=6.5, base_height=7)
if(SHOW_PDFS) system("open figures_exploratory/secondstage_byclip_bylogo.pdf")

#####------------------------------------------------------#
##### Correlations with distraction clip attitudes? ####
#####------------------------------------------------------#

#TODO

#####------------------------------------------------------#
##### Correlations b/t 'believe-fake' and 'did-happen'? ####
#####------------------------------------------------------#

## === Trump resigns ===
summary(lm(dfsurvdat$fake_vs_didnt_happen_1 ~ I(dfsurvdat$fake_trump_resign=="This video is not fake or doctored")))
# Coefficients:
#                                                                            Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  9.9206     0.3136   31.64   <2e-16 ***
# I(dfsurvdat$fake_trump_resign == "This video is not fake or doctored")TRUE  21.6948     2.0343   10.66   <2e-16 ***
summary(lm(dfsurvdat$fake_vs_didnt_happen_1 ~ I(dfsurvdat$fake_trump_resign=="This video is fake or doctored")))
# Coefficients:
#                                                                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                             12.5349     0.3685   34.02   <2e-16 ***
# I(dfsurvdat$fake_trump_resign == "This video is fake or doctored")TRUE  -7.1720     0.6812  -10.53   <2e-16 ***

dfsurvdat %>% 
  filter(!is.na(factor(fake_trump_resign))) %>%
  mutate(fake_trump_resign = case_when(fake_trump_resign == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                       fake_trump_resign == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                       fake_trump_resign == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_1)) + 
  xlab("confidence that event took place") +
  facet_grid(~ fake_trump_resign) +
  geom_density(color="black", fill="#53B400") + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpresign.pdf", width=9, height=3.25)

dfsurvdat %>% 
  filter(!is.na(factor(fake_trump_resign))) %>%
  mutate(fake_trump_resign = case_when(fake_trump_resign == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                       fake_trump_resign == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                       fake_trump_resign == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_1, fill=PID)) + 
  xlab("confidence that event took place") +
  facet_grid(~ fake_trump_resign) +
  scale_fill_manual(values=c("blue","grey","red"), name="Party identification:") +
  # geom_histogram(color="black", alpha=0.8) + 
  geom_density(color="black", alpha=0.5) + 
  # scale_y_continuous(trans="log2", expand=c(0,0)) +
  # scale_y_continuous(breaks=c(0.0, 0.05, 0.1, 0.2, 0.9)) +
  scale_x_continuous(limits=c(1, 100)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpresign_byPID.pdf", width=9, height=4.25)


## === Trump COVID precautions ===
dfsurvdat$real_trump_covid <- ifelse(is.na(factor(dfsurvdat$real_trump_covid)), 
                                     as.character(dfsurvdat$real_trump_covid_1),
                                     as.character(dfsurvdat$real_trump_covid))

summary(lm(dfsurvdat$fake_vs_didnt_happen_2 ~ I(dfsurvdat$real_trump_covid=="This video is not fake or doctored")))
# Coefficients:
#                                                                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                39.9948     0.9112   43.89   <2e-16 ***
# I(dfsurvdat$real_trump_covid == "This video is not fake or doctored")TRUE  35.3524     1.1016   32.09   <2e-16 ***
summary(lm(dfsurvdat$fake_vs_didnt_happen_2 ~ I(dfsurvdat$real_trump_covid=="This video is fake or doctored")))
# Coefficients:
#                                                                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                            70.9244     0.5837  121.51   <2e-16 ***
# I(dfsurvdat$real_trump_covid == "This video is fake or doctored")TRUE -37.4844     1.3761  -27.24   <2e-16 ***

dfsurvdat %>% 
  filter(!is.na(factor(real_trump_covid))) %>%
  mutate(real_trump_covid = case_when(real_trump_covid == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                      real_trump_covid == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                      real_trump_covid == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_2)) + 
  xlab("confidence that event took place") +
  facet_grid(~ real_trump_covid) +
  # scale_y_continuous(trans="log2", expand=c(0,0)) +
  geom_density(color="black", fill="#53B400") + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpcovid.pdf", width=9, height=3.25)


dfsurvdat %>% ## by partisanship
  filter(!is.na(factor(real_trump_covid))) %>%
  mutate(real_trump_covid = case_when(real_trump_covid == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                      real_trump_covid == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                      real_trump_covid == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_2, fill=PID)) + 
  xlab("confidence that event took place") +
  facet_grid(~ real_trump_covid) +
  scale_fill_manual(values=c("blue","grey","red"), name="Party identification:") +
  geom_density(color="black", alpha=0.5) + 
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_trumpcovid_byPID.pdf", width=9, height=4.25)

## === Obama Russian president hot mic ===
dfsurvdat$real_obama_missile <- ifelse(is.na(factor(dfsurvdat$real_obama_missile)), 
                                       as.character(dfsurvdat$real_obama_missile_1),
                                       as.character(dfsurvdat$real_obama_missile))

summary(lm(dfsurvdat$fake_vs_didnt_happen_3 ~ I(dfsurvdat$real_obama_missile=="This video is not fake or doctored")))
# Coefficients:
#                                                                             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                                  46.0525     0.5151   89.40   <2e-16 ***
# I(dfsurvdat$real_obama_missile == "This video is not fake or doctored")TRUE  36.0184     1.5463   23.29   <2e-16 ***

summary(lm(dfsurvdat$fake_vs_didnt_happen_3 ~ I(dfsurvdat$real_obama_missile=="This video is fake or doctored")))
# Coefficients:
#                                                                         Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                                              51.1225     0.5488  93.147  < 2e-16 ***
# I(dfsurvdat$real_obama_missile == "This video is fake or doctored")TRUE  -7.4933     1.4504  -5.167 2.47e-07 ***

dfsurvdat %>% 
  filter(!is.na(factor(real_obama_missile))) %>%
  mutate(real_obama_missile = case_when(real_obama_missile == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                      real_obama_missile == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                      real_obama_missile == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_3)) + 
  xlab("confidence that event took place") +
  facet_grid(~ real_obama_missile) +
  geom_density(color="black", fill="#53B400") + 
  theme_bw() +
  theme(
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_obamamissile.pdf", width=9, height=3.25)

dfsurvdat %>% ## by partisanship
  filter(!is.na(factor(real_obama_missile))) %>%
  mutate(real_obama_missile = case_when(real_obama_missile == "This video is fake or doctored" ~ "Believed that\nvideo is fake",
                                      real_obama_missile == "This video is not fake or doctored" ~ "Believed that\nvideo is real",
                                      real_obama_missile == "I don't know" ~ "Unsure")) %>%
  ggplot(aes(x=fake_vs_didnt_happen_3, fill=PID)) + 
  xlab("confidence that event took place") +
  facet_grid( ~ real_obama_missile) +
  geom_density(color="black", alpha=0.5) + 
  scale_fill_manual(values=c("blue","grey","red"), name="Party identification:") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size=14),
    axis.text.x = element_text(size=14),
    strip.text = element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18)
  )
ggsave("figures_exploratory/secondstage_fake-vs-happen_obamamissile_byPID.pdf", width=9, height=4.25)


#####------------------------------------------------------#
##### Experiment 1: Non-parametric tests ####
#####------------------------------------------------------#

## deception
t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_audio == 1])) ##t = -1.9296, df = 1767.1, p-value = 0.05382, \delta = -0.119805

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_text == 1])) ##t = -1.2151, df = 1761.7, p-value = 0.2245, \delta = -0.075769

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_skit == 1])) ##t = 8.4754, df = 1083.9, p-value < 2.2e-16, \delta = 0.653852

## affect
t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_audio == 1])) ##t = -1.647, df = 1802.3, p-value = 0.09974, \delta = -2.64796

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_text == 1])) ##t = -1.8447, df = 1794.4, p-value = 0.06525, \delta = -2.94543

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_skit == 1])) ##t = -1.0868, df = 1793.6, p-value = 0.2773, \delta = -1.72203

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_attackad == 1])) ##t = -0.13861, df = 1787.2, p-value = 0.8898, \delta = -0.22528

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_control == 1])) ##t = -2.793, df = 1767.1, p-value = 0.005278, \delta = -4.53598

## info
t.test(dat$believed_true[dat$exp_1_prompt_info],
       dat$believed_true[dat$exp_1_prompt_control]) ##t = -6.8445, df = 4177.9, p-value = 8.79e-12, \delta = -0.284111

t.test(dat$believed_true[dat$exp_1_prompt_info & dat$treat_fake_video],
       dat$believed_true[dat$exp_1_prompt_control & dat$treat_fake_video]) ##t = -3.918, df = 853.94, p-value = 9.641e-05, \delta = -0.354745

## heterogeneity by age
t.test(dat$post_favor_Warren[dat$age_65 == ">65"],
       dat$post_favor_Warren[dat$age_65 == "<=65"]) ##t = -5.7333, df = 4698, p-value = 1.047e-08, \delta = -5.42

## heterogeneity by medium within age
t.test(dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_text==1]) ##t = -1.4803, df = 771.99, p-value = 0.1392, \delta = -3.81

## heterogeneity by sexism
t.test(dat$believed_true[dat$ambivalent_sexism <= 2.33],
       dat$believed_true[dat$ambivalent_sexism > 3.66]) ##t = -7.9801, df = 1250.3, p-value = 3.284e-15, \delta = -0.558898

t.test(dat$post_favor_Warren[dat$ambivalent_sexism <= 2.33],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66]) ##t = 17.772, df = 1525.8, p-value < 2.2e-16, \delta = -25.9583

## heterogeneity by medium within sexism
t.test(dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = 1.122, df = 276.71, p-value = 0.2628, \delta = 0.180147

t.test(dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = -2.0674, df = 277.91, p-value = 0.03962, \delta = -8.41421

## heterogeneity by PID
t.test(dat$believed_true[dat$PID=="Democrat"],
       dat$believed_true[dat$PID=="Republican"]) ##t = -13.771, df = 3621.7, p-value < 2.2e-16, \delta = -0.60564

t.test(dat$post_favor_Warren[dat$PID=="Democrat"],
       dat$post_favor_Warren[dat$PID=="Republican"]) ##t = 53.531, df = 4753, p-value < 2.2e-16, \delta = -42.58104

## heterogeneity by polknow
t.test(dat$believed_true[dat$polknow <= 0.5],
       dat$believed_true[dat$polknow > 0.5]) ##t = 0.10765, df = 1115.6, p-value = 0.9143, \delta = 0.005508

t.test(dat$post_favor_Warren[dat$polknow <= 0.5],
       dat$post_favor_Warren[dat$polknow > 0.5]) ##t = -3.6516, df = 1600.4, p-value = 0.000269, \delta = 3.85

#####------------------------------------------------------#
##### Experiment 2: Non-parametric tests ####
#####------------------------------------------------------#

## real vs. fake clips
t.test(scores$pct_correct[grepl("real_",scores$video)]*100, 
       scores$pct_correct[grepl("fake_",scores$video)]*100) ##t = 0.5702, df = 5.8213, p-value = 0.5899, \delta = 0.0720895

## logo vs. no logo clips
t.test(scores$pct_correct[scores$is_logo], 
       scores$pct_correct[!scores$is_logo]) ##t = 0.53689, df = 5.9601, p-value = 0.6108, \delta = 0.0603977

## obama missile
chisq.test(
table(as.character(dfsurvdat$real_obama_missile[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]))
) ##X-squared = 333.34, df = 1, p-value < 2.2e-16

## trump apple
chisq.test(
table(as.character(dfsurvdat$real_trump_apple[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]))
) ##X-squared = 75.155, df = 1, p-value < 2.2e-16

## trump covid
chisq.test(
table(as.character(dfsurvdat$real_trump_covid[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]),
      as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]))
) ##X-squared = 169.96, df = 1, p-value < 2.2e-16

