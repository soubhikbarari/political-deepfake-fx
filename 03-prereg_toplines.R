# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Create some figures that summarise analyses in step 3 (for
# Science submission).
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1min
# 
# Input:
# - code/deepfake.Rdata
# - code/03-deepfake_prereg_analysis.R
#
# Output:
# - figures/topline*.pdf
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls())
setwd("~/Research_Group Dropbox/Soubhik Barari/Projects/repos/deepfakes_project")
load("code/deepfake.RData")
# source("code/03-deepfake_prereg_analysis.R")

library(optparse)
library(cowplot)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)


# #####------------------------------------------------------#
# ##### Experiment 1: Stacked coefficient plots ####
# #####------------------------------------------------------#
# 
# ### diff-in-means
# dat %>% filter(treat != "ad") %>%
#     mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#     do(tidy(lm(believed_true ~ treat, data = .))) %>% 
#     filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
#     ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black") +
#     geom_text(aes(label=term, y=estimate+1.96*std.error), nudge_y=0.025, size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           axis.ticks.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))
# 
# ### info vs. no info
# dat %>% group_by(exp_1_prompt) %>% filter(treat != "ad") %>%
#     mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#     do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#     filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
#     ggplot(aes(x=exp_1_prompt, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black", position=position_dodge(0.5)) +
#     geom_text(aes(group=term, label=term, y=estimate+1.96*std.error+0.1), position=position_dodge(0.5), size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           # axis.ticks.y = element_blank(),
#           axis.text.y = element_text(size=14),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))
# 
# bind_rows(
#     ## by party
#     dat %>% filter(!is.na(PID)) %>% group_by(PID) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=PID),
#     ## by age
#     dat %>% filter(!is.na(age_65)) %>% group_by(age_65) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=age_65),
#     ## by CRT
#     dat %>% filter(!is.na(crt)) %>% 
#         mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
#                    labels=c("low cognitive\nreflection", "moderate cognitive\nreflection", "high cognitive\nreflection"))) %>%
#         group_by(crt) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=crt),
#     ## by sexism
#     dat %>% mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
#                                          labels=c("low ambivalent\nsexism", "moderate ambivalent\nsexism", "high ambivalent\nsexism"))) %>%
#         filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>% 
#         group_by(ambivalent_sexism) %>% filter(treat != "ad", treat !=" control", !is.na(treat)) %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=ambivalent_sexism)
# ) %>% 
#     arrange(desc(row_number())) %>%
#     mutate(group=as_factor(group)) %>%
#     ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black", position=position_dodge(0.5)) +
#     geom_text(aes(group=term, label=term, y=estimate+1.96*std.error+0.1), position=position_dodge(0.5), size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     # scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           # axis.ticks.y = element_blank(),
#           axis.text.y = element_text(size=14),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))

#####------------------------------------------------------#
##### Experiment 1: Stacked means -- deception ####
#####------------------------------------------------------#

colorscheme <- scale_fill_manual(values=c("#C77CFF","#00BFC4","#7CAE00","#F8766D"))

### diff-in-means
panel1 <- dat %>% filter(treat != "ad", treat != "control") %>%
    mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
    group_by(treat) %>% summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
    mutate(panel="All\n") %>%
    ggplot(aes(x=treat, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_segment(x=0, xend=4.5, 
                 y=mean(dat$believed_true[!(dat$treat %in% c("ad","control","skit"))], na.rm=T),
                 yend=mean(dat$believed_true[!(dat$treat %in% c("ad","control","skit"))], na.rm=T), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    scale_shape_manual(values=c(21,22,23,24)) +
    facet_grid(panel ~ .) + 
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("") + xlab("") + theme_linedraw() + coord_flip(ylim=c(1,5)) +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14, hjust=0))

### info vs. no info
panel2 <- dat %>% filter(treat != "ad", treat != "control") %>%
    group_by(exp_1_prompt,treat) %>%
    mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
    summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
    mutate(panel="By\nIntervention") %>%
    ggplot(aes(x=exp_1_prompt, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    # geom_bar(aes(fill=treat), position=position_dodge(width=.9), color="black", stat="identity") +
    # geom_errorbar(aes(group=treat), position=position_dodge(width=.9), width=.2, size=1) +
    geom_segment(data = dat %>% 
                     filter(treat != "skit") %>% 
                     mutate(group_x = ifelse(exp_1_prompt == "control", 1, 2)) %>%
                     group_by(exp_1_prompt,group_x) %>% 
                     summarise(yint=mean(believed_true, na.rm=T)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    scale_shape_manual(values=c(21,22,23,24)) +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    scale_x_discrete(labels=c("Received\nno information","Received\ninformation"), expand=c(0.2,0.2)) + colorscheme +
    facet_grid(panel ~ .) + 
    ylab("") + xlab("") + coord_flip(ylim=c(1,5)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=14),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size=16),
          axis.title.x = element_text(size=18))

dd <- bind_rows(
    ## by party
    dat %>% filter(!is.na(PID)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(PID,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=PID) %>% mutate(type="By Partisan\nIdentity"),
    ## by age
    dat %>% filter(!is.na(age_65)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(age_65 = ifelse(age_65 == ">65", "65 or older", "64 or younger")) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(age_65,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=age_65) %>% mutate(type="By Age\nGroup"),
    ## by CRT
    dat %>% filter(!is.na(crt)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low reflection", "Moderate reflection", "High reflection"))) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(crt,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=crt) %>% mutate(type="By Cognitive\nReflection"),
    ## by political knowledge
    dat %>% filter(!is.na(polknow)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(polknow=cut(polknow, breaks=c(-1, 0.5, 1.1), 
                   labels=c("Less knowledge", "More knowledge"))) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(polknow,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=polknow) %>% mutate(type="By Pol.\nKnowledge"),
    ## by sexism
    dat %>% filter(!is.na(ambivalent_sexism)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                         labels=c("Low sexism", "Moderate sexism", "High sexism"))) %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>% 
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(ambivalent_sexism,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=ambivalent_sexism) %>% mutate(type="By Ambivalent\nSexism")
)
dd$group <- as_factor(dd$group)
    
panel3 <- dd %>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    # geom_bar(aes(fill=treat), position=position_dodge(width=.9), color="black", stat="identity") +
    # geom_errorbar(aes(group=treat), position=position_dodge(width=.9), width=.2, size=1) +
    scale_shape_manual(values=c(21,22,23,24)) +
    geom_segment(data = dd %>% 
                     filter(treat != "skit") %>% 
                     group_by(type, treat) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(type,group,group_x) %>% 
                     summarise(yint=mean(estimate)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    facet_grid(type ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("mean level of deception in group (95% CI)") + xlab("") + coord_flip(ylim=c(1,5)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=15),
          axis.text.y = element_text(size=14),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=18))

pp <- cowplot::plot_grid(panel1, NULL, panel2, NULL, panel3, 
                         nrow=5, align="v", rel_heights = c(0.7,-.2,1.2,-.25,5.5))
cowplot::save_plot("figures/topline_exp1_a.pdf", plot = pp, base_height=10.5, base_width=8)
system("open figures/topline_exp1_a.pdf")

#####------------------------------------------------------#
##### Experiment 1: Stacked means -- affect ####
#####------------------------------------------------------#

colorscheme <- scale_fill_manual(values=c("black","white","#C77CFF","#00BFC4","#7CAE00","#F8766D"))

### diff-in-means
panel1.b <- dat %>% filter(!is.na(treat)) %>%
    mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
    group_by(treat) %>% summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
    mutate(panel="All\n") %>%
    ggplot(aes(x=treat, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_segment(x=0, xend=6.5, 
                 y=mean(dat$post_favor_Warren[!(dat$treat %in% c("ad","control","skit"))], na.rm=T),
                 yend=mean(dat$post_favor_Warren[!(dat$treat %in% c("ad","control","skit"))], na.rm=T), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=.5, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+2.7), position=position_dodge(width=.9), size=3) +
    scale_shape_manual(values=c(8,25,21,22,23,24)) +
    facet_grid(panel ~ .) + 
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("") + xlab("") + theme_linedraw() + coord_flip(ylim=c(15,75)) +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14, hjust=0))

### info vs. no info
panel2.b <- dat %>% filter(!is.na(treat)) %>%
    group_by(exp_1_prompt,treat) %>%
    mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
    summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
    mutate(panel="By\nIntervention") %>%
    ggplot(aes(x=exp_1_prompt, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    # geom_bar(aes(fill=treat), position=position_dodge(width=.9), color="black", stat="identity") +
    # geom_errorbar(aes(group=treat), position=position_dodge(width=.9), width=.2, size=1) +
    geom_segment(data = dat %>% 
                     filter(treat != "skit") %>% 
                     mutate(group_x = ifelse(exp_1_prompt == "control", 1, 2)) %>%
                     group_by(exp_1_prompt,group_x) %>% 
                     summarise(yint=mean(post_favor_Warren, na.rm=T)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=.5, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+2.5), position=position_dodge(width=.9), size=3) +
    scale_shape_manual(values=c(8,25,21,22,23,24)) +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    scale_x_discrete(labels=c("Received\nno information","Received\ninformation"), expand=c(0.2,0.2)) + colorscheme +
    facet_grid(panel ~ .) + 
    ylab("") + xlab("") + coord_flip(ylim=c(15,75)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size=14),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size=16),
          axis.title.x = element_text(size=18))

dd <- bind_rows(
    ## by party
    dat %>% filter(!is.na(PID)) %>% filter(!is.na(treat)) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(PID,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=PID) %>% mutate(type="By Partisan\nIdentity"),
    ## by age
    dat %>% filter(!is.na(age_65)) %>% filter(!is.na(treat)) %>%
        mutate(age_65 = ifelse(age_65 == ">65", "65 or older", "64 or younger")) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(age_65,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=age_65) %>% mutate(type="By Age\nGroup"),
    ## by CRT
    dat %>% filter(!is.na(crt)) %>% filter(!is.na(treat)) %>%
        mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low reflection", "Moderate reflection", "High reflection"))) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(crt,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=crt) %>% mutate(type="By Cognitive\nReflection"),
    ## by political knowledge
    dat %>% filter(!is.na(polknow)) %>% filter(!is.na(treat)) %>%
        mutate(polknow=cut(polknow, breaks=c(-1, 0.5, 1.1), 
                   labels=c("Less knowledge", "More knowledge"))) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(polknow,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=polknow) %>% mutate(type="By Pol.\nKnowledge"),
    ## by sexism
    dat %>% filter(!is.na(ambivalent_sexism)) %>% filter(!is.na(treat)) %>%
        mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
                                         labels=c("Low sexism", "Moderate sexism", "High sexism"))) %>%
        filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>% 
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(ambivalent_sexism,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=ambivalent_sexism) %>% mutate(type="By Ambivalent\nSexism")
)
dd$group <- as_factor(dd$group)
    
panel3.b <- dd %>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    # geom_bar(aes(fill=treat), position=position_dodge(width=.9), color="black", stat="identity") +
    # geom_errorbar(aes(group=treat), position=position_dodge(width=.9), width=.2, size=1) +
    scale_shape_manual(values=c(8,25,21,22,23,24)) +
    geom_segment(data = dd %>% 
                     filter(treat != "skit") %>% 
                     group_by(type, treat) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(type,group,group_x) %>% 
                     summarise(yint=mean(estimate)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=.5, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+3), position=position_dodge(width=.9), size=3) +
    facet_grid(type ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("mean feeling thermometer\ntowards Elizabeth Warren (95% CI)") + xlab("") + coord_flip(ylim=c(15,75)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=15),
          axis.text.y = element_text(size=14),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=18))

pp.b <- cowplot::plot_grid(panel1.b, NULL, panel2.b, NULL, panel3.b, 
                         nrow=5, align="v", rel_heights = c(0.7,-.2,1.2,-.25,5.5))
cowplot::save_plot("figures/topline_exp1_b.pdf", plot = pp.b, base_height=10.5, base_width=8)
system("open figures/topline_exp1_b.pdf")

#####------------------------------------------------------#
##### Experiment 1: Stacked means -- both ####
#####------------------------------------------------------#

pp.A <- cowplot::plot_grid(panel1 + ggtitle("Deception") + theme(strip.background = element_blank(), plot.title = element_text(size=18, hjust=0.5)), NULL, 
                           panel2 + theme(strip.background = element_blank()), NULL, 
                           panel3 + theme(strip.background = element_blank(), axis.title.x=element_text(size=15)) +ylab("mean level of deception\nin group (95% CI)"), 
                           nrow=5, align="v", rel_heights = c(0.7,-.18,1.2,-.22,5.5))

pp.B <- cowplot::plot_grid(panel1.b + ggtitle("Affect") + theme(axis.ticks.x=element_blank(), plot.title = element_text(size=18, hjust=0.5)), NULL, 
                           panel2.b + theme(axis.text.y=element_blank()), NULL, 
                           panel3.b + theme(axis.text.y=element_blank(), axis.title.x=element_text(size=15)),
                           nrow=5, align="v", rel_heights = c(0.7,-.18,1.2,-.22,5.5))
pp.A_B <- cowplot::plot_grid(pp.A, NULL, pp.B, ncol=3, align="h", rel_widths = c(1.5,-0.1,1))
cowplot::save_plot("figures/topline_exp1.pdf", plot = pp.A_B, base_height=12, base_width=12)
system("open figures/topline_exp1.pdf")

#####------------------------------------------------------#
##### Experiment 2: Stacked means ####
#####------------------------------------------------------#

d <- dat %>%
    gather(key="metric", value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "Accuracy")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "False Positive Rate")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "False Negative Rate")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake"))
 
polknow_quantile <- quantile(d$polknow, probs=c(0,0.5,1), include.lowest=T)
polknow_quantile[1] <- -Inf
polknow_quantile[3] <- Inf

diglit_quantile <- quantile(d$post_dig_lit, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
diglit_quantile[1] <- -Inf
diglit_quantile[4] <- Inf

dd <- bind_rows(
    ## diff-in-means
    d %>% filter(!is.na(exp_2)) %>%
        group_by(exp_2, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error)) %>% mutate(type="All", group="")
    ,
    ## interventions
    d %>% filter(!is.na(exp_2)) %>%
        group_by(exp_2, exp_1_prompt, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        mutate(exp_1_prompt = as.character(exp_1_prompt)) %>%
        mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "control", "Received\nno information")) %>%
        mutate(exp_1_prompt = replace(exp_1_prompt, exp_1_prompt == "info", "Received\ninformation")) %>%
        filter(!is.na(std.error)) %>% rename(group=exp_1_prompt) %>% mutate(type="By Intervention\nSubgroups")
    ,
    d %>% filter(!is.na(exp_2_after_debrief)) %>%
        group_by(exp_2, exp_2_after_debrief, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        mutate(exp_2_after_debrief = as.character(exp_2_after_debrief)) %>%
        mutate(exp_2_after_debrief = replace(exp_2_after_debrief, exp_2_after_debrief == "0", "Debriefed\nbefore task")) %>%
        mutate(exp_2_after_debrief = replace(exp_2_after_debrief, exp_2_after_debrief == "1", "Debriefed\nafter task")) %>%
        filter(!is.na(std.error)) %>% rename(group=exp_2_after_debrief) %>% mutate(type="By Intervention\nSubgroups")
    ,
    d %>% filter(!is.na(exp_2_prompt)) %>%
        group_by(exp_2, exp_2_prompt, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        mutate(exp_2_prompt = as.character(exp_2_prompt)) %>%
        mutate(exp_2_prompt = replace(exp_2_prompt, exp_2_prompt == "accuracy", "Received\naccuracy prime")) %>%
        mutate(exp_2_prompt = replace(exp_2_prompt, exp_2_prompt == "control", "Received\nno accuracy prime")) %>%
        filter(!is.na(std.error)) %>% rename(group=exp_2_prompt) %>% mutate(type="By Intervention\nSubgroups")
    ,
    ## knowledge
    d %>% filter(!is.na(polknow)) %>%
        mutate(polknow=cut(polknow, breaks=polknow_quantile, 
                           labels=c("Less \nknowledge", "More \nknowledge"))) %>%
        group_by(exp_2, polknow, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error)) %>% rename(group=polknow) %>% mutate(type="By Pol.\nKnowledge")
    ,
    ## dig-lit
    d %>% filter(!is.na(post_dig_lit)) %>%
        mutate(post_dig_lit=cut(post_dig_lit, breaks=diglit_quantile, 
                                labels=c("Low digital\nliteracy", "Moderate digital\nliteracy","High digital\nliteracy"))) %>%
        group_by(exp_2, post_dig_lit, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error), !is.na(post_dig_lit)) %>% rename(group=post_dig_lit) %>% mutate(type="By Digital\nLiteracy")
    ,
    ## CR
    d %>% filter(!is.na(crt)) %>%
        mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
                   labels=c("Low\nreflection", "Moderate\nreflection", "High\nreflection"))) %>%
        group_by(exp_2, crt, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error), !is.na(crt)) %>% rename(group=crt) %>% mutate(type="By Cognitive\nReflection") 
    ,
    ## PID
    d %>% filter(!is.na(PID)) %>%
        group_by(exp_2, PID, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error), !is.na(PID)) %>% rename(group=PID) %>% mutate(type="By Partisan\nIdentity") 
)
dd$group <- as_factor(dd$group)
dd$type <- as_factor(dd$type)

pp <- dd %>%
    mutate(nn=ifelse(grepl("Acc",metric), 0.04, ifelse(grepl("Neg",metric), 0.035, 0.1))) %>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    scale_shape_manual(values=c(21,22,24)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=5.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(data = dd %>% filter(type=="By Intervention\nSubgroups"), aes(xintercept=4.5), lty=1, color="grey", alpha=1) +
    geom_vline(data = dd %>% filter(type=="By Intervention\nSubgroups"), aes(xintercept=2.5), lty=1, color="grey", alpha=1) +
    # geom_vline(data = dd %>% filter(type=="By Knowledge\nSubgroups"), aes(xintercept=2.5), lty=1, color="grey", alpha=1) +
    geom_segment(data = dd %>%
                     group_by(metric,type,exp_2) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(metric,type,group,group_x) %>%
                     summarise(yint=mean(estimate)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint),
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=exp_2, fill=exp_2), position=position_dodge(width=.7), color="black", size=.5) +
    geom_text(aes(group=exp_2, label=exp_2, y=estimate+1.96*std.error+nn), position=position_dodge(width=.7), size=3) +
    facet_grid(type ~ metric, space="free_y", scales="free") + 
    scale_x_discrete(expand=c(0,0)) +
    ylab("mean score in group (95% CI)") + xlab("") + theme_linedraw() + coord_flip() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          panel.spacing.x = unit(1.25, "lines"),
          # axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background.x = element_blank(),
          strip.text.x = element_text(size=13, colour="black"),
          strip.text.y = element_text(size=10),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14))
ggsave("figures/topline_exp2.pdf", plot = pp, height=10.5, width=8)
system("open figures/topline_exp2.pdf")

