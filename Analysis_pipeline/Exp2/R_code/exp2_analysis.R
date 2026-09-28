libs = c("dplyr","tidyverse","lme4","lmerTest","ez","lubridate", "ggsignif","karthik",
         "wesanderson","Cairo","DescTools","emmeans","broom","sjPlot", "patchwork",
         "ggeffects", "ggpubr", "pracma","imputeTS", "zoo", "Hmisc", "Rmisc", "lmerTest")
lapply(libs, require, character.only = TRUE)
baseDir<- "~/Downloads/"
setwd(baseDir)

test_data<-read_csv("test_exp2.csv")
pred_data<-read_csv("pred_exp2.csv")
enc_data<-read_csv("enc_exp2.csv")


attn<-pred_data %>% 
  dplyr::group_by(`Participant Public ID`) %>% #filter(`Participant Public ID` %in% participants_good) %>% 
  dplyr::filter(display == "Attention") %>%
  dplyr::mutate(attemptCount = replace_na(Attempt,0)) %>% 
  dplyr::filter(!(attemptCount > 1)) %>% 
  dplyr::summarise(attn = sum(attemptCount)) %>% 
  dplyr::filter(attn < 3)
attnTest<-test_data %>% 
  dplyr::group_by(`Participant Public ID`) %>% #filter(`Participant Public ID` %in% participants_good) %>% 
  dplyr::filter(display == "Attention") %>%
  dplyr::mutate(attemptCount = replace_na(Attempt,0)) %>% 
  dplyr::filter(!(attemptCount > 1)) %>% 
  dplyr::summarise(attn = sum(attemptCount)) %>% 
  dplyr::filter(attn < 3)


#clean pred data
drop.cols<-tolower(c("Event Index", "UTC Timestamp", "UTC Date and Time", "Local Timestamp", 
                     "Local Timezone", "Local Date and Time", "Experiment ID", 
                     "Experiment Version", "Tree Node Key", "Repeat Key", "Schedule ID", 
                     "Participant Starting Group", "Participant Completion Code", 
                     "Participant Device Type", "Participant Device", "Participant OS", 
                     "Participant Browser", "Participant Status")) # can add more here if we want
drop.cols<-str_replace(drop.cols, " ", "_")

pred_data<-pred_data %>%
  dplyr::rename_all(~str_replace(.," ", "_")) %>% #removes white space from column names
  dplyr::rename_all(tolower) %>% #makes all column names lower case
  dplyr::rename(participant = `participant_public id`) %>% 
  dplyr::select(-drop.cols) %>% 
  dplyr::filter(display == "Prediction") %>% #remove instructions and NAs
  dplyr::filter(attempt < 2 | is.na(attempt)) %>% #remove times when the p responded more than once
  dplyr::filter(zone_type != "fixation")%>% 
  dplyr::filter(screen_name == "Decision")

#now we're going to create a new df that only includes the trials where people responded 
pred_data_resp<-subset(pred_data, zone_name == "Zone3" | zone_name == "Zone4")
pred_data_resp$attempt_clean<-pred_data_resp$attempt
pred_data_resp<-select(pred_data_resp,"attempt_clean", "zone_name", "participant",
                       "trial_number", "correct", "reaction_time")

pred_data_check<-left_join(pred_data, pred_data_resp, by = c("participant", "trial_number"))

pred_data_check<-pred_data_check %>% 
  filter(screen_name == "Decision" & zone_name.x == "Zone5")  %>% #only include timelimit zone bc it happens on every trial
  mutate(corr = if_else(!is.na(attempt_clean), correct.y, 0)) %>%   # missed responses are WRONG 
  mutate(corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))

few_Pred_responses <- pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(corr_word=="missed")) %>% 
  dplyr::filter(missed > 14.4)

predAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::filter(display == "Prediction") %>% 
  dplyr::summarise(avg = mean(corr, na.rm =TRUE)) %>% 
  dplyr::filter(avg < 0.500)
fullpredAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::filter(display == "Prediction") %>% 
  dplyr::summarise(avg = mean(corr, na.rm =TRUE))

#clean test data
test_data<-test_data %>%
  dplyr::rename_all(~str_replace(.," ", "_")) %>% #removes white space from column names
  dplyr::rename_all(tolower) %>% #makes all column names lower case
  dplyr::rename(participant = `participant_public id`) %>% 
  dplyr::select(-drop.cols) %>% 
  dplyr::filter(display == "Test") %>% #remove instructions and NAs
  dplyr::filter(attempt < 2 | is.na(attempt)) #remove times when the p responded more than once


test_data$participant<-as.character(test_data$participant)
test_data <- test_data %>% 
  mutate(corrConf = if_else((response == "a" | response == "s" | response == "d"), "old", "new")) %>% 
  mutate(corrConf = as.numeric((corrConf == corrans))) %>% 
  mutate(Numeric_Confidence = if_else(response == "a" | response == "l", 3, if_else(response == "s"|response == "k", 2, 1)))

#now we're going to create a new df that only includes the trials where people responded 
test_data_resp<-subset(test_data, zone_name == "Zone2" | zone_name == "Zone3" | zone_name == "Zone4" | zone_name == "Zone7" | zone_name == "Zone8" | zone_name == "Zone9")
test_data_resp$attempt_clean<-test_data_resp$attempt
test_data_resp<-select(test_data_resp,"attempt_clean", "zone_name", "participant", "response",
                       "trial_number", "corrConf", "reaction_time", "Numeric_Confidence")

test_data<-left_join(test_data, test_data_resp, by = c("participant", "trial_number"))

test_data<-test_data %>% 
  filter(zone_name.x == "Zone14")  %>% #only include timelimit zone bc it happens on every trial
  mutate(corr = if_else(!is.na(attempt_clean), corrConf.y, 0)) %>%   # missed responses WRONG
  mutate(corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))
few_mem_responses <- test_data %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(corr_word=="missed")) %>% 
  dplyr::filter(missed > 24.8)
#enc data cleaning--
enc_data<-enc_data %>%
  dplyr::rename_all(~str_replace(.," ", "_")) %>% #removes white space from column names
  dplyr::rename_all(tolower) %>% #makes all column names lower case
  dplyr::rename(participant = `participant_public id`) %>% 
  dplyr::select(-drop.cols) %>% 
  dplyr::filter(display == "Encoding") %>% #remove instructions and NAs
  dplyr::filter(attempt < 2 | is.na(attempt)) %>% #remove times when the p responded more than once
  dplyr::filter(zone_type != "fixation")


##test phase
#add a signal column
test_data<-test_data %>%
  filter(!is.na(corr)) %>% 
  mutate(signal=if_else((response.y == "a"|response.y == "s"|response.y == "d") & corr == 1, "Hit",
                        if_else((response.y == "a"|response.y == "s"|response.y == "d") & corr == 0, "FA",
                                if_else((response.y == "j"|response.y == "k"|response.y == "l") & corr == 1, "CR", "Miss"), missing = "Miss")))
##dprime
dprime_overall<-test_data %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarize(hitRate = sum(signal == "Hit")/sum(corrans=="old"),
                   faRate = sum(signal == "FA")/sum(corrans=="new")) %>%
  dplyr::mutate(dprime = qnorm(hitRate) - qnorm(faRate)) 
dprime_overall

dprimeExcl<-dprime_overall %>% 
  dplyr::filter(dprime <= 0)

#removing subjects who have below chance on BOTH prediction and memory
exclude <- union(few_mem_responses$participant, few_Pred_responses$participant)
belowChanceParticipant <- intersect(predAcc$participant, dprimeExcl$participant)
belowChanceParticipant <- union(belowChanceParticipant, exclude)
for (i in 1:length(belowChanceParticipant)){
  if(!is_empty(belowChanceParticipant)){
    pred_data_check <- pred_data_check %>% 
      filter(participant !=belowChanceParticipant[i])
    test_data <- test_data %>% 
      filter(participant !=belowChanceParticipant[i])
  }
}
pred_data_check$participant<-as.factor(pred_data_check$participant)
test_data$participant<-as.factor(test_data$participant)

#accuracy checks
pred_data_check$corrdist<-as.numeric(pred_data_check$corrdist)
pred_data_check$rt<-as.numeric(pred_data_check$reaction_time.y)

##prediction
summary(glmer(corr~corrdist+(1+corrdist|participant), family = "binomial", data = pred_data_check))
summary(lmer(rt~corrdist+(1+corrdist|participant), data = pred_data_check, subset = (corr == 1)))


#main analyses
#Merge data sets to get cor_dist and test data together
pred_data_check$recogimage<-pred_data_check$cue
pred_data_check$corrPred<-pred_data_check$corr
test_data$corrTest<-test_data$corr
test_data$rtTest<-test_data$reaction_time.y
combined_data <- left_join(test_data, pred_data_check, by = c("participant", "recogimage"))

combined_data<-combined_data %>%  #separates out missed responses from incorrect responses
  mutate(corr_missed = ifelse(corrPred == 1, "correct", "incorrect")) 
table(combined_data$corr_missed)
table(combined_data$corr.y)
combined_data<-combined_data %>% 
  mutate(corr_missed_e = ifelse(corr_missed == "incorrect", -0.5, ifelse(corr_missed == "correct", 0.5, NA))) %>% 
  mutate(cor_distance_e = ifelse(corrdist == "1", -.5, .5))


#are participants more accurate on the memory test for shorter prediction trials?
summary(distance_mod<-glmer(corrTest~cor_distance_e + (1+cor_distance_e|participant), family = "binomial", data = combined_data, subset = (expphase == "pred" & corrPred == 1&!is.na(corrTest)&corrdist!= incorrdist)))
plot_model(distance_mod, type = "pred", terms = c("cor_distance_e"))

##are people less accurate on the memory test for correct prediction trials and v.v.? 
summary(tradeOff_mod<-glmer(corrTest~corr_missed_e+(1+corr_missed_e|participant), family = "binomial", data = combined_data, subset = (corr_missed != "missed"&!is.na(corrTest))))
plot_model(tradeOff_mod, type = "pred", terms = c("corr_missed_e"))

meanAcc<-combined_data %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::filter(corrans.x=="old") %>% 
  dplyr::filter(corrdist!=incorrdist) %>% 
  dplyr::summarise(avgAcc = mean(corrTest, na.rm = T)) %>% 
  pivot_wider(names_from = c(corr_missed_e), values_from = avgAcc) 

t.test(meanAcc$`0.5`, meanAcc$`-0.5`, paired = TRUE)

newcombined_data<-combined_data %>%
  filter(!(corrdist==incorrdist)) %>%
  mutate(deterministic_e=if_else((sequence_position.y == "B1"|sequence_position.y == "B2"|sequence_position.y == "E1"|sequence_position.y == "E2"), "DD",
                                 if_else((sequence_position.y == "A"|sequence_position.y == "D"), "ND",
                                         if_else((sequence_position.y == "C"|sequence_position.y == "F"), "DN", "NA"), missing = NA)))

newcombined_data$deterministic_e <- factor(newcombined_data$deterministic_e, levels = c("DD","DN", "ND"))
summary(distance_mod<-glmer(corrTest~cor_distance_e+deterministic_e + (1+cor_distance_e|participant), family = "binomial", data = newcombined_data, subset = (expphase == "pred" & corrPred == 1)))
summary(tradeOff_mod<-glmer(corrTest~corr_missed_e+deterministic_e+(1+corr_missed_e|participant), family = "binomial", data = newcombined_data))

#Plot Distance vs. Prediction Accuracy
summary(dist_predAcc_model <- glmer(corrPred~cor_distance_e*deterministic_e+(1+cor_distance_e|participant), family = "binomial", data =newcombined_data))
avg_data_predict <- ggpredict(dist_predAcc_model, terms = c("cor_distance_e [-0.5:0.5 by=1]","deterministic_e"), ci_level = .95)
avg_data_predict <- avg_data_predict %>% mutate(deterministic_e = group)
avg_data_predict$deterministic_e <- factor(avg_data_predict$deterministic_e, levels = c("DD","DN", "ND"))

subj_dataDD<-newcombined_data %>%
  filter(deterministic_e =="DD") %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrPred))
subj_dataDN<-newcombined_data %>%
  filter(deterministic_e =="DN") %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrPred))
subj_dataND<-newcombined_data %>%
  filter(deterministic_e =="ND") %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrPred))


ggplot()+
  geom_jitter(data =subj_dataDN, aes(x=cor_distance_e-0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#5086FF")+
  geom_jitter(data =subj_dataND, aes(x=cor_distance_e+0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#17B12B")+
  geom_jitter(data =subj_dataDD, aes(x=cor_distance_e,y=hitRate), width = 0.01, alpha = 0.25, color = "#F35E5A")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill = deterministic_e), alpha=.2) + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted, color = deterministic_e), alpha = 0.65, size=2)+
  ylim(-0.05,1.05)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("1","2"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),legend.text=element_text(size=15),legend.title=element_text(size=15),axis.title=element_text(size=20,face="bold"))+
  scale_color_manual(values = c(DD = "#F35E5A",
                                DN = "#5086FF",
                                ND = "#17B12B"),labels = c("Both Steps Certain", "Second Step Uncertain", "First Step Uncertain"))+
  scale_fill_manual(values = c(DD = "#F35E5A",
                               DN = "#5086FF",
                               ND = "#17B12B"),guide = "none")+
  ylab("Prediction Accuracy")+
  xlab("Prediction Distance (Steps)")+
  labs(color="")+
  theme(
    legend.position = c(.8, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


#Plot Distance vs. RT
summary(dist_predRT_model <- lmer(rt~cor_distance_e*deterministic_e+(1+cor_distance_e|participant), data = newcombined_data, subset = (corrPred == 1)))
avg_data_predict <- ggpredict(dist_predRT_model, terms = c("cor_distance_e [-0.5:0.5 by=1]","deterministic_e"), ci_level = .95)
avg_data_predict <- avg_data_predict %>% mutate(deterministic_e = group)
avg_data_predict$deterministic_e <- factor(avg_data_predict$deterministic_e, levels = c("DD","DN", "ND"))

subj_dataDD<-newcombined_data %>%
  filter(deterministic_e =="DD",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(rt = mean(rt))
subj_dataDN<-newcombined_data %>%
  filter(deterministic_e =="DN",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(rt = mean(rt))
subj_dataND<-newcombined_data %>%
  filter(deterministic_e =="ND",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(rt = mean(rt))


ggplot()+
  geom_jitter(data =subj_dataDN, aes(x=cor_distance_e-0.05,y=rt), width = 0.01, alpha = 0.25, color = "#5086FF")+
  geom_jitter(data =subj_dataND, aes(x=cor_distance_e+0.05,y=rt), width = 0.01, alpha = 0.25, color = "#17B12B")+
  geom_jitter(data =subj_dataDD, aes(x=cor_distance_e,y=rt), width = 0.01, alpha = 0.25, color = "#F35E5A")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill = deterministic_e), alpha=.2) + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted, color = deterministic_e), alpha = 0.65,size=2)+
  ylim(0,1900)+
  annotate("text", label = '***',x=0, y=1500, size = 10)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("1","2"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),legend.text=element_text(size=15),legend.title=element_text(size=15),axis.title=element_text(size=20,face="bold"))+
  scale_color_manual(values = c(DD = "#F35E5A",
                                DN = "#5086FF",
                                ND = "#17B12B"),labels = c("Both Steps Certain", "Second Step Uncertain", "First Step Uncertain"))+
  scale_fill_manual(values = c(DD = "#F35E5A",
                               DN = "#5086FF",
                               ND = "#17B12B"),guide = "none")+
  ylab("Prediction Response Time (ms)")+
  xlab("Prediction Distance (Steps)")+
  labs(color="")+
  theme(
    legend.position = c(.8, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

#Plot Prediction Accuracy vs. Memory Hit Rate
summary(tradeOff_mod<-glmer(corrTest~corr_missed_e*deterministic_e+(1+corr_missed_e|participant), family = "binomial", data = newcombined_data))
avg_data_predict <- ggpredict(tradeOff_mod, terms = c("corr_missed_e [-0.5:0.5 by=1]","deterministic_e"), ci_level = .95)
avg_data_predict <- avg_data_predict %>% mutate(deterministic_e = group)
avg_data_predict$deterministic_e <- factor(avg_data_predict$deterministic_e, levels = c("DD","DN", "ND"))

subj_dataDD<-newcombined_data %>%
  filter(deterministic_e =="DD",corrans.x=="old") %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataDN<-newcombined_data %>%
  filter(deterministic_e =="DN",corrans.x=="old") %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataND<-newcombined_data %>%
  filter(deterministic_e =="ND",corrans.x=="old") %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))


ggplot()+
  geom_jitter(data =subj_dataDN, aes(x=corr_missed_e-0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#5086FF")+
  geom_jitter(data =subj_dataND, aes(x=corr_missed_e+0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#17B12B")+
  geom_jitter(data =subj_dataDD, aes(x=corr_missed_e,y=hitRate), width = 0.01, alpha = 0.25, color = "#F35E5A")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill = deterministic_e), alpha=.2) + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted, color = deterministic_e), alpha = 0.65, size=2)+
  ylim(-0.01,1.01)+
  annotate("text", label = '*',x=0, y=.7, size = 10)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("Incorrect","Correct"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),legend.text=element_text(size=15),legend.title=element_text(size=15),axis.title=element_text(size=20,face="bold"))+
  scale_color_manual(values = c(DD = "#F35E5A",
                                DN = "#5086FF",
                                ND = "#17B12B"),labels = c("Both Steps Certain", "Second Step Uncertain", "First Step Uncertain"))+
  scale_fill_manual(values = c(DD = "#F35E5A",
                               DN = "#5086FF",
                               ND = "#17B12B"),guide = "none")+
  ylab("Memory Hit Rate")+
  xlab("Prediction Accuracy")+
  labs(color="")+
  theme(
    legend.position = c(.8, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

#Plot Distance vs. Memory
summary(distance_mod<-glmer(corrTest~cor_distance_e*deterministic_e + (1+cor_distance_e|participant), family = "binomial", data = newcombined_data, subset = (expphase == "pred" & corrPred == 1)))
avg_data_predict <- ggpredict(distance_mod, terms = c("cor_distance_e [-0.5:0.5 by=1]","deterministic_e"), ci_level = .95)
avg_data_predict <- avg_data_predict %>% mutate(deterministic_e = group)
avg_data_predict$deterministic_e <- factor(avg_data_predict$deterministic_e, levels = c("DD","DN", "ND"))

subj_dataDD<-newcombined_data %>%
  filter(deterministic_e =="DD",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataDN<-newcombined_data %>%
  filter(deterministic_e =="DN",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataND<-newcombined_data %>%
  filter(deterministic_e =="ND",corrPred == 1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))


ggplot()+
  geom_jitter(data =subj_dataDN, aes(x=cor_distance_e-0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#5086FF")+
  geom_jitter(data =subj_dataND, aes(x=cor_distance_e+0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#17B12B")+
  geom_jitter(data =subj_dataDD, aes(x=cor_distance_e,y=hitRate), width = 0.01, alpha = 0.25, color = "#F35E5A")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill = deterministic_e), alpha=.2) + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted, color = deterministic_e), alpha = 0.65,size=2)+
  ylim(-0.05,1.05)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("1","2"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),legend.text=element_text(size=15),legend.title=element_text(size=15),axis.title=element_text(size=20,face="bold"))+
  scale_color_manual(values = c(DD = "#F35E5A",
                                DN = "#5086FF",
                                ND = "#17B12B"),labels = c("Both Steps Certain", "Second Step Uncertain", "First Step Uncertain"))+
  scale_fill_manual(values = c(DD = "#F35E5A",
                               DN = "#5086FF",
                               ND = "#17B12B"),guide = "none")+
  ylab("Memory Hit Rate")+
  xlab("Prediction Distance (Steps)")+
  labs(color="")+
  theme(
    legend.position = c(.8, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
