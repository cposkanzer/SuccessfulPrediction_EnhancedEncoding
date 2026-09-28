libs = c("dplyr","tidyverse","lme4","lmerTest","ez","lubridate", "ggsignif","karthik",
         "wesanderson","Cairo","DescTools","emmeans","broom","sjPlot", "patchwork",
         "ggeffects", "ggpubr", "pracma","imputeTS", "zoo", "Hmisc", "rmisc", "lmerTest")
lapply(libs, require, character.only = TRUE)
baseDir<- "~/Desktop/Craig/exp2/"
setwd(baseDir)

#load files-------------------------------
test_excl<-read_csv("exp1_testExcl.csv")
pred_excl<-read_csv("exp1_predExcl.csv")
enc_excl<-read_csv("exp1_encExcl.csv")

#included participants
test_data<-read_csv("exp1_testData.csv")
pred_data<-read_csv("exp1_predData.csv")
enc_data<-read_csv("exp1_encData.csv")

test_data <- rbind(test_data,subset(test_excl[,2:60]))
pred_data <- rbind(pred_data,subset(pred_excl[,2:65]))

attn<-pred_data %>% 
  group_by(`Participant Public ID`) %>% 
  filter(display == "Attention") %>%
  mutate(attemptCount = replace_na(Attempt,0)) %>% 
  filter(!(attemptCount > 1)) %>% 
  summarise(attn = sum(attemptCount)) #%>% 

attnTest<-test_data %>% 
  group_by(`Participant Public ID`) %>% 
  filter(display == "Attention") %>%
  mutate(attemptCount = replace_na(Attempt,0)) %>% 
  filter(!(attemptCount > 1)) %>% 
  summarise(attn = sum(attemptCount)) #%>% 


#clean pred data-------------------------
drop.cols<-tolower(c("Event Index", "UTC Timestamp", "UTC Date", "Local Timestamp", 
                     "Local Timezone", "Local Date", "Experiment ID", 
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
  mutate(corr = if_else(!is.na(attempt_clean), correct.y, 0)) %>%   # missed responses 0
  mutate(corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))
#check accuracy for exclusion
fullpredAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::filter(display == "Prediction") %>% 
  dplyr::summarise(avg = mean(corr))
predAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::filter(display == "Prediction") %>% 
  dplyr::summarise(avg = mean(corr)) %>% 
  dplyr::filter(avg < 0.500)
few_Pred_responses <- pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(corr_word=="missed")) %>% 
  dplyr::filter(missed > 20)

#clean test data---------------------
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
  mutate(corr = if_else(!is.na(attempt_clean), corrConf.y, 0)) %>%   # missed responses 0
  mutate(corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))
few_mem_responses <- test_data %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(corr_word=="missed")) %>% 
  dplyr::filter(missed > 48)
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
  mutate(signal=if_else((response.y == "a"|response.y == "s"|response.y == "d") & corr == 1, "Hit",
                        if_else((response.y == "a"|response.y == "s"|response.y == "d") & corr == 0, "FA",
                                if_else((response.y == "j"|response.y == "k"|response.y == "l") & corr == 1, "CR", "Miss"), missing = "Miss")))
##dprime
dprime_overall<-test_data %>% 
  filter(!is.na(response.y)&expphase!="enc") %>% 
  group_by(participant) %>% 
  summarize(hitRate = sum(signal == "Hit")/sum(corrans=="old"),
            faRate = sum(signal == "FA")/sum(corrans=="new")) %>%
  mutate(dprime = qnorm(hitRate) - qnorm(faRate))
dprime_overall

dprimeExcl<-dprime_overall %>% 
  dplyr::filter(dprime <= 0)


#removing subjects who have below chance on BOTH prediction and memory
exclude <- union(few_Pred_responses$participant, few_mem_responses$participant)
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
#accuracy checks---------------
pred_data_check$corrdist<-as.numeric(pred_data_check$corrdist)
pred_data_check$rt<-as.numeric(pred_data_check$reaction_time.y)

##prediction
summary(glmer(corr~corrdist+(1+corrdist|participant), family = "binomial", data = pred_data_check))
summary(lmer(rt~corrdist+(1+corrdist|participant), data = pred_data_check, subset = (corr == 1)))


#main analyses------------------
#Merge data sets to get cor_dist and test data together
pred_data_check$recogimage<-pred_data_check$cue
pred_data_check$corrPred<-pred_data_check$corr
test_data$corrTest<-test_data$corr
combined_data <- left_join(test_data, pred_data_check, by = c("participant", "recogimage"))

combined_data<-combined_data %>%  #separates out missed responses from incorrect responses
  mutate(corr_missed = ifelse(corrPred == 1, "correct", "incorrect"))
table(combined_data$corr_missed)
table(combined_data$corr.y)
combined_data<-combined_data %>%
  mutate(corr_missed_e = ifelse(corr_missed == "incorrect", -0.5, ifelse(corr_missed == "correct", 0.5, NA))) %>% 
  mutate(cor_distance_e = ifelse(corrdist == "1", -1.5, ifelse(corrdist == "2", -0.5, ifelse(corrdist == "3",0.5, 1.5))))


#are participants more accurate on the memory test for shorter prediction trials?
summary(distance_mod<-glmer(corrTest~cor_distance_e + (1+cor_distance_e|participant), family = "binomial", data = combined_data, subset = (expphase == "pred" & corrPred == 1&!is.na(corrTest))))
plot_model(distance_mod, type = "pred", terms = c("cor_distance_e"))

##are people less accurate on the memory test for correct prediction trials and v.v.? 
summary(tradeOff_mod<-glmer(corrTest~corr_missed_e+(1+corr_missed_e|participant), family = "binomial", data = combined_data, subset = (corr_missed != "missed"&!is.na(corrTest))))
plot_model(tradeOff_mod, type = "pred", terms = c("corr_missed_e"))

meanAcc<-combined_data %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::filter(corr_missed != "missed"&!is.na(corrTest)) %>% 
  dplyr::summarise(avgAcc = mean(corrTest, na.rm = T)) %>% 
  pivot_wider(names_from = corr_missed_e, values_from = avgAcc) 
t.test(meanAcc$`0.5`, meanAcc$`-0.5`, paired = TRUE)

meanAcc<-combined_data %>% 
  filter(corrans.x=="old"&!is.na(cor_distance_e)) %>% 
  mutate(cor_distance = ifelse(cor_distance))
dplyr::group_by(participant, cor_distance) %>% 
  dplyr::summarise(avgAcc = mean(corrTest)) 
ezANOVA(data = meanAcc, dv = avgAcc, wid = participant, within = .(cor_distance_e))

#Plots

#Prediction Accuracy and RT
summary(dist_predAcc_model <- glmer(corrPred~cor_distance_e+(1+cor_distance_e|participant), family = "binomial", data = combined_data))
avg_Accdata_predict <- ggpredict(dist_predAcc_model,terms = "cor_distance_e [-1.5:1.5 by=0.01]", ci_level=0.95)
summary(dist_predRT_model <- lmer(rt~cor_distance_e+(1+cor_distance_e|participant), data = combined_data, subset = (corrPred == 1)))
avg_RTdata_predict <- ggpredict(dist_predRT_model,terms = "cor_distance_e [-1.5:1.5 by=0.01]", ci_level=0.95)
subj_RTdata<-combined_data %>%
  filter(corrPred==1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(RT = mean(rt))
subj_Accdata<-combined_data %>%
  filter(!is.na(corrTest)) %>%
  filter(!is.na(corr_missed)) %>%  
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(pred_acc = mean(corrPred))

ggplot()+
  geom_jitter(data =subj_Accdata, aes(x=cor_distance_e,y=pred_acc), width = 0.025,alpha = 0.25, color= "#F15A24")+
  geom_ribbon(data=avg_Accdata_predict, aes(x=x,ymin=conf.low, ymax=conf.high), fill = "#F15A24", alpha = 0.4)+
  geom_line(data=avg_Accdata_predict, aes(x=x,y=predicted), size=2, color= "#F15A24")+
  scale_y_continuous(
    name = "Prediction Accuracy",,limits = c(-0.01,1.05))+
  scale_x_continuous(breaks = c(-1.5, -.5,.5,1.5), labels = c("1","2","3","4"))+
  theme(legend.position = "none",plot.background = element_blank())+
  annotate("text", label = '***',x=0, y=.9, size = 10)+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"),plot.margin = margin(10, 30, 10, 10))+
  theme(
    legend.position = c(.55, .85),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.title.y = element_text(size=20,face="bold"),
    axis.line = element_line(colour = "black", size = 1.5),
    axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")
  )+
  #theme(axis.text=element_text(size=20),axis.title=element_text(size=20))+
  xlab("Prediction Distance (steps)")


ggplot()+
  geom_jitter(data =subj_RTdata, aes(x=cor_distance_e,y=RT), width = 0.025,alpha = 0.25, color= "#F15A24")+
  geom_ribbon(data=avg_RTdata_predict, aes(x = x, ymin=conf.low, ymax=conf.high), fill = "#F15A24", alpha = 0.4)+
  geom_line(data=avg_RTdata_predict, aes(x=x,y=predicted), size=2, color= "#F15A24")+
  scale_y_continuous(
    name = "Prediction Response Time (ms)",,limits = c(0,2500))+
  scale_x_continuous(breaks = c(-1.5, -.5,.5,1.5), labels = c("1","2","3","4"))+
  theme(legend.position = "none",plot.background = element_blank())+
  annotate("text", label = '***',x=0, y=2250, size = 10)+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"),plot.margin = margin(10, 30, 10, 10))+
  theme(
    legend.position = c(.55, .85),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    axis.title.y = element_text(size=20,face="bold"),
    axis.line = element_line(colour = "black", size = 1.5),
    axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold")
  )+
  #theme(axis.text=element_text(size=20),axis.title=element_text(size=20))+
  xlab("Prediction Distance (steps)")

#Prediction Distance vs. Memory Hit Rate
summary(distance_mod<-glmer(corrTest~cor_distance_e + (1+cor_distance_e|participant), family = "binomial", data = combined_data, subset = (expphase == "pred" & corrPred == 1&!is.na(corrTest))))
avg_data_predict <- ggpredict(distance_mod, terms = "cor_distance_e [-1.5:1.5 by=0.01]", ci_level = .95)
subj_data<-combined_data %>%
  filter(corrans.x=="old") %>% 
  filter(!is.na(corrTest)) %>%
  filter(!is.na(corr_missed)) %>% 
  filter(corrPred==1) %>% 
  dplyr::group_by(participant, cor_distance_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))

ggplot()+
  geom_jitter(data =subj_data, aes(x=cor_distance_e,y=hitRate), width = 0.05, alpha = 0.25, color="#0000FF")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.4, fill="#0000FF") + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted), size=2, color="#0000FF")+
  ylim(0,1)+
  scale_x_continuous(breaks = c(-1.5, -.5,.5,1.5), labels = c("1","2","3","4"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  annotate("text", label = '*',x=0, y=.9, size = 10)+
  theme(legend.text=element_text(size=20),axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))+
  ylab("Memory Hit Rate")+
  xlab("Prediction Distance (steps)")+
  labs(color="Prediction Type")+
  theme(
    legend.position = c(.99, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )


#Prediction Accuracy vs. Memory Hit Rate
summary(tradeOff_mod<-glmer(corrTest~corr_missed_e+(1+corr_missed_e|participant), family = "binomial", data = combined_data, subset = (corr_missed != "missed"&!is.na(corrTest))))
avg_data_predict <- ggpredict(tradeOff_mod, terms = "corr_missed_e [-0.5:0.5 by=0.01]", ci_level = .95)
subj_data<-combined_data %>%
  filter(corrans.x=="old") %>% 
  filter(!is.na(corrTest)) %>%
  filter(!is.na(corr_missed)) %>% 
  dplyr::group_by(participant, corr_missed_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))

ggplot()+
  geom_jitter(data =subj_data, aes(x=corr_missed_e,y=hitRate), width = 0.05, alpha = 0.25, color="#0000FF")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high), alpha=.4, fill="#0000FF") + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted), size=2, color="#0000FF")+
  ylim(-0.001,1.01)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("Incorrect","Correct"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(legend.text=element_text(size=20),axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))+
  ylab("Memory Hit Rate")+
  xlab("Prediction Accuracy")+
  labs(color="Prediction Type")+
  theme(
    legend.position = c(.99, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
