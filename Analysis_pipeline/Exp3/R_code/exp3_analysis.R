libs = c("dplyr","tidyverse","lme4","lmerTest","ez","lubridate", "ggsignif","karthik",
         "wesanderson","Cairo","DescTools","emmeans","broom","sjPlot", "patchwork",
         "ggeffects", "ggpubr", "pracma","imputeTS", "zoo", "Hmisc", "Rmisc", "lmerTest","plotrix")
lapply(libs, require, character.only = TRUE)
baseDir<- "~/Downloads/"
setwd(baseDir)

#load files-------------------------------

#included participants
test_data<-read_csv("test_exp3.csv")
pred_data<-read_csv("pred_exp3.csv")
enc_data<-read_csv("enc_exp3.csv")

#clean pred data-------------------------
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
  dplyr::filter(attempt < 2 | is.na(attempt)) #%>% #remove times when the p responded more than once


#now we're going to create a new df that only includes the cue phase 
pred_data_cue_resp<-subset(pred_data, screen_name == "Cue"&zone_type!="timelimit_screen")
pred_data_cue_resp$attempt_clean<-pred_data_cue_resp$attempt
pred_data_cue_resp<-select(pred_data_cue_resp,"attempt_clean", "participant",
                           "trial_number","reaction_time", "response", "corrans")

#define correct cue answers
pred_data_cue_resp <- pred_data_cue_resp %>% 
  mutate(cue_rt = reaction_time) %>% 
  mutate(cue_key = if_else((corrans=="Not Predictable"), "Not Able to Predict", "Able to Predict")) %>% 
  mutate(correct_cue = if_else((response==cue_key),1,0))

#now we're going to create a new df that only includes the proe phase 
pred_data_probe_resp<-subset(pred_data, screen_name == "Screen 1"&zone_type!="timelimit_screen")
pred_data_probe_resp$attempt_clean<-pred_data_probe_resp$attempt
pred_data_probe_resp<-select(pred_data_probe_resp,"attempt_clean", "participant",
                             "trial_number","reaction_time", "response", "corrans")
#write a function to match responses to probe key
convert_to_singular <- function(x) {
  x <- sub("Ski_slopes", "Ski Slope", x)
  x <- sub("Castles", "Castle", x)
  x <- sub("Airplane_cabins", "Airplane Cabin", x)
  x <- sub("Forests", "Forest", x)
  x <- sub("Amusement_parks", "Amusement Park", x)
  x <- sub("Bathrooms", "Bathroom", x)
  x <- sub("Beaches", "Beach", x)
  x <- sub("City_skylines", "City Skyline", x)
  x <- sub("Kitchens", "Kitchen", x)
  x <- sub("Lecture_Halls", "Lecture Hall", x)
  x <- sub("Restaurants", "Restaurant", x)
  x <- sub("Bedrooms", "Bedroom", x)
  x <- sub("Not Predictable", "Random Image", x)
  "Not Predictable"
  return(x)
}
pred_data_probe_resp$corrans <- sapply(pred_data_probe_resp$corrans, convert_to_singular)

#define correct cue answers
pred_data_probe_resp <- pred_data_probe_resp %>% 
  mutate(probe_rt = reaction_time) %>% 
  mutate(correct_probe = if_else((response==corrans),1,0))

#missed responses are wrong
pred_data_check_cue<-left_join(pred_data, pred_data_cue_resp, by = c("participant", "trial_number"))
pred_data_check_cue<-pred_data_check_cue %>% 
  filter(screen_name == "Cue" & zone_name == "Zone2")  %>% #only include timelimit zone bc it happens on every trial
  mutate(cue_corr = if_else(!is.na(attempt_clean), correct_cue, 0))%>% #missed responses are 0
  mutate(cue_corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))  # missed responses are 0 

pred_data_check_probe<-left_join(pred_data, pred_data_probe_resp, by = c("participant", "trial_number"))
pred_data_check_probe<-pred_data_check_probe %>% 
  filter(screen_name == "Screen 1" & zone_name == "Zone2")  %>% #only include timelimit zone bc it happens on every trial
  mutate(probe_corr = if_else(!is.na(attempt_clean), correct_probe, 0))%>% #missed responses are 0
  mutate(probe_corr_word = if_else(!is.na(attempt_clean), "responded", "missed"))  # missed responses are 0 

#combine both datasets cue and probe answers
pred_data_check <- left_join(pred_data_check_cue, pred_data_check_probe, by = c("participant", "trial_number"))

#create a column for just the cue category
remove_image_numbers <- function(x) {
  sub("\\d.*", "",x)
}
pred_data_check <- pred_data_check %>% mutate(cue_category = sapply(pred_data_check$cue.x, remove_image_numbers)) 
#list of participants with too many missed responses
few_cue_responses <- pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(cue_corr_word=="missed")) %>% 
  dplyr::filter(missed > 24)
few_probe_responses <- pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr:: summarise(missed = sum(probe_corr_word=="missed")) %>% 
  dplyr::filter(missed > 24)

#check accuracy for exclusion
excl_cueAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarise(avg = mean(cue_corr)) %>% 
  dplyr::filter(avg < 0.500)

excl_probeAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarise(avg = mean(probe_corr)) %>% 
  dplyr::filter(avg < 0.07)


#see if they select random image when they say "not predictable"
not_predictable_sanitycheck <- pred_data_check %>% 
  dplyr::filter(response.y.x=="Not Able to Predict") %>% 
  dplyr::mutate(sanity_check = if_else((response.y.y=="Random Image"), 1, 0, missing=0)) %>%
  dplyr::group_by(participant) %>% 
  dplyr::summarize(mean_sanity = mean(sanity_check))

actually_predictable_sanitycheck <- pred_data_check %>% 
  dplyr::filter(sequence_position.x=="A") %>% 
  dplyr::mutate(true_pred_acc = if_else((probe_corr==1), 1, 0, missing=0)) %>%
  dplyr::group_by(participant) %>% 
  dplyr::summarize(mean_true_pred_acc = mean(true_pred_acc))


#do the analysis to detect when particpants have identified the correct pair 100% of the time 
update_col3 <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(abpair_100acc = ifelse(cue_category %in% corrans.x.x[which(pair_100acc == 1)], 1, pair_100acc))
  return(data)
}
actually_predictable_sanitycheck_byPair <- pred_data_check %>% 
  dplyr::mutate(true_pred_acc = if_else((probe_corr==1), 1, 0, missing=0)) %>%
  dplyr::group_by(participant, corrans.x.x, cue_category, sequence_position.x) %>% 
  dplyr::summarize(mean_true_pred_acc = mean(true_pred_acc), count = n())%>% 
  mutate(pair_100acc = ifelse(mean_true_pred_acc==1.0&sequence_position.x=="A"&count==10, 1,0)) %>% 
  dplyr::group_by(participant) %>% 
  do(update_col3(.))


#exclude people who say "not predictable" but then don't select random image
excl_randomImage_sanity<-not_predictable_sanitycheck %>% 
  dplyr::filter(mean_sanity<0.8)


multiple_wrong_predictions <- pred_data_check %>% filter(probe_corr==0, response.y.y!="Random Image") %>% dplyr::group_by(participant, cue_category,response.y.y) %>% dplyr::summarise(count = n()) %>% filter(count>6)

#look at treated pairs
update_treated_pairs <- function(data) {
  data <- data %>%
    rowwise() %>%
    mutate(abTreatedPairs = ifelse(cue_category %in% response_category[which(treated_as_pair == 0.5)], treated_as_pair+1, treated_as_pair))
  return(data)
}
convert_to_multiple <- function(x) {
  x <- sub("Ski Slope","Ski_slopes",  x)
  x <- sub( "Castle","Castles", x)
  x <- sub( "Airplane Cabin","Airplane_cabins", x)
  x <- sub( "Forest","Forests", x)
  x <- sub( "Amusement Park","Amusement_parks", x)
  x <- sub( "Bathroom","Bathrooms", x)
  x <- sub( "Beach", "Beaches",x)
  x <- sub( "City Skyline", "City_skylines",x)
  x <- sub( "Kitchen", "Kitchens",x)
  x <- sub( "Lecture Hall","Lecture_Halls", x)
  x <- sub( "Restaurant","Restaurants", x)
  x <- sub( "Bedroom","Bedrooms", x)
  #x <- sub("Not Predictable", "Random Image", x)
  #"Not Predictable"
  return(x)
}
multiple_predictions <- pred_data_check %>% 
  mutate(response_category = sapply(response.y.y, convert_to_multiple)) %>% 
  dplyr::group_by(participant, cue_category,response_category) %>% 
  dplyr::summarise(count = n()) %>% 
  mutate(treated_as_pair = ifelse(count>6&response_category!="Random Image", .5,0)) %>% 
  dplyr::group_by(participant) %>% 
  do(update_treated_pairs(.)) %>% 
  dplyr::group_by(participant, cue_category) %>% dplyr::summarise(ab_pairs_assumed = max(abTreatedPairs)) %>% 
  filter(ab_pairs_assumed!=1.5)

pred_data_check <-left_join(pred_data_check,multiple_predictions[,c(1,2,3)], by= c("participant","cue_category"))
pred_data_check <-left_join(pred_data_check,actually_predictable_sanitycheck_byPair[,c(1,3,7)], by= c("participant","cue_category"))

#use encoding performance for "treated pair"
reported_pairs<-read_csv("~/Downloads/encoding_pair_performance.csv")
reported_pairs <- reported_pairs %>% mutate(pred_a = `Predictive A`, pred_b = `Predictable B`,real_pair = `Yes or no?`, participant= `Participant public ID`)
reported_pairs <- reported_pairs[,c(8,5,6,7)]
reported_pairs <- pivot_longer(reported_pairs, cols = c(pred_a, pred_b))

reported_pairs_ab <- reported_pairs %>% 
  dplyr::mutate(reported_atEnc = ifelse(real_pair==1,1,0), cue_category = value)
reported_pairs_ab <- reported_pairs_ab[,c(1,6,5)]
pred_data_check <-left_join(pred_data_check,reported_pairs_ab, by= c("participant", "cue_category"))


fullpredAcc<-pred_data_check %>% 
  dplyr::group_by(participant) %>% 
  dplyr::summarise(cue_avg = mean(cue_corr), probe_avg = mean(probe_corr))

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
exclude <- union(few_mem_responses$participant, few_cue_responses$participant)
exclude <- union(exclude,few_probe_responses$participant)
exclude <- union(exclude,excl_randomImage_sanity$participant) 
predAcc <- union(excl_cueAcc$participant,excl_probeAcc$participant)
belowChanceParticipant <- intersect(predAcc, dprimeExcl$participant)
belowChanceParticipant <- union(belowChanceParticipant, exclude)
#belowChanceParticipant[2]="6234d77291179c1badffad36" #this subjects prediction section was corrupted
#belowChanceParticipant[1]="11621" #this subject didn't answer any prediction
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

#main analyses------------------
#Merge data sets to get cor_dist and test data together
pred_data_check$recogimage<-pred_data_check$cue.x
test_data$corrTest<-test_data$corr
test_data$rtTest<-test_data$reaction_time.y
combined_data <- left_join(test_data, pred_data_check, by = c("participant", "recogimage"))

table(combined_data$cue_corr)
table(combined_data$probe_corr)
table(combined_data$corrTest)
combined_data<-combined_data %>% 
  mutate(corr_cue_e = ifelse(cue_corr == 0, -0.5, 0.5)) %>% 
  mutate(corr_probe_e = ifelse(probe_corr == 0, -0.5, 0.5)) %>% 
  mutate(corr_test_e = ifelse(corrTest == 0, -0.5, 0.5)) %>% 
  mutate(sequence_e = ifelse(sequence_position.x == "A", -0.5, ifelse(sequence_position.x == "B", 0, 0.5)))
combined_data$sequence_position.x <- factor(combined_data$sequence_position.x, levels = c("X", "A","B"))

#position prediction
predicting_data <- combined_data %>% 
  filter(corrans=="old") %>% 
  mutate(able = if_else(response.y.x=="Able to Predict",1,0, missing = 0)) %>% 
  dplyr::group_by(participant, sequence_position.x) %>% 
  dplyr::summarize(p_predict = sum(able)/n())
predicting_data$sequence_position.x <- factor(predicting_data$sequence_position.x, levels = c("X", "A","B"))
ezANOVA(predicting_data,dv = p_predict, wid = participant, within = sequence_position.x)
ttestPredicting <- predicting_data %>% pivot_wider(names_from = sequence_position.x, values_from = p_predict) 
t.test(ttestPredicting$A, ttestPredicting$X, paired = TRUE)
t.test(ttestPredicting$A, ttestPredicting$B, paired = TRUE)
t.test(ttestPredicting$B, ttestPredicting$X, paired = TRUE)

avg_predicting <- predicting_data %>% 
  dplyr::group_by(sequence_position.x) %>% 
  dplyr:: summarise(avg_p = mean(p_predict))

#are participants more accurate on the memory test for different sequence positions?
position_memory <- combined_data %>% 
  filter(corrans=="old") %>% #,abpair_100acc==1, sequence_position.x!='X'
  dplyr::group_by(participant, sequence_position.x) %>% 
  dplyr::summarize(mem = mean(corrTest))
position_memory$sequence_position.x <- factor(position_memory$sequence_position.x, levels = c("X", "A","B"))
ezANOVA(position_memory,dv = mem, wid = participant, within = sequence_position.x)
ttestPositionmem <- position_memory %>% pivot_wider(names_from = sequence_position.x, values_from = mem) 
t.test(ttestPositionmem$A, ttestPositionmem$X, paired = TRUE)
t.test(ttestPositionmem$A, ttestPositionmem$B, paired = TRUE)
t.test(ttestPositionmem$B, ttestPositionmem$X, paired = TRUE)


avg_position_memory <- position_memory %>% 
  dplyr::group_by(sequence_position.x) %>% 
  dplyr:: summarise(avg_mem = mean(mem))
ggplot()+geom_col(data = avg_position_memory, aes(x = sequence_position.x, y = avg_mem))

#memory for encoding pairs
encoding_pred_memory <- combined_data %>% 
  filter(corrans=="old", !is.na(reported_atEnc)) %>% 
  dplyr::group_by(participant,reported_atEnc, sequence_position.x) %>% 
  dplyr::summarize(mem = mean(corrTest)) 
ttestEncodingPositionmem <- encoding_pred_memory %>% pivot_wider(names_from = c(reported_atEnc,sequence_position.x), values_from = mem)
participants_with_all_conditions <- encoding_pred_memory$participant[ave(rep(1, nrow(encoding_pred_memory)), encoding_pred_memory$participant, FUN = length) == 4]
ANOVAencoding_pred_memory <- encoding_pred_memory[encoding_pred_memory$participant %in% participants_with_all_conditions, ]
ANOVAencoding_pred_memory$sequence_position.x <- factor(ANOVAencoding_pred_memory$sequence_position.x, levels = c("X", "A","B"))
ezANOVA(ANOVAencoding_pred_memory,dv = mem, wid = participant, within = .(sequence_position.x, reported_atEnc))
summary(lmer(mem~reported_atEnc*sequence_position.x+(1|participant), data = encoding_pred_memory))
summary(glmer(corrTest~reported_atEnc*sequence_position.x+(1|participant), family = 'binomial', data = combined_data, subset = corrans=="old"))
t.test(ttestEncodingPositionmem$'0_-0.5', ttestEncodingPositionmem$'1_-0.5', paired = TRUE)
t.test(ttestEncodingPositionmem$'1_-0.5', ttestEncodingPositionmem$'1_0', paired = TRUE)
t.test(ttestEncodingPositionmem$'0_0', ttestEncodingPositionmem$'1_0', paired = TRUE)
t.test(ttestEncodingPositionmem$'0_-0.5', ttestEncodingPositionmem$'0_0', paired = TRUE)

#memory for encoding pairs including treated pairs
encodingTreated_pred_memory <- combined_data %>% 
  filter(corrans=="old") %>% 
  dplyr:: mutate(reported_position_f = ifelse(reported_atEnc==0&ab_pairs_assumed==0.5, "A",ifelse(reported_atEnc==0&ab_pairs_assumed==1,"B",sequence_position.x))) %>% 
  filter(reported_position_f!="X") %>% 
  dplyr::group_by(participant,reported_atEnc, sequence_position.x, reported_position_f) %>% 
  dplyr::summarize(mem = mean(corrTest))
encodingTreated_pred_memory_full <- combined_data %>% 
  dplyr:: mutate(reported_position_f = ifelse(reported_atEnc==0&ab_pairs_assumed==0.5, "A",ifelse(reported_atEnc==0&ab_pairs_assumed==1,"B",sequence_position.x)))
summary(lmer(mem~reported_atEnc*reported_position_f+(1|participant), data = encodingTreated_pred_memory))
summary(glmer(corrTest~reported_atEnc*reported_position_f+(1|participant), family = 'binomial', data = encodingTreated_pred_memory_full), subset = (corrans=="old"&reported_position_f!="X"))



avg_encodingPair_memory <- encoding_pred_memory %>% 
  dplyr::group_by(reported_atEnc, sequence_position.x) %>% 
  dplyr:: summarise(avg_mem = mean(mem)) 
ggplot()+geom_col(data = avg_encodingPair_memory, aes(x = reported_atEnc, y = avg_mem, fill = sequence_position.x), position = position_dodge(1))+
  geom_segment(aes(x = .75, y = .65, xend = 1.25, yend = .65))+
  annotate("text", label = '*',x=1, y=.67, size = 10)


#treated as pairs
active_pred_memory <- combined_data %>% 
  filter(corrans=="old") %>% 
  #dplyr::mutate(isPair = ifelse(is.na(treated_as_pair), 0,1)) %>% 
  dplyr::group_by(participant,ab_pairs_assumed) %>% 
  dplyr::summarize(mem = mean(corrTest)) 
ttestAssumedPositionmem <- active_pred_memory %>% pivot_wider(names_from = ab_pairs_assumed, values_from = mem) 
t.test(ttestAssumedPositionmem$'0.5', ttestAssumedPositionmem$'1', paired = TRUE)


avg_treatedPair_memory <- active_pred_memory %>% 
  filter(ab_pairs_assumed>0) %>% 
  dplyr::group_by(ab_pairs_assumed) %>% 
  dplyr:: summarise(avg_mem = mean(mem))
ggplot()+geom_col(data = avg_treatedPair_memory, aes(x = ab_pairs_assumed, y = avg_mem))

##are people less accurate on the memory test for correct prediction trials and v.v.? 
summary(tradeOff_mod_cue<-glmer(corrTest~corr_cue_e+(1+corr_cue_e|participant), family = "binomial", data = combined_data, subset = (corrans=="old"&sequence_position.x =="A")))
plot_model(tradeOff_mod_cue, type = "pred", terms = c("corr_cue_e"))

summary(tradeOff_mod_probe<-glmer(corrTest~corr_probe_e+(1+corr_probe_e|participant), family = "binomial", data = combined_data, subset = (corrans=="old"&sequence_position.x =="A")))
plot_model(tradeOff_mod_probe, type = "pred", terms = c("corr_probe_e"))

#are people better at memory when they say predictable?
choose_predicting_data <- combined_data %>% 
  filter(corrans=="old") %>%
  dplyr::mutate(predicting_e = ifelse(response.y.x=="Able to Predict", 0.5, -0.5),random_e = ifelse(response.y.y=="Random Image", 0.5, -0.5), position_e = ifelse(sequence_position.x=='A',-.5, 
                                                                                                                                                                  ifelse(sequence_position.x=='B',0,0.5))) 
choose_predicting_data$sequence_position.x <- factor(choose_predicting_data$sequence_position.x, levels = c("X", "A","B"))
summary(tradeOff_mod_probe<-glmer(corrTest~predicting_e*position_e+(1+predicting_e|participant), family = "binomial", data = choose_predicting_data, subset = (corrans=="old")))
summary(tradeOff_mod_probe<-glmer(corrTest~corr_probe_e*sequence_position.x+(1+corr_probe_e|participant), family = "binomial", data = choose_predicting_data, subset = (corrans=="old")))
plot_model(tradeOff_mod_probe, type = "pred", terms = c("corr_probe_e", "sequence_position.x"))

#Paper figures

#Plot 1 Memory by sequence position
position_mem_err <- position_memory %>% 
  dplyr::group_by(sequence_position.x) %>% 
  dplyr::summarise(err = std.error(mem))
avg_position_memory <- avg_position_memory %>% 
  mutate(err =position_mem_err$err )
avg_position_memory$sequence_f <- factor(avg_position_memory$sequence_position.x, levels = c("X", "A", "B"))
position_memory$sequence_f <- factor(position_memory$sequence_position.x, levels = c("X", "A", "B"))
ggplot()+
  geom_bar(data = avg_position_memory, stat="identity", aes(x = sequence_f, y= avg_mem, color = sequence_f), fill = "white", size= 1.5)+
  geom_errorbar(data = avg_position_memory, aes(x = sequence_f, ymin = avg_mem-err, ymax = avg_mem+err),width = 0, size= 1.5)+
  geom_jitter(data = position_memory, aes(x = sequence_f, y= mem, color = sequence_f), width = 0.1, alpha = .2)+
  geom_segment(aes(x = 1, y = .95, xend = 2, yend = .95))+
  annotate("text", label = '*',x=1.5, y=.97, size = 10)+
  scale_color_manual(values = c(A = "#920000",
                                B = "#009292",
                                X = "#999999"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black", size = 1.5),
        plot.margin = margin(10, 30, 10, 10),axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))+
  xlab("Image Type")+
  ylab("Memory Hit Rate")


#Plot2 Interaction Effect Model Probe vs. Memory
summary(tradeOff_mod_probe<-glmer(corrTest~corr_probe_e*sequence_position.x+(1+corr_probe_e|participant), family = "binomial", data = predicting_data, subset = (corrans=="old")))
avg_data_predict <- ggpredict(tradeOff_mod_probe, terms = c("corr_probe_e [-0.5:0.5 by=1]","sequence_position.x"), ci_level = .95)
avg_data_predict <- avg_data_predict %>% mutate(sequence_f = group)
avg_data_predict$sequence_f <- factor(avg_data_predict$sequence_f, levels = c("X", "A", "B"))

subj_dataA<-combined_data %>%
  filter(corrans=="old",sequence_position.x =="A") %>% 
  dplyr::group_by(participant, corr_probe_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataB<-combined_data %>%
  filter(corrans=="old",sequence_position.x =="B") %>% 
  dplyr::group_by(participant, corr_probe_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))
subj_dataX<-combined_data %>%
  filter(corrans=="old",sequence_position.x =="X") %>% 
  dplyr::group_by(participant, corr_probe_e) %>% 
  dplyr::summarise(hitRate = mean(corrTest))

ggplot()+
  geom_jitter(data =subj_dataA, aes(x=corr_probe_e-0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#920000")+
  geom_jitter(data =subj_dataX, aes(x=corr_probe_e+0.05,y=hitRate), width = 0.01, alpha = 0.25, color = "#999999")+
  geom_jitter(data =subj_dataB, aes(x=corr_probe_e,y=hitRate), width = 0.01, alpha = 0.25, color = "#009292")+
  geom_ribbon(data=avg_data_predict, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, fill = sequence_f), alpha=.2) + 
  geom_line(data=avg_data_predict, aes(x=x,y=predicted, color = sequence_f), size=2)+
  ylim(-0.001,1.001)+
  scale_x_continuous(breaks = c(-.5, .5), labels = c("Incorrect","Correct"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  theme(axis.line = element_line(colour = "black", size = 1.5),
        axis.text=element_text(size=20),legend.text=element_text(size=15),legend.title=element_text(size=15),axis.title=element_text(size=20,face="bold"))+
  annotate("text", label = '*',x=0, y=.95, size = 10)+
  geom_point(aes(x = 0, y = .9), color = "black", shape = 13, size = 7) +
  scale_color_manual(values = c(A = "#920000",
                                B = "#009292",
                                X = "#999999"))+
  scale_fill_manual(values = c(A = "#920000",
                               B = "#009292",
                               X = "#999999"),guide = "none")+
  ylab("Memory Hit Rate")+
  xlab("Upcoming Category Accuracy")+
  labs(color="Image Type")+
  theme(
    legend.position = c(.7, .35),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )+coord_fixed(ratio = 1.5)

#Plot 3 Able to predict by sequence position
position_pred_err <- choose_predicting_data %>% 
  dplyr::group_by(sequence_position.x) %>% 
  dplyr::summarise(err = std.error(p_predict))
avg_predicting <- avg_predicting %>% 
  mutate(err =position_pred_err$err )
avg_predicting$sequence_f <- factor(avg_predicting$sequence_position.x, levels = c("X", "A", "B"))
choose_predicting_data$sequence_f <- factor(choose_predicting_data$sequence_position.x, levels = c("X", "A", "B"))
ggplot()+
  geom_bar(data = avg_predicting, stat="identity", aes(x = sequence_f, y= avg_p, color = sequence_f), fill = "white", size= 1.5)+
  geom_errorbar(data = avg_predicting, aes(x = sequence_f, ymin = avg_p-err, ymax = avg_p+err),width = 0, size= 1.5)+
  geom_jitter(data = choose_predicting_data, aes(x = sequence_f, y= p_predict, color = sequence_f), width = 0.1, alpha = .2)+
  geom_segment(aes(x = 1, y = .95, xend = 1.98, yend = .95))+
  annotate("text", label = '***',x=1.5, y=.97, size = 10)+
  geom_segment(aes(x = 2.02, y = .95, xend = 3, yend = .95))+
  annotate("text", label = '***',x=2.5, y=.97, size = 10)+
  scale_color_manual(values = c(A = "#920000",
                                B = "#009292",
                                X = "#999999"))+
  theme(legend.position = "none",panel.background = element_blank(),axis.line = element_line(colour = "black", size = 1.5),
        plot.margin = margin(10, 30, 10, 10),axis.text=element_text(size=20),axis.title=element_text(size=20,face="bold"))+
  xlab("Image Type")+
  ylab("p(Able to Predict)")

