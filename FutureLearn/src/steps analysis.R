
library(ProjectTemplate)
load.project()

test_step=cyber.security.3.step.activity %>% 
  select(learner_id, step, last_completed_at)

test_step_2 =test_step%>% 
  mutate(test_step$step, step_char=as.character(test_step$step)) %>% 
  select(learner_id, step_char, last_completed_at) 

spread(test_step_2, test_step_2$step_char, test_step_2$last_completed_at)


test_step_2=mutate(test_step, progress=as.numeric(ifelse(test_step$last_completed_at!="", "1", "0")))

test_step_2 %>% 
  group_by(learner_id) %>% 
  summarise(sum(test_step_2$progress))

test_step_arch=left_join(test_step_2, arch_compl, by="learner_id")

nearly=test_step_arch %>% 
  drop_na(archetype) %>% 
  group_by(archetype, learner_id) %>% 
  summarise(sum(progress))

nearly_adv=nearly %>% 
  filter(archetype=="Advancers")

advancers=pull(nearly_adv, var=3)
summary(advancers)
boxplot(advancers)
 
ggplot(data=nearly)+geom_bar(aes(x=archetype))

nearly %>% 
  filter(archetype=="Advancers") 
 


step_data=rbind(cyber.security.3.step.activity, cyber.security.4.step.activity, cyber.security.5.step.activity, cyber.security.6.step.activity, cyber.security.7.step.activity)
ifelse(nrow(subset(step_data, duplicated("learner_id")))==0, "No Duplicates in Step Data", "Duplicates in Step Data")
step_arch_compl=left_join(step_data, arch_compl, by="learner_id")
step_arch=step_arch_compl %>% 
  drop_na(archetype)

#use this code to calculate step data per arch 
#create new column to count progress steps 
step_arch_prog=mutate(step_arch, progress=as.numeric(ifelse(step_arch$last_completed_at!="", "1", "0")))

#filter by year 
step_arch_prog_2017 = step_arch_prog%>%
  filter(year=="2017")
step_arch_prog_2018=step_arch_prog %>% 
  filter(year=="2018")


#group by arch then learner id to produce individual steps 
step_by_arch_2017=step_arch_prog_2017 %>% 
  group_by(archetype, learner_id) %>% 
  summarise(sum(progress))

# pull out vectors for each arch 
Step_adv_2017=step_by_arch_2017 %>% 
  filter(archetype=="Advancers")
Advancers_2017=pull(Step_adv_2017, var=3)
summary(Advancers_2017)

Step_fix_2017=step_by_arch_2017 %>% 
  filter(archetype=="Fixers")
Fixers_2017=pull(Step_fix_2017, var=3)
summary(Fixers_2017)


str(Step_adv_2017)
ggplot(step_by_arch_2017, aes(x = as.factor(archetype), y = `sum(progress)`)) + geom_boxplot()


boxplot(Advancers_2017, Fixers_2017)


a=step_arch_compl %>% 
  filter(learner_id=="b58e2472-4989-49cd-b7fc-1c75aaa99e65")

