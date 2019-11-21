
 
#use this code to calculate step data per arch 

step_data=rbind(cyber.security.3.step.activity, cyber.security.4.step.activity, cyber.security.5.step.activity, cyber.security.6.step.activity, cyber.security.7.step.activity)
ifelse(nrow(subset(step_data, duplicated("learner_id")))==0, "No Duplicates in Step Data", "Duplicates in Step Data")
step_arch_compl=left_join(step_data, arch_compl, by="learner_id")
step_arch=step_arch_compl %>% 
  drop_na(archetype)


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
  summarise("total progress"=sum(progress))

step_by_arch_2018=step_arch_prog_2018 %>%
   group_by(archetype, learner_id) %>% 
   summarise("total progress" = sum(progress))



  

str(Step_adv_2017)
ggplot(step_by_arch_2017, aes(x = as.factor(archetype), y = `total_progress`)) + geom_boxplot()
ggplot(step_by_arch_2018, aes(x = as.factor(archetype), y = `total_progress`)) + geom_boxplot()

boxplot(Advancers_2017, Fixers_2017)


a=step_arch_compl %>% 
  filter(learner_id=="b58e2472-4989-49cd-b7fc-1c75aaa99e65")

step_arch_prog_run=mutate(step_data_with_run, progress=as.numeric(ifelse(step_data_with_run$last_completed_at!="", "1", "0")))
steps_comp=left_join(step_arch_prog_run, arch_compl, by="learner_id")

steps_3=steps_comp %>% 
  drop_na(archetype) %>% 
  filter(run=="3") %>% 
  group_by(archetype, learner_id) %>% 
  summarise("total progress"=sum(progress))

steps_3 %>% 
  filter(`total progress`<=62)
