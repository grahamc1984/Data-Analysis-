enrol_leave_compl=left_join(enrol_compl, leave_compl, by="learner_id")
enrol_leave_arch_compl=left_join(enrol_leave_compl, arch_compl, by="learner_id")

# grouping by archetype and year 
enrol_leave_arch_compl %>% 
  drop_na(archetype) %>% 
  group_by(archetype, year.x) %>% 
  summarise(count=n())

#grouping by archetype and age 
arch_age=enrol_leave_arch %>% 
  filter(age_range!="Unknown") %>% 
  group_by(archetype, age_range) %>% 
  summarise(count=n())

#grouping by archetype and gender 
arch_gender=enrol_leave_arch %>% 
  filter(gender!="Unknown") %>% 
  group_by(archetype, gender) %>% 
  summarise(count=n())

#grouping by archetype and country 
arch_country=enrol_leave_arch %>% 
  filter(detected_country!="--") %>% 
  group_by(archetype, detected_country) %>% 
  summarise(count=n())

#grouping by archetype and work status 
arch_empl_status=enrol_leave_arch %>% 
  filter(employment_status!="Unknown") %>% 
  group_by(archetype,employment_status) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))
ggplot(data=arch_empl_status)+geom_bar(aes(x=employment_status, y=n, fill = factor(arch_empl_status$archetype)),stat = "identity")

#grouping by archetype and work area 
arch_empl_area=enrol_leave_arch %>% 
  filter(employment_area!="Unknown") %>% 
  group_by(archetype, employment_area) %>% 
  summarise(count=n())

#relative proportion age by archetype
arch_age_prop=enrol_leave_arch %>% 
  filter(age_range!="Unknown") %>% 
  group_by(age_range, archetype) %>% 
  summarise(n=n()) %>% 
  mutate(freq=n/sum(n))

#age proportion 
ggplot(data=arch_age_prop)+geom_bar(aes(x=age_range, y=n, fill = factor(arch_age_prop$archetype)),stat = "identity")

maxage=arch_age_prop %>% 
  filter(age_range=="46-55") %>% 
  pull(var=-2)
 sum(maxage)

sum(arch_age_prop[,3])


V46_55=arch_age_prop %>% 
  filter(age_range=="46-55", archetype=="Vitalisers") %>% 
  pull(var=-1)
V65=arch_age_prop %>% 
  filter(age_range==">65", archetype=="Vitalisers") %>% 
  pull(var=-1)
E26_35=arch_age_prop %>% 
  filter(age_range=="26-35", archetype=="Explorers") %>% 
  pull(var=-1)
E36_45=arch_age_prop %>% 
  filter(age_range=="36-45", archetype=="Explorers") %>% 
  pull(var=-1)
  
round(V46_55*100,digits=1)
round(V65*100,digits=1)
round(E26_35*100,digits=1)
round(E36_45*100,digits=1)



#produce html table 
arch_age_prop %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "condensed", "striped")

arch_age_prop







#graphs

library(ggplot2)
#archetype by year 
ggplot(data=enrol_leave_arch)+geom_bar(aes(x=factor(year), fill=factor(archetype)))
#age by archteype
ggplot(data=arch_age)+geom_bar(aes(x=age_range, fill=factor(arch_age$archetype)))
#age proportion 
ggplot(data=arch_age_prop)+geom_bar(aes(x=age_range, y=n, fill = factor(arch_age_prop$archetype)),stat = "identity")

install.packages("reshape2")
library(reshape2)
melt.data=melt(arch_age_prop, )


round(nrow(enrol_leave_arch)/nrow(enrol_compl)*100, digits=2)

library(ProjectTemplate)
load.project()


arch_age_prop %>% 
  kable() %>% 
  kable_styling()

install.packages("kableExtra")
library(knitr)                                             
library(kableExtra)

sum(arch_empl_status$n)
