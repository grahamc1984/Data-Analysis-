library(ProjectTemplate) 
load.project()
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyverse)


#test for duplicates
ifelse(nrow(subset(enrol_compl, duplicated("learner_id")))==0, "No Duplicates in Enrolment Data", "Duplicates in Enrolment Data")
ifelse(nrow(subset(arch_compl, duplicated("learner_id")))==0, "No Duplicates in Archetype Data", "Duplicates in Archetype Data")
ifelse(nrow(subset(leave_compl, duplicated("learner_id")))==0, "No Duplicates in Leavers Data", "Duplicates in Leavers Data")

#summarise the data - no. of archetype vs total enrolments 
arch_per_year=data.frame(c(1:7), c(nrow(cyber.security.1.archetype.survey.responses),nrow(cyber.security.2.archetype.survey.responses), nrow(cyber.security.3.archetype.survey.responses),nrow(cyber.security.4.archetype.survey.responses),nrow(cyber.security.5.archetype.survey.responses),nrow(cyber.security.6.archetype.survey.responses),nrow(cyber.security.7.archetype.survey.responses)))
arch_per_year


#add month & year columns to all enrolment sheets 
cyber.security.3.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.3.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
enrol_3=mutate(cyber.security.3.enrolments, month=month(cyber.security.3.enrolments$enrolled_at, label=TRUE),year=year(cyber.security.3.enrolments$enrolled_at) )
cyber.security.4.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.4.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
enrol_4=mutate(cyber.security.4.enrolments, month=month(cyber.security.4.enrolments$enrolled_at, label=TRUE), year=year(cyber.security.4.enrolments$enrolled_at))
cyber.security.5.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.5.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
enrol_5=mutate(cyber.security.5.enrolments, month=month(cyber.security.5.enrolments$enrolled_at, label=TRUE), year=year(cyber.security.5.enrolments$enrolled_at))
cyber.security.6.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.6.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
enrol_6=mutate(cyber.security.6.enrolments, month=month(cyber.security.6.enrolments$enrolled_at, label=TRUE), year=year(cyber.security.6.enrolments$enrolled_at))
cyber.security.7.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.7.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
enrol_7=mutate(cyber.security.7.enrolments, month=month(cyber.security.7.enrolments$enrolled_at, label=TRUE), year=year(cyber.security.7.enrolments$enrolled_at))
enrol_compl=rbind(enrol_3, enrol_4, enrol_5, enrol_6, enrol_7)
ifelse(nrow(subset(enrol_compl, duplicated("learner_id")))==0, "No Duplicates in Enrolment Data", "Duplicates in Enrolment Data")
#add month & year columns to all archetype sheets 
cyber.security.3.archetype.survey.responses$responded_at=as.POSIXct(strptime(cyber.security.3.archetype.survey.responses$responded_at, format = ("%Y-%m-%d %H:%M:%S")))
arch_3=mutate(cyber.security.3.archetype.survey.responses, month=month(cyber.security.3.archetype.survey.responses$responded_at, label=TRUE),year=year(cyber.security.3.archetype.survey.responses$responded_at) )
cyber.security.4.archetype.survey.responses$responded_at=as.POSIXct(strptime(cyber.security.4.archetype.survey.responses$responded_at, format = ("%Y-%m-%d %H:%M:%S")))
arch_4=mutate(cyber.security.4.archetype.survey.responses, month=month(cyber.security.4.archetype.survey.responses$responded_at, label=TRUE), year=year(cyber.security.4.archetype.survey.responses$responded_at))
cyber.security.5.archetype.survey.responses$responded_at=as.POSIXct(strptime(cyber.security.5.archetype.survey.responses$responded_at, format = ("%Y-%m-%d %H:%M:%S")))
arch_5=mutate(cyber.security.5.archetype.survey.responses, month=month(cyber.security.5.archetype.survey.responses$responded_at, label=TRUE), year=year(cyber.security.5.archetype.survey.responses$responded_at))
cyber.security.6.archetype.survey.responses$responded_at=as.POSIXct(strptime(cyber.security.6.archetype.survey.responses$responded_at, format = ("%Y-%m-%d %H:%M:%S")))
arch_6=mutate(cyber.security.6.archetype.survey.responses, month=month(cyber.security.6.archetype.survey.responses$responded_at, label=TRUE), year=year(cyber.security.6.archetype.survey.responses$responded_at))
cyber.security.7.archetype.survey.responses$responded_at=as.POSIXct(strptime(cyber.security.7.archetype.survey.responses$responded_at, format = ("%Y-%m-%d %H:%M:%S")))
arch_7=mutate(cyber.security.7.archetype.survey.responses, month=month(cyber.security.7.archetype.survey.responses$responded_at, label=TRUE), year=year(cyber.security.7.archetype.survey.responses$responded_at))
arch_compl=rbind(arch_3, arch_4, arch_5, arch_6, arch_7)
ifelse(nrow(subset(arch_compl, duplicated("learner_id")))==0, "No Duplicates in Archetype Data", "Duplicates in Archetype Data")
#add month & year columns to all leavers data 
cyber.security.3.leaving.survey.responses$left_at=as.POSIXct(strptime(cyber.security.3.leaving.survey.responses$left_at, format = ("%Y-%m-%d %H:%M:%S")))
leave_3=mutate(cyber.security.3.leaving.survey.responses, month=month(cyber.security.3.leaving.survey.responses$left_at, label=TRUE),year=year(cyber.security.3.leaving.survey.responses$left_at) )
cyber.security.4.leaving.survey.responses$left_at=as.POSIXct(strptime(cyber.security.4.leaving.survey.responses$left_at, format = ("%Y-%m-%d %H:%M:%S")))
leave_4=mutate(cyber.security.4.leaving.survey.responses, month=month(cyber.security.4.leaving.survey.responses$left_at, label=TRUE), year=year(cyber.security.4.leaving.survey.responses$left_at))
cyber.security.5.leaving.survey.responses$left_at=as.POSIXct(strptime(cyber.security.5.leaving.survey.responses$left_at, format = ("%Y-%m-%d %H:%M:%S")))
leave_5=mutate(cyber.security.5.leaving.survey.responses, month=month(cyber.security.5.leaving.survey.responses$left_at, label=TRUE), year=year(cyber.security.5.leaving.survey.responses$left_at))
cyber.security.6.leaving.survey.responses$left_at=as.POSIXct(strptime(cyber.security.6.leaving.survey.responses$left_at, format = ("%Y-%m-%d %H:%M:%S")))
leave_6=mutate(cyber.security.6.leaving.survey.responses, month=month(cyber.security.6.leaving.survey.responses$left_at, label=TRUE), year=year(cyber.security.6.leaving.survey.responses$left_at))
cyber.security.7.leaving.survey.responses$left_at=as.POSIXct(strptime(cyber.security.7.leaving.survey.responses$left_at, format = ("%Y-%m-%d %H:%M:%S")))
leave_7=mutate(cyber.security.7.leaving.survey.responses, month=month(cyber.security.7.leaving.survey.responses$left_at, label=TRUE), year=year(cyber.security.7.leaving.survey.responses$left_at))
leave_compl=rbind(leave_3, leave_4, leave_5, leave_6, leave_7)
ifelse(nrow(subset(leave_compl, duplicated("learner_id")))==0, "No Duplicates in Leavers Data", "Duplicates in Leavers Data")




#spread step data 
View(spread(cyber.security.2.step.activity, "step", "last_completed_at"))

#join data for each year 
enrol_arch_7=left_join(cyber.security.7.enrolments, cyber.security.7.archetype.survey.responses, by="learner_id" )

#convert to date/time 
enrol_arch_7$enrolled_at=as.POSIXct(strptime(enrol_arch_7$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
#create new month column for enrolment date 
library(lubridate)
enrol_arch_7_plot=mutate(enrol_arch_7, month=month(enrol_arch_7$enrolled_at, label=TRUE))
#remove N/A from archetype 
library(tidyverse)
clean_enrol=enrol_arch_7_plot %>% 
  drop_na(archetype)
#produce plot 
library(ggplot2)
ggplot(data=clean_enrol)+geom_bar(aes(x=month, fill=factor(clean_enrol$archetype)))
#produce by archetype summary 
clean_enrol %>% 
  group_by(month, archetype) %>% 
  summarise(count=n())


