
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

#test for duplicate learner ID's
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

#test for duplicate learner IDs's
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

#test for duplicate learner ID's
ifelse(nrow(subset(leave_compl, duplicated("learner_id")))==0, "No Duplicates in Leavers Data", "Duplicates in Leavers Data")

#join enrolment, leaving and archetype datasets 
enrol_leave_compl=left_join(enrol_compl, leave_compl, by="learner_id")
enrol_leave_arch_compl=left_join(enrol_leave_compl, arch_compl, by="learner_id")

#remove archetype = na 
enrol_leave_arch=enrol_leave_arch_compl %>% 
  drop_na(archetype)

#combine all step data 
step_data=rbind(cyber.security.3.step.activity, cyber.security.4.step.activity, cyber.security.5.step.activity, cyber.security.6.step.activity, cyber.security.7.step.activity)

#test for duplicate Learner ID's
ifelse(nrow(subset(step_data, duplicated("learner_id")))==0, "No Duplicates in Step Data", "Duplicates in Step Data")

#join Step_data with arch_comp
step_arch_compl=left_join(step_data, arch_compl, by="learner_id")
step_arch=step_arch_compl %>% 
  drop_na(archetype)

