install.packages('ProjectTemplate')
library(ProjectTemplate)
create.project("FutureLearn")
load.project()
library(dplyr)
library(lubridate)
head(cyber.security.7.archetype.survey.responses)

#count number of variables by grouped observations 
cyber.security.7.archetype.survey.responses %>% 
  group_by(archetype)%>%
  summarise(Learners=n())

#join data tables 
testjoin=left_join(cyber.security.7.archetype.survey.responses, cyber.security.7.enrolments, by="learner_id")

#transform date columns 
cyber.security.7.enrolments$enrolled_at=as.POSIXct(strptime(cyber.security.7.enrolments$enrolled_at, format = ("%Y-%m-%d %H:%M:%S")))
cyber.security.7.enrolments$unenrolled_at=as.POSIXct(strptime(cyber.security.7.enrolments$unenrolled_at, format = ("%Y-%m-%d %H:%M:%S")))

#trying to subtract one date from another 
enrollength= drop_na(cyber.security.7.enrolments, unenrolled_at) 
as.Date(mutate(enrollength, length=enrollength$unenrolled_at-enrollength$enrolled_at))


#create a new column with the month 
enrol2=mutate(cyber.security.7.enrolments, month=month(cyber.security.7.enrolments$enrolled_at, label=TRUE))

#change to POSIXct format 
cyber.security.7.step.activity$first_visited_at=as.POSIXct(strptime(cyber.security.7.step.activity$first_visited_at, format = ("%Y-%m-%d %H:%M:%S")))
cyber.security.7.step.activity$last_completed_at=as.POSIXct(strptime(cyber.security.7.step.activity$last_completed_at, format=("%Y-%m-%d %H:%M:%S")))


#count the number of enrolments per month with new month column (enrol2 data)
enrolcount2=enrol2 %>% 
  group_by(enrol2$month) %>%
  summarise (count=n())
enrolcount2
#graph it
ggplot(data=enrol2, aes(x=month))+geom_bar()



#count with two groupings 
enrolcount2=testjoin %>% 
  group_by(month=floor_date(testjoin$enrolled_at, "month"), archetype) %>% 
  summarise(count=n())
enrolcount2











  