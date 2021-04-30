# Getting Data ready
raw = read.csv("cohort.csv")
library(dplyr)

rawselect = raw %>% select(Demographic, DBN, Cohort, Total.Cohort, 
                           Total.Grads...n, Total.Grads.....of.cohort,
                           Total.Regents...n, Total.Regents.....of.cohort,
                           Total.Regents.....of.grads,
                           Dropped.Out.....of.cohort) %>% 
                           filter(Total.Regents...n !="s" & Cohort == 2005)

colnames(rawselect) = c("Demographic", "DBN", "Year", "Cohortn", "Gradsn",
                        "GradsPerc","Regentsn","RegentsPercofCohort",
                        "RegentsPercofGrad","DropOutPerc")
main = rawselect %>% select(Demographic, DBN, Year,
                            Gradsn, GradsPerc,
                            Regentsn, RegentsPercofCohort, RegentsPercofGrad,
                            DropOutPerc) %>%
                            mutate(Borough = substr(DBN,3,3)) %>%
                            mutate(District = substr(DBN,1,2)) %>%
                            mutate(Cohortn = as.numeric(rawselect$Cohortn))
glimpse(main)

#******************************************************************

ClassType = c("General Education Students","Special Education Students")
Race = c("Asian","Black","White","Hispanic")
Gender = c("Female","Male")
Language = c("English Language Learners","English Proficient Students")

mainClassType = main %>% filter(Demographic %in% ClassType)
mainRace = main %>% filter(Demographic %in% Race)
mainGender = main %>% filter(Demographic %in% Gender)
mainLanguage = main %>% filter(Demographic %in% Language)

##new models
#ModelRace1
modelRace1 = lm(GradsPerc~RegentsPercofGrad, data = mainRace)
modelRace1
summary(modelRace1)
GPP = predict(modelRace, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelRace2
modelRace2 = lm(GradsPerc~DropOutPerc, data = mainRace)
modelRace2
summary(modelRace2)
GPP = predict(modelRace2, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelRace3
modelRace3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainRace)
modelRace3
summary(modelRace3)
GPP = predict(modelRace3, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType1
modelClassType1= lm(GradsPerc~RegentsPercofGrad, data = mainClassType)
modelClassType1
summary(modelClassType1)
GPP = predict(modelClassType, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType2
modelClassType2 = lm(GradsPerc~DropOutPerc, data = mainClassType)
modelClassType2
summary(modelClassType2)
GPP = predict(modelClassType2, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType3
modelClassType3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainClassType)
modelClassType3
summary(modelClassType3)
GPP = predict(modelClassType3, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender1
modelGender1 = lm(GradsPerc~RegentsPercofGrad, data = mainGender)
modelGender1
summary(modelGender1)
GPP = predict(modelGender, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender2
modelGender2 = lm(GradsPerc~DropOutPerc, data = mainGender)
modelGender2
summary(modelGender2)
GPP = predict(modelGender2, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender3
modelGender3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainGender)
modelGender3
summary(modelGender3)
GPP = predict(modelGender3, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage1
modelLanguage1= lm(GradsPerc~RegentsPercofGrad, data = mainLanguage)
modelLanguage1
summary(modelLanguage1)
GPP = predict(modelLanguage1, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage2
modelLanguage2 = lm(GradsPerc~DropOutPerc, data = mainLanguage)
modelLanguage2
summary(modelLanguage2)
GPP = predict(modelLanguage2, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage3
modelLanguage3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainLanguage)
modelLanguage3
summary(modelLanguage3)
GPP = predict(modelLanguage3, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage.Dummy
modelLanguage.Dummy = lm(GradsPerc~-1,data = mainLanguage)
modelLanguage.Dummy
summary(modelLanguage.Dummy)
GPP = predict(modelLanguage.Dummy, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelRace.Dummy
modelRace.Dummy = lm(GradsPerc~-1,data = mainRace)
modelRace.Dummy
summary(modelRace.Dummy)
GPP = predict(modelRace.Dummy, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType.Dummy
modelClassType.Dummy = lm(GradsPerc~-1,data = mainClassType)
modelClassType.Dummy
summary(modelClassType.Dummy)
GPP = predict(modelClassType.Dummy, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender.Dummy
modelGender.Dummy = lm(GradsPerc~-1,data = mainGender)
modelGender.Dummy
summary(modelGender.Dummy)
GPP = predict(modelGender.Dummy, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

##new models
#ModelRace.T1
modelRace.T1 = rpart(GradsPerc~RegentsPercofGrad, data = mainRace)
GPP = predict(modelRace.T1, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelRace.T2
modelRace.T2 = rpart(GradsPerc~DropOutPerc, data = mainRace)
GPP = predict(modelRace.T2, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelRace.T3
modelRace.T3 = rpart(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainRace)
modelRace.T3
summary(modelRace.T3)
GPP = predict(modelRace.T3, newdata = mainRace)
mean((mainRace$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType.T1
modelClassType.T1= rpart(GradsPerc~RegentsPercofGrad, data = mainClassType)
GPP = predict(modelClassType.T1, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType.T2
modelClassType.T2 = rpart(GradsPerc~DropOutPerc, data = mainClassType)
GPP = predict(modelClassType.T2, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelClassType.T3
modelClassType.T3 = rpart(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainClassType)
GPP = predict(modelClassType.T3, newdata = mainClassType)
mean((mainClassType$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender.T1
modelGender.T1 = rpart(GradsPerc~RegentsPercofGrad, data = mainGender)
GPP = predict(modelGender.T1, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender.T2
modelGender.T2 = rpart(GradsPerc~DropOutPerc, data = mainGender)
GPP = predict(modelGender.T2, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelGender.T3
modelGender.T3 = rpart(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainGender)
GPP = predict(modelGender.T3, newdata = mainGender)
mean((mainGender$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage.T1
modelLanguage.T1= rpart(GradsPerc~RegentsPercofGrad, data = mainLanguage)
modelLanguage.T1
summary(modelLanguage.T1)
GPP = predict(modelLanguage.T1, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage.T2
modelLanguage.T2 = rpart(GradsPerc~DropOutPerc, data = mainLanguage)
GPP = predict(modelLanguage.T2, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)

#ModelLanguage.T3
modelLanguage.T3 = rpart(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainLanguage)
GPP = predict(modelLanguage.T3, newdata = mainLanguage)
mean((mainLanguage$GradsPerc-GPP)^2, na.rm = TRUE)


#**************
ggplot(mainRace) +
  aes(x = RegentsPercofGrad, y = GradsPerc, color = Demographic) +
  geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
  facet_grid(Demographic ~ .) +
  ggtitle("Graduation Rate by Percentage of Students Passing Regent Exams")

ggplot(mainClassType) +
  aes(x = RegentsPercofGrad, y = GradsPerc, color = Demographic) +
  geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
  facet_grid(Demographic ~ .) +
  ggtitle("Graduation Rate by Percentage of Students Passing Regent Exams")

ggplot(mainLanguage) +
  aes(x = RegentsPercofGrad, y = GradsPerc, color = Demographic) +
  geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
  facet_grid(Demographic ~ .) +
  ggtitle("Graduation Rate by Percentage of Students Passing Regent Exams")

ggplot(mainGender) +
  aes(x = RegentsPercofGrad, y = GradsPerc, color = Demographic) +
  geom_point(alpha = 0.6)+geom_smooth(method = "lm")+
  facet_grid(Demographic ~ .) +
  ggtitle("Graduation Rate by Percentage of Students Passing Regent Exams")

##t-test

#ModelRace3
modelRace3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainRace)
GPP = predict(modelRace3, newdata = mainRace)
t.test(mainRace$GradsPerc, GPP, data = mainRace)


#ModelClassType3
modelClassType3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainClassType)
GPP = predict(modelClassType3, newdata = mainClassType)
t.test(mainRace$GradsPerc, GPP, data = mainClassType)

#ModelLanguage3
modelLanguage3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainLanguage)
GPP = predict(modelLanguage3, newdata = mainLanguage)
t.test(mainRace$GradsPerc, GPP, data = mainLanguage)

#ModelGender3
modelGender3 = lm(GradsPerc~RegentsPercofGrad+DropOutPerc, data = mainGender)
GPP = predict(modelGender3, newdata = mainGender)
t.test(mainRace$GradsPerc, GPP, data = mainGender)