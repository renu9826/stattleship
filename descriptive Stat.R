summary(PLITpstat_2)
#No of obs- 2256- all players16
# No of Injured players Unique-756
#Most Injuries - Gerald Posey- 17 records
#Team with Most injuries-Dodgers -100 records.
which.max(table(PLIT_2$nickname))
View(PLIT_2)
length(unique(PLIT_2$player_id))
which.max(table(PLIT_2$player))
PLIT_2$age <- 2016-year(PLITpstat$birth_date)
PLIT_2$BMI <- (PLIT_2$weight*0.45)/((PLIT_2$height*0.025)^2)
hist(PLIT_2$age,main= "Age", xlab="Age of Injured players", ylab="Frequency", col= c("red","pink"))
hist(PLIT_2$BMI,main= "BMI", xlab="BMI of Injured players", ylab="Frequency", col= c("red","yellow"))
barplot(PLIT_2$years_of_experience,main= "Years of Experience", xlab="Player", ylab="years")
View(PLhealth)
hist(PLhealth$age,main= "Age", xlab="Age of Healthy players", ylab="Frequency", col= c("red","pink"))
hist(PLhealth$BMI,main= "BMI", xlab="BMI of Healthy players", ylab="Frequency", col= c("red","yellow"))
barplot(PLhealth$years_of_experience,main= "Years of Experience", xlab="Healthy Player", ylab="years")

(stat.desc(PLIT_2$BMI)
stat.desc(PLIT_2$years_of_experience)
stat.desc(PLhealth$BMI)
stat.desc(PLhealth$years_of_experience)
which.max(table(PLhealth$team_id))

(table(PLITpstat_2$Ilocation)/sum(table(PLITpstat_2$Ilocation)))    
