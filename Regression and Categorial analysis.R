


InjuryMat<- as.matrix(PLITpstat_2[,c(6,8,12,13,50,51,34,26,48,49,54,23)])
M <- cor(PLITpstat_2[
  complete.cases(PLITpstat_2[,c(6,8,12,13,50,51,34,26,48,49,54,23)]),c(6,8,12,13,50,51,34,26,48,49,54,23)])
corrplot(M, method="circle")
colnames(PLITpstat_2)
#linear regression - Coorelation between variables that were significant in corrplot and injury status
#First Model
I<- glm(PLITpstat_2$Injury_stat~PLITpstat_2$years_of_experience + 
          PLITpstat_2$humanized_salary +
          PLITpstat_2$age + PLITpstat_2$BMI + PLITpstat_2$pitches_thrown 
+ PLITpstat_2$pitcher_games_played +
            PLITpstat_2$fielder_games_played + 
  PLITpstat_2$hitter_games_played,family=binomial(link='logit'))
summary(I)
R2 <- 1 - (sum((PLITpstat_2$Injury_stat-predict(I))^2)/
             sum((PLITpstat_2$Injury_stat-mean(PLITpstat_2$Injury_stat))^2))
plot(I,dat,col=cols_t1,pch=16)

#select the colors that will be used
library(RColorBrewer)
#all palette available from RColorBrewer
display.brewer.all()
#we will select the first 4 colors in the Set1 palette
cols<-brewer.pal(n=8,name="Set2")
cols_t1<-cols[PLITpstat_2$Injury_stat]
plot(I,col=cols_t1,pch=17)

#Second Model
K<- glm(PLITpstat_2$Injury_stat~PLITpstat_2$years_of_experience + 
      PLITpstat_2$humanized_salary +
      PLITpstat_2$age + PLITpstat_2$BMI + PLITpstat_2$pitches_thrown, 
    family=binomial(link='logit'))
summary(K)
R3 <- 1 - (sum((PLITpstat_2$Injury_stat-predict(K))^2)/
             sum((PLITpstat_2$Injury_stat-mean(PLITpstat_2$Injury_stat))^2))

#Third Model
L<- glm (PLITpstat_2$Injury_stat~PLITpstat_2$years_of_experience + PLITpstat_2$BMI, 
    family=binomial(link='logit'))
summary(L)
R4 <- 1 - (sum((PLITpstat_2$Injury_stat-predict(L))^2)/
             sum((PLITpstat_2$Injury_stat-mean(PLITpstat_2$Injury_stat))^2))


# Categorical Variable
colnames(PLITpstat_2)
#Bat
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$bats)
PLITpstat_2$Injury_stat <- as.factor(PLITpstat_2$Injury_stat)
PLITpstat_2$bats <- as.factor(PLITpstat_2$bats)

Bat <- CrossTable(PLITpstat_2$Injury_stat,PLITpstat_2$bats,digits = 2)
InjuryBat<-chisq.test(rbind(table(PLITpstat_2$Injury_stat,PLITpstat_2$bats)))
barplot(InjuryBat,col=heat.colors(length(rownames(InjuryBat))), width=2)
legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(InjuryBat))), legend=rownames(InjuryBat))
#Handedness
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$handedness)
Hand<- CrossTable(PLITpstat_2$Injury_stat,PLITpstat_2$handedness,digits = 2)
InjuryHand<- table(PLITpstat_2$Injury_stat,PLITpstat_2$handedness)
barplot(InjuryHand,col=brewer.pal(4,"Set3"), width=2)
# High school , School , Cob,cross table for position and injury location
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$high_school)
Highschool<- CrossTable(PLITpstat_2$Injury_stat,PLITpstat_2$high_school,digits = 2)
InjuryHighschool<- table(PLITpstat_2$Injury_stat,PLITpstat_2$high_school)
barplot(InjuryHighschool,col=rainbow_hcl(4), width=2)
#school
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$school)
school<- CrossTable(PLITpstat_2$Injury_stat,PLITpstat_2$school,digits = 2)
Injuryschool<- table(PLITpstat_2$Injury_stat,PLITpstat_2$school)
barplot(Injuryschool,col=rainbow_hcl(4), width=2)
#cob
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$Country)
Country<- CrossTable(PLITpstat_2$Injury_stat,PLITpstat_2$Country,digits = 2)
InjuryCountry<- table(PLITpstat_2$Injury_stat,PLITpstat_2$Country)
barplot(InjuryCountry,col=brewer.pal(4,"Set3"), width=2)
PLITpstat_2$Country <- (ifelse(PLITpstat_2$cob=="USA","USA","NonUSA"))
View(Country)
chisq.test(PLITpstat_2$Injury_stat,PLITpstat_2$Position)
#crosstable
InjuryPosition<- table(PLITpstat_2$Ilocation,PLITpstat_2$Position)
barplot(InjuryPosition,col=heat.colors(length(rownames(InjuryPosition))), width=2)

legend("topright",inset=c(-0.25,0), fill=heat.colors(length(rownames(InjuryPosition))), legend=rownames(PLITpstat_2$Ilocation))

# Scatter Plaots for all the continuous variables as Regression model provide only for years of experience

plot(PLITpstat_2$Injury_stat,PLITpstat_2$BMI,main= "InjuryStatus/BMI", xlab="BMI", ylab="InjuryStatus", col="red")
abline(glm(PLITpstat_2$Injury_stat~PLITpstat_2$BMI), col="blue")

plot(PLITpstat_2$Injury_stat,PLITpstat_2$years_of_experience,main= "InjuryStatus/Years of Experience", xlab="Years of Experience", ylab="InjuryStatus", col="red")
abline(glm(PLITpstat_2$Injury_stat~PLITpstat_2$years_of_experience), col="blue")