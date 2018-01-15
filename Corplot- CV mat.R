#corrplot
Matrix <- cor(PLITpstat)
summary(PLITpstat)
colnames(PLITpstat_2)
mixedCor(data=PLITpstat_2[,c(c(53,52,16,18,5,2),c(6,8,12,13,50,51,26:49),c(7,11,54))])
mixed.cor(PLITpstat_2,c=c(6,8,12,13,50,51,26:49),p=c(53,52,16,18,5,2),d=c(7,11,54))
str(PLITpstat_2$Injury_stat)
#Types of injuries according to the position of player
table(PLITpstat$location_name)
table(PLITpstat$position_name)

colnames(PLITpstat)
qplot(x=Position, data=PLITpstat_2,facets=~Ilocation,fill=Position) 
  

ggplot(data=PLITpstat_2, aes(x=Position, y=Ilocation))

PLITpstat_2[PLITpstat_2$position_name %in% c("Starter","Reliever","P"),"Position"] <- "P"
PLITpstat_2[PLITpstat_2$position_name %in% c("Catcher","Center Outfield","IF",
                                             "Left Outfield","OF","Outfield","Right Outfield",
                                             "Shortstop"),"Position"] <- "F"
PLITpstat_2[PLITpstat_2$position_name %in% c("First Base","Second Base",
                                             "Third Base"),"Position"] <- "R"

PLITpstat_2[PLITpstat_2$position_name=="Designated Hitter","Position"] <- "H"
table(PLITpstat_2$Position)
PLITpstat_2$Position <- as.factor(PLITpstat_2$Position)
PLITpstat_2[PLITpstat_2$location_name %in% c("Wrist","Tricep","Triceps","Trap",
                                             "Thumb","Shoulder","Rotator Cuff",
                                             "Hand","Forearm","Finger","Flexor",
                                             "Flexor Strain","Elbow inflammation","Elbow",
                                             "Arm","Biceps"),"Ilocation"] <- "Upper extremity"
PLITpstat_2[PLITpstat_2$location_name %in% c("Achilles","Ankle","Blister",
                                             "Calf","Fibula","Hamstring",
                                             "Foot","Groin","Heel","Hip",
                                             "Knee","Lat","Leg",
                                             "Quadriceps","Shin","Thigh","Toe"),"Ilocation"] <- "Lower extremity"
PLITpstat_2[PLITpstat_2$location_name %in% c("Abdominal","Hernia","Oblique"),"Ilocation"] <- "Abdomen"
PLITpstat_2[PLITpstat_2$location_name %in% c("Chin","Eye","Ear","Throat","Mouth",
                                             "Face","Head","Concussion",
                                             "Possible Concussion"),"Ilocation"] <- "Head and Face"
PLITpstat_2[PLITpstat_2$location_name %in% c("Back","Chest","Intercostal",
                                             "Neck","Pectoral","Ribs",
                                             "Side","Spine","Upper Body"),"Ilocation"] <- "Chest Neck Back"
PLITpstat_2[PLITpstat_2$location_name %in% c("Undisclosed","Shingles","Illness",
                                             "Food Poisoning","Flu","Fatigue"),"Ilocation"] <- "General"
PLITpstat_2[PLITpstat_2$location_name %in% c("Coach's Decision","Disciplinary","Personal",
                                             "Poss.Suspension","Suspension","Rest"),"Ilocation"] <- "Noninjury"
PLITpstat_2$Ilocation <- as.factor(PLITpstat_2$Ilocation)

#removing heathy vs injured
PLhealth <- PLITpstat_2[PLITpstat_2$Injury_stat==0,]

#Descriptive Statistics
summary(PLITpstat_2[,c(53,52,16,18,5,2)])


#Total observation for 2016 players and their stat - 2256
# Total no of players -1388
length(unique(PLITpstat_2$player))
#No of players who have injury- 
#No of injuries- 1348

#percentage of players who get injuries in 2016-
#teams who got ax injuries

#stat of injured players

#Descriptive stat-
colnames(PLITpstat_2)
CVstat<- stat.desc(PLITpstat_2[,c(6,8,12,13,50,51)])
Bats<- table(PLITpstat_2$bats)
handedness <- table(PLITpstat_2$handedness)
InjurL <-table(PLITpstat_2$Position,PLITpstat_2$Ilocation)
Highschool <- table(PLITpstat_2$high_school,PLITpstat_2$Injury_stat)
School <- table(PLITpstat_2$school,PLITpstat_2$Injury_stat)

#Analysis
J<- lm(PLITpstat_2$Injury_stat~PLITpstat_2$humanized_salary)
summary(J)
plot(J)







sns.barplot(x="", y="survived", hue="class", data=titanic)

length(unique(PLhealth$player_id))
dim(PLIT_2)




 

