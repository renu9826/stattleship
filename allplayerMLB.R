# Stattleship data for all the players
set_token('c96b55e55080b317913d10e8ef0565cd')
ep <- "Players"

q_body <- list()

Ply17 <- ss_get_result(sport = sport, league = league, ep = ep, query
                       = q_body, version = 1, walk = TRUE)
Play17<- do.call("rbind", lapply(Ply17, function(x) x$players))
Play17[!duplicated(Play17),]
# On Stattleship 2016 and 2017 player data is together. 
# work on Play 17 
playall<- Play17[Play17$active== "TRUE",]
colnames(playall)
table(is.na(playall$last_name))
playall$player<- paste(playall$first_name,playall$last_name)
playall1<- playall[which(!duplicated(playall$player)==TRUE),]
colnames(playall1)
unique(playall1$player)

colnames(playall1)
playall2 <- playall1[,c(1,5,6,10,15:18,24:25,28,35:36,39:40)]
colnames(playall2)[1] <- "player_id"

playall2$pro_debut <- ymd(playall2$pro_debut)

playall3 <- playall3[which(year(playall3$pro_debut)!="2017"),]

write.csv(playall3,file="playall3.csv")
#adding country of birth for all the players
playall4 <- join(playall3,cob2016)
playall4[which(is.na(playall4$cob)==TRUE),"cob"] <- "USA"

# Formatting coulmns 
playall4$birth_date <- ymd(playall4$birth_date)
playall4$bats <- as.factor(playall4$bats)
playall4$cob <- as.factor(playall4$cob)
playall4$handedness <- as.factor(playall4$handedness)
playall4$height <- as.numeric(playall4$height)
playall4$position_name <- as.factor(playall4$position_name)

playall4$humanized_salary<- as.numeric(gsub(",","",playall4$humanized_salary))
playall4$weight <- as.numeric(playall4$weight)

playall4$years_of_experience <- as.numeric(playall4$years_of_experience)

write.csv(playall4,file="playall4.csv")

#combining all players with injuried players with all players
PLITplayer_1<- join(PLIT_2,playall4)
write.csv(PLITplayer_1,file="PLITplayer_1.csv")
PLITplayer_2 <- join(playall4,PLIT_2)
colnames(PLITplayer_2)
write.csv(PLITplayer_1,file="PLITplayer_2.csv")


# Getting Playerstats for 2016
colnames(PlayerStat16)
PS16_1<- PlayerStat16[,-c(2:5)]
PS16_2<- PS16_1[which(duplicated(PS16_1$player_id)==FALSE),]
write.csv(PS16_2,file="PS16_2.csv")
PS16_3 <- PS16_2[,c(4,6,8,9,10,20,36,56,62,63,65,70,74,75,80,82,86,105,118,124,125,130,135,138,141,142)]
write.csv(PS16_3,file="PS16_3.csv")
#Joining Stat with Players and Injury- 2016 all data
PLITpstat<- join(PLITplayer_2,PS16_3)
PLITpstat$age <- 2016-year(PLITpstat$birth_date)
PLITpstat$BMI <- (PLITpstat$weight*0.45)/((PLITpstat$height*0.025)^2)
PLITpstat$high_school<- ifelse(is.na(PLITpstat$high_school),"No","Yes")
PLITpstat$school <- ifelse(is.na(PLITpstat$school),"No","Yes")
write.csv(PLITpstat,file="PLITpstat.csv")
PLITpstat_2<- join(PLITplayer_2,PS16_3)
PLITpstat_2$age <- 2016-year(PLITpstat_2$birth_date)
PLITpstat_2$BMI <- (PLITpstat_2$weight*0.45)/((PLITpstat_2$height*0.025)^2)
# 0- no school ; 1-  school 
PLITpstat_2$high_school<- ifelse(is.na(PLITpstat_2$high_school),"No","Yes")
# 0- no school ; 1- undergrad school 
PLITpstat_2$school <- ifelse(is.na(PLITpstat_2$school),"No","Yes")
write.csv(PLITpstat_2,file="PLITpstat_2.csv")

PLITpstat_2$Ilocation <-""
PLITpstat_2$Position <- ""
PLITpstat_2$Injury_stat <- ifelse(is.na(PLITpstat_2$injury_id),0,1)
table(PLITpstat_1$Ilocation)
table(PLITpstat_2$Injury_stat)
