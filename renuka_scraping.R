colnames(renuka_baseball) <- c("2016","2017")
nrow(renuka_baseball)
renuka_baseball$`2016`

library(stringr)
renuka_baseball$`2016` <-  str_trim(renuka_baseball$`2016`)
renuka_baseball$`2017` <- str_trim(renuka_baseball$`2017`)
renuka_baseball <- renuka_baseball[,1:2]
require(XML)
require(scrapeR)
country <- "Dominican Republic"
year <- "2016" 
cob <- function(country,year)
{
  ifelse(grepl(" ",country)==TRUE,
         {m <- unlist(str_split(country," "))
         webpage <- getURL(paste("http://www.baseball-almanac.com/players/birthplace.php?loc=",m[1],
                                 "%20", m[2],"&y=",year,sep = ""))
         },
          {webpage <- getURL(paste("http://www.baseball-almanac.com/players/birthplace.php?loc=",
                                   country,"&y=",year,sep = ""))
          }
         )
  # convert the page into a line-by-line format rather than a single string
  tc <- textConnection(webpage) # outputs an R character vector
  webpage <- readLines(tc) # read the web site as a collection of lines
  close(tc) # close website
  # Making the scraping parameterized that allows a data scientist to select search parameters 
  #-- website uses GET requests.
  pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)

  # xpathApply
Name<- as.data.frame(unlist(xpathApply(pagetree, "//*/table[@class = 'boxed']/tr/td[1]/a",xmlValue)))
 Name$country <- country
 colnames(Name)[1] <- "player" 
 return(unique(Name))
}
cob2016 <- do.call("rbind",lapply(renuka_baseball$`2016`[1:22],function(x) cob(x,"2016")))
cob2017 <- do.call("rbind",lapply(renuka_baseball$`2017`, function(x) cob(x,"2017")))
 
cob2016[cob2016$country=="Virgin Island","country"] <- "Virgin Islands"
write.csv(cob2016,file ="cob2016.csv")
#remove "&A" from excel sheet and import the dataset again!
cob2016 <- cob2016[,1:2]
Sys.getlocale()
#Removing encoding problems
cob2016$player <- str_replace(cob2016$player,"\xca"," ")
colnames(cob2016)[2] <- "cob"
PL16_1 <- join(PL16P,cob2016)
PL16_1[is.na(PL16_1$cob),"cob"] <- "USA"
table(PL16_1$player)


# exporting player ranking and stat data

set_token('c96b55e55080b317913d10e8ef0565cd')
league <- "mlb"

sport <- "baseball"

ep <- "Player_Season_Stats"

q_body <- list(season_id="mlb-2016")

PSS<- ss_get_result(sport = sport, league = league, ep = ep, query
                       = q_body, version = 1, walk = TRUE)
PSS[[6]]

SeasonStat16<- do.call("rbind", lapply(PSS, function(x) x$seasons))
LeagueStat16<- do.call("rbind", lapply(PSS, function(x) x$leagues))
TeamStat16<- do.call("rbind", lapply(PSS, function(x) x$teams))
PlayerStat16 <- do.call("rbind", lapply(PSS, function(x) x$player_season_stats))
ConferenceStat16 <- do.call("rbind", lapply(PSS, function(x) x$conference))
divisionStat16 <- do.call("rbind", lapply(PSS, function(x) x$division))
write.csv(PlayerStat16,file="PlayerStat16")

set_token('c96b55e55080b317913d10e8ef0565cd')
ep <- "Players"

q_body <- list(season_id="mlb-2017")

Ply17 <- ss_get_result(sport = sport, league = league, ep = ep, query
                            = q_body, version = 1, walk = TRUE)
Play17<- do.call("rbind", lapply(Ply17, function(x) x$players))
Play17[!duplicated(Play17),]
team16<- do.call("rbind", lapply(Ply17, function(x) x$team))


PTIplayer$handedness<- as.factor(PTIplayer$handedness)
PTIplayer$bats <- as.factor(PTIplayer$bats)
PTIplayer$country <- as.factor(PTIplayer$country)
unique(PTI$location_name)
table(PTI$location_name)
PTI$location_name<- gsub("Ankle/Hip","Ankle",PTI$location_name)
PTI$location_name<- gsub("Back/Hip","Back",PTI$location_name)
PTI$location_name<- gsub("BicepsKnee","Biceps",PTI$location_name)
PTI$location_name<- gsub("Elbow/Shoulder","Elbow",PTI$location_name)
PTI$location_name<- gsub("Fingers","Finger",PTI$location_name)
PTI$location_name<- gsub("Flexor Tendon","Flexor",PTI$location_name)
PTI$location_name<- gsub("Forearm Strain","Forearm",PTI$location_name)
PTI$location_name<- gsub("Groin/Hip","Groin",PTI$location_name)
PTI$location_name<- gsub("Labrum","Shoulder",PTI$location_name)
PTI$location_name<- gsub("Hamstrimg","Hamstring",PTI$location_name)
PTI$location_name<- gsub("Hamstring/Groin","Hamstring",PTI$location_name)
PTI$location_name<- gsub("Hand contusion","Hand",PTI$location_name)
PTI$location_name<- gsub("Hip/Ankle","Hip",PTI$location_name)
PTI$location_name<- gsub("Virus","Illness",PTI$location_name)
PTI$location_name <- gsub("Intercostal Spasms","",PTI$location_name)
PTI$location_name <- gsub("Intercostal Strain","",PTI$location_name)
PTI$location_name <- gsub("Intercostal","Intercostal",PTI$location_name)
PTI$location_name<- gsub("Knees","Knee",PTI$location_name)
PTI$location_name<- gsub("Wrist/Forearm","Wrist",PTI$location_name)
PTI$location_name<- gsub("Wrist/Thumb","Wrist",PTI$location_name)
PTI$location_name<- gsub("Tricepss","Triceps",PTI$location_name)
PTI$location_name<- gsub("Trapeziusezius","Trapezius",PTI$location_name)
PTI$location_name<- gsub("Lat Strain","Lat",PTI$location_name)
PTI$location_name<- gsub("Left Shoulder","Shoulder",PTI$location_name)
PTI$location_name<- gsub("Tibia","Shin",PTI$location_name)
PTI$location_name<- gsub("Quadricepricep","Quadricep",PTI$location_name)
PTI$location_name<- as.factor(PTI$location_name)
sort(summary(PTI$location_name),decreasing = FALSE)
PTIclean<- PTI[complete.cases(PTI$location_name), ]

write.csv(PTIclean,file="PTIclean.csv")
sort(PTIclean$started_on<- as.Date(PTIclean$started_on, "%m/%d/%y"))
# Removing 2015 and 2014 data 
PTIclean<- PTIclean[PTIclean$started_on>"2016-01-01",]
# removing duplicate values 
PTInondup<- unique(PTIclean)
PTInondup<- PTIclean[!duplicated(PTIclean),]
write.csv(PTInondup,file="PTInondup.csv")
colnames(PTInondup)
#reorganizing Columns
PTIor <- PTInondup[,c(1,2,18,28,24,33,20,4,34,9,16,8,3,11,21,15,17,14,10,22,23,29,30,31,32)]
write.csv(PTIor,file="PTIor.csv")
summary(PTIor)
PTIor$location_name<- as.factor(PTIor$location_name)
PTIor[PTIor$location_name=='Intercostal("", "", "", "", "", "", "", "", "", "")',"location_name"] <- "Intercostal"
# correcting the "age" at the time of injury-this is approximate not exact value
PTIor$birth_date<- mdy(PTIor$birth_date)
PTIor$age<- as.integer((PTIor$started_on-PTIor$birth_date)/365)
# Adding a column based on notes whether player needed surgery (1- surgery; 0-no surgery)
PTIor$surgery_status<- ifelse(grepl("surgery",PTIor$note),1,0)
table(PTIor$surgery_status)
#Creating data for 2016 only
PI2017<- PTIor[PTIor$started_on>"2017-01-01",]
PI2016 <- PTIor[ which(PTIor$started_on>'2016-01-01' & PTIor$started_on < "2017-01-01"),]
table(PI2016$player)
table(PI2016$started_on)
write.csv(PI2016,file="PI2016.csv")
write.csv(PI2017,file="PI2017.csv")
View(read.csv("PI2016.csv"))

# Descriptive Statistics on Variables
#Age
hist(PTIplayer$age)
summary(PTIplayer$age)
PTIplayer$age<- as.integer(PTIplayer$age)
table(PTIplayer$age)
#height and weight
summary(PTIplayer$height)
summary(PTIplayer$weight)
boxplot(PTIplayer$weight)
summary(PTIplayer)
which.max(table(PTI$location_name))
which.max(table(PTIclean$team_id))
TeamMaxI<- PTIclean[PTIclean$team_id=="f53d7ed3-1ebe-4e02-979d-6e51c0f5fb18",]
LADogd<- (unique(TeamMaxI))
sum(table(LADogd$location_name))
sort(table(LADogd$player))

colnames(PlayerStat16)


summary(PTI)
frequency(PTI$country)

unique(PTI$player)
install.packages("vcd")
library(vcd)
summary(PTI)
summary(PTI$country)
levels(PTI$country)
PTI$country
frequency(unique(PTI$player))
unique(PTI$player %in% PTI$country)
PTI$country <- as.factor(PTI$country)
summary(PTI$country)
Players$country <- as.factor(Players$country)
summary(Players$country)
unique(Players$player)

PlayerRest<-PTIclean[which (PTIclean$location_name==c("Rest","Suspension","Personal","Coach's Decision","Disciplinary","Poss. Suspension"),]


