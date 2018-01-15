install.packages("devtools")
library(devtools)

install.packages("chron")
library(chron)
devtools::install_github("stattleship/stattleship-r")
library(stattleshipR)
set_token('c96b55e55080b317913d10e8ef0565cd')
league <- "mlb"

sport <- "baseball"

ep <- "Injuries"

q_body <- list(season_id="mlb-2016")

Injury<- ss_get_result(sport = sport, league = league, ep = ep, query
                         = q_body, version = 1, walk = TRUE)
set_token('c96b55e55080b317913d10e8ef0565cd')
league <- "mlb"

sport <- "baseball"

ep <- "injuries"

q_body <- list()

injuries17 <- ss_get_result(sport = sport, league = league, ep = ep, query
                          = q_body, version = 1, walk = TRUE)

injuries16 <- do.call("rbind", lapply(Injury, function(x) x$injuries)) 
players16 <- do.call("rbind",lapply(Injury,function(x) x$players))
Team <- do.call("rbind",lapply(Injury,function(x) x$teams))
Injury17 <- do.call("rbind",lapply(injuries17,function(x) x$injuries))
players17 <- do.call("rbind",lapply(injuries17,function(x) x$players))
teams17 <- do.call("rbind",lapply(injuries17,function(x) x$teams))
# Working on data from 2016 Injury-Teams
unique(Team$league_id)

colnames(Team)[1] <- "team_id"
colnames(Team)
unique(Teams$division_id)
Teams <- Team[,-c(2:7,9,11:13,15)]
colnames(Teams)
# formatting col Teams
Teams$location <- as.factor(Teams$location)
Teams$nickname <- as.factor(Teams$nickname)
write.csv(Teams,file="Teams.csv")

#Injury 2016 data clean up
colnames(injuries16)
unique(injuries16$season_id)
colnames(injuries16)[1] <- "injury_id"
Injury16 <- injuries16[,c(1,4,5,7,8,10,12)]

# formatting columns- Injury 16
#status-
Injury16$status <- as.factor(Injury16$status)
Injury16[Injury16$status %in% c("out","questionable","doubtful","probable"),"status"] <- "D1"
Injury16$status <- gsub("-day DL","",Injury16$status)
Injury16[Injury16$status %in% c("15","60","7"),"status"] <- paste("D",Injury16[Injury16$status %in% c("15","60","7"),"status"],sep = "") 

#location name
Injury16[Injury16$location_name %in% c("Abdomen","Abdomin","Abdominal Strain","Colon","Appendix","Ulcerative Colitis"),"location_name"] <- "Abdominal"
Injury16[Injury16$location_name %in% c("Bicep","Biceps/"),"location_name"] <- "Biceps"
Injury16$location_name<- gsub("Ankle/Hip","Ankle",Injury16$location_name)
Injury16$location_name<- gsub("Rib Cage","Ribs",Injury16$location_name)
Injury16$location_name<- gsub("Quad","Quadriceps",Injury16$location_name)
Injury16$location_name<- gsub("Back/Hip","Back",Injury16$location_name)
Injury16$location_name<- gsub("BicepsKnee","Biceps",Injury16$location_name)
Injury16$location_name<- gsub("Elbow/Shoulder","Elbow",Injury16$location_name)
Injury16$location_name<- gsub("Fingers","Finger",Injury16$location_name)
Injury16$location_name<- gsub("Flexor Tendon","Flexor",Injury16$location_name)
Injury16$location_name<- gsub("Forearm Strain","Forearm",Injury16$location_name)
Injury16$location_name<- gsub("Groin/Hip","Groin",Injury16$location_name)
Injury16$location_name<- gsub("Labrum","Shoulder",Injury16$location_name)
Injury16$location_name<- gsub("Hamstrimg","Hamstring",Injury16$location_name)
Injury16$location_name<- gsub("Hamstring/Groin","Hamstring",Injury16$location_name)
Injury16$location_name<- gsub("Hand contusion","Hand",Injury16$location_name)
Injury16$location_name<- gsub("Hip/Ankle","Hip",Injury16$location_name)
Injury16$location_name<- gsub("Virus","Illness",Injury16$location_name)
Injury16[grep("Intercostal",Injury16$location_name),"location_name"] <- "Intercostal"
Injury16$location_name<- gsub("Knees","Knee",Injury16$location_name)
Injury16$location_name<- gsub("Wrist/Forearm","Wrist",Injury16$location_name)
Injury16$location_name<- gsub("Wrist/Thumb","Wrist",Injury16$location_name)
Injury16$location_name<- gsub("Tricepss","Triceps",Injury16$location_name)
Injury16$location_name<- gsub("Trapeziusezius","Trapezius",Injury16$location_name)
Injury16$location_name<- gsub("Lat Strain","Lat",Injury16$location_name)
Injury16$location_name<- gsub("Left Shoulder","Shoulder",Injury16$location_name)
Injury16$location_name<- gsub("Tibia","Shin",Injury16$location_name)
Injury16$location_name<- gsub("Quadricepricep","Quadricep",Injury16$location_name)
Injury16[grep("Bicep",Injury16$location_name),"location_name"] <- "Biceps"
Injury16[grep("Shoulder/",Injury16$location_name),"location_name"] <- "Shoulder"
Injury16[grep("Neck/",Injury16$location_name),"location_name"] <- "Neck"
Injury16[grep("Quad",Injury16$location_name),"location_name"] <- "Quadriceps"
Injury16$location_name<- as.factor(Injury16$location_name)
table(Injury16$location_name)
# started on
Injury16$started_on <- ymd(Injury16$started_on)
Injury16only<- (Injury16[year(Injury16$started_on)==2016,])
View(Injury16only)
# Adding a column based on notes whether player needed surgery (1- surgery; 0-no surgery)
Injury16only$surgery_status<- ifelse(grepl("surgery",Injury16only$note),1,0)
write.csv(Injury16only,file = "Injury16only.csv")

#Work on Players for 2016
View(players16)

PL16<- players16[,c(1,5,6,9,10,14,15,16,17,18,19,24,25,28,35,36,39)]
colnames(PL16)[1] <- "player_id"
PL16$bats <- as.factor(PL16$bats)
PL16$birth_date <- ymd(PL16$birth_date)
PL16$player <- paste(PL16$first_name,PL16$last_name)
PL16$handedness <- as.factor(PL16$handedness)
PL16$humanized_salary<- as.numeric(gsub(",","",PL16$humanized_salary))
PL16$position_name <- as.factor(PL16$position_name)
PL16$pro_debut <- ymd(PL16$pro_debut)
colnames(PL16)
PL16P<- PL16[,-c(6,11)]
write.csv(PL16P, file="PL16P.csv")

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
View(PL16_1)
PL16_2<- PL16_1[,-4]
PL16_3 <- (unique(PL16_2))
# Join Teams Injury and Player
unique(PL16_3$player_id)
colnames(Teams)
unique(Injury16only$player_id)
PLI_1<- join(PL16_3,Injury16only)
Team1<- unique(Teams)
PLIT_1<- join(PLI_1,Team1)
colnames(PLIT_1)
PLIT_2 <- PLIT_1[,c(1,14,25,24,15,3,6,12,16,7,11,8,9,5,2,4,10,13,17:19,21:23,20)]
PLIT_2[is.na(PLIT_2$surgery_status)==TRUE,"surgery_status"] <- 0
unique(PLIT_2$player_id)
write.csv(PLIT_2,file="PLIT_2.csv")


