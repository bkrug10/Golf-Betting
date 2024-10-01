library(tidyverse)
library(readr)
library(rvest)
library(utils)
library(readxl)
library(ggplot2)
library(gridExtra)
library(formattable)
library(reshape)
library(dplyr)
Tournament = "Sanderson Farms"
Odds <- read_csv("Golf/golf-odds-rotowire.csv", skip=1) %>% 
  select(Golfer, Win, `Top 5`, `Top 20`, Yes)
CutLine <- nrow(Odds)/2

old <- c("Santiago de la Fuente", "S.H.  Kim", "Ludvig Aberg", "Nicolai Hojgaard", "Alexander Bjork", "Thorbjorn Olesen", "Rasmus Hojgaard")
new <- c("Santiago De la Fuente", "S.H. Kim", "Ludvig Åberg", "Nicolai Højgaard", "Alexander Björk", "Thorbjørn Olesen", "Rasmus Højgaard")
Odds$Golfer[Odds$Golfer %in% old] <- new[match(Odds$Golfer, old, nomatch = 0)]


TheSentry <- read_excel("Golf/PlayerStats.xlsx", sheet = "The Sentry")
SonyOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "Sony Open")
FarmersInsurance <- read_excel("Golf/PlayerStats.xlsx", sheet = "Farmers Insurance")
`AT&T` <- read_excel("Golf/PlayerStats.xlsx", sheet = "AT&T Pebble Beach")
WM <- read_excel("Golf/PlayerStats.xlsx", sheet = "WM Phoenix Open")
Genesis <- read_excel("Golf/PlayerStats.xlsx", sheet = "Genesis")
`Mexican Open` <- read_excel("Golf/PlayerStats.xlsx", sheet = "Mexican Open")
Cognizant <- read_excel("Golf/PlayerStats.xlsx", sheet = "Cognizant")
ArnoldPalmer <- read_excel("Golf/PlayerStats.xlsx", sheet = "Arnold Palmer")
RSM <- read_excel("Golf/PlayerStats.xlsx", sheet = "RSM Classic")
Shriners <- read_excel("Golf/PlayerStats.xlsx", sheet = "Shriners Open")
SandersonFarms <- read_excel("Golf/PlayerStats.xlsx", sheet = "Sanderson Farms")
Fortinet <- read_excel("Golf/PlayerStats.xlsx", sheet = "Fortinet")
TourChampionship <- read_excel("Golf/PlayerStats.xlsx", sheet = "Tour Championship")
BMW <- read_excel("Golf/PlayerStats.xlsx", sheet = "BMW")
FedexStJude <- read_excel("Golf/PlayerStats.xlsx", sheet = "Fedex St. Jude")
Wyndham <- read_excel("Golf/PlayerStats.xlsx", sheet = "Wyndham")
`3M` <- read_excel("Golf/PlayerStats.xlsx", sheet = "3M")
ScottishOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "Scottish Open")
Players <- read_excel("Golf/PlayerStats.xlsx", sheet = "Players")
Valspar <- read_excel("Golf/PlayerStats.xlsx", sheet = "Valspar")
HoustonOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "Houston Open")
TexasOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "Texas Open")
RBCHeritage <- read_excel("Golf/PlayerStats.xlsx", sheet = "RBC Heritage")
ByronNelson <- read_excel("Golf/PlayerStats.xlsx", sheet = "Byron Nelson")
WellsFargo <- read_excel("Golf/PlayerStats.xlsx", sheet = "Wells Fargo")
MyrtleBeach <- read_excel("Golf/PlayerStats.xlsx", sheet = "Myrtle Beach Classic")
PGAChampionship <- read_excel("Golf/PlayerStats.xlsx", sheet = "PGA Championship")
CharlesSchwab <- read_excel("Golf/PlayerStats.xlsx", sheet = "Charles Schwab")
CanadianOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "Canadian Open")
Memorial <- read_excel("Golf/PlayerStats.xlsx", sheet = "Memorial")
USOpen <- read_excel("Golf/PlayerStats.xlsx", sheet = "US Open")
Travelers <- read_excel("Golf/PlayerStats.xlsx", sheet = "Travelers")
RocketMortgage <- read_excel("Golf/PlayerStats.xlsx", sheet = "Rocket Mortgage")
JohnDeere <- read_excel("Golf/PlayerStats.xlsx", sheet = "John Deere")
ISCO <- read_excel("Golf/PlayerStats.xlsx", sheet = "ISCO")
Unfiltered_Stats <- rbind(ISCO, JohnDeere, RocketMortgage, Travelers, USOpen, Memorial, CanadianOpen, CharlesSchwab, PGAChampionship, MyrtleBeach, TheSentry, SonyOpen, FarmersInsurance, `AT&T`, WM, Genesis, `Mexican Open`, ArnoldPalmer, RSM, Shriners, SandersonFarms, Fortinet, TourChampionship, BMW, FedexStJude, Wyndham, `3M`, ScottishOpen, Players, Valspar, HoustonOpen, TexasOpen, RBCHeritage, ByronNelson, WellsFargo) %>% 
  mutate(`AVG RTP...26`=case_when(`AVG RTP...26`=="E"~"0",
                                  `AVG RTP...26`!="E"~`AVG RTP...26`),
         `AVG RTP...29`=case_when(`AVG RTP...29`=="E"~"0",
                                  `AVG RTP...29`!="E"~`AVG RTP...29`)) %>% 
  arrange(desc(...1))
Total_Stats <- data.frame(unique(Unfiltered_Stats$PLAYER...3))
colnames(Total_Stats) <- "Name"
for (i in 1:nrow(Total_Stats)) {
  stats <- filter(Unfiltered_Stats, PLAYER...3==Total_Stats$Name[i])
  Total_Stats$`Tournaments Played`[i]=nrow(stats)
  stats <- head(filter(Unfiltered_Stats, PLAYER...3==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...3==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...3==Total_Stats$Name[i]), 10)
  Total_Stats$`T2G`[i]=mean(stats$AVG...4)
  Total_Stats$`Last 5 T2G`[i]=mean(last5stats$AVG...4)
  Total_Stats$`Last 10 T2G`[i]=mean(last10stats$AVG...4)
  Total_Stats$`OTT`[i]=mean(stats$`SG:OTT`)
  Total_Stats$`Last 5 OTT`[i]=mean(last5stats$`SG:OTT`)
  Total_Stats$`Last 10 OTT`[i]=mean(last10stats$`SG:OTT`)
  Total_Stats$`APR`[i]=mean(stats$`SG:APR`)
  Total_Stats$`Last 5 APR`[i]=mean(last5stats$`SG:APR`)
  Total_Stats$`Last 10 APR`[i]=mean(last10stats$`SG:APR`)
  Total_Stats$`ARG`[i]=mean(stats$`SG:ARG`)
  Total_Stats$`Last 5 ARG`[i]=mean(last5stats$`SG:ARG`)
  Total_Stats$`Last 10 ARG`[i]=mean(last10stats$`SG:ARG`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...10==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...10==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...10==Total_Stats$Name[i]), 10)
  Total_Stats$`Putt`[i]=mean(stats$`AVG...11`)
  Total_Stats$`Last 5 Putt`[i]=mean(last5stats$`AVG...11`)
  Total_Stats$`Last 10 Putt`[i]=mean(last10stats$`AVG...11`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...15==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...15==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...15==Total_Stats$Name[i]), 10)
  Total_Stats$`Driving Distance`[i]=mean(stats$`AVG...16`)
  Total_Stats$`Last 5 Driving Distance`[i]=mean(last5stats$`AVG...16`)
  Total_Stats$`Last 10 Driving Distance`[i]=mean(last10stats$`AVG...16`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...20==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...20==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...20==Total_Stats$Name[i]), 10)
  Total_Stats$`Driving Accuracy`[i]=mean(stats$`%...21`)
  Total_Stats$`Last 5 Driving Accuracy`[i]=mean(last5stats$`%...21`)
  Total_Stats$`Last 10 Driving Accuracy`[i]=mean(last10stats$`%...21`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...25==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...25==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...25==Total_Stats$Name[i]), 10)
  Total_Stats$`150-175 APR`[i]=mean(as.numeric(stats$`AVG RTP...26`))
  Total_Stats$`Last 5 150-175 APR`[i]=mean(as.numeric(last5stats$`AVG RTP...26`))
  Total_Stats$`Last 10 150-175 APR`[i]=mean(as.numeric(last10stats$`AVG RTP...26`))
  stats <- head(filter(Unfiltered_Stats, PLAYER...28==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...28==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...28==Total_Stats$Name[i]), 10)
  Total_Stats$`125-150 APR`[i]=mean(as.numeric(stats$`AVG RTP...29`))
  Total_Stats$`Last 5 125-150 APR`[i]=mean(as.numeric(last5stats$`AVG RTP...29`))
  Total_Stats$`Last 10 125-150 APR`[i]=mean(as.numeric(last10stats$`AVG RTP...29`))
  stats <- head(filter(Unfiltered_Stats, PLAYER...31==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...31==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...31==Total_Stats$Name[i]), 10)
  Total_Stats$`Scrambling`[i]=mean(stats$`%...32`)
  Total_Stats$`Last 5 Scrambling`[i]=mean(last5stats$`%...32`)
  Total_Stats$`Last 10 Scrambling`[i]=mean(last10stats$`%...32`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...36==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...36==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...36==Total_Stats$Name[i]), 10)
  Total_Stats$`One Putt`[i]=mean(stats$`%...37`)
  Total_Stats$`Last 5 One Putt`[i]=mean(last5stats$`%...37`)
  Total_Stats$`Last 10 One Putt`[i]=mean(last10stats$`%...37`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...41==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...41==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...41==Total_Stats$Name[i]), 10)
  Total_Stats$`Three Putt Avoidance`[i]=mean(stats$`%...42`)
  Total_Stats$`Last 5 Three Putt Avoidance`[i]=mean(last5stats$`%...42`)
  Total_Stats$`Last 10 Three Putt Avoidance`[i]=mean(last10stats$`%...42`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...46==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...46==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...46==Total_Stats$Name[i]), 10)
  Total_Stats$`Par 3 Scoring`[i]=mean(stats$`AVG...47`)
  Total_Stats$`Last 5 Par 3 Scoring`[i]=mean(last5stats$`AVG...47`)
  Total_Stats$`Last 10 Par 3 Scoring`[i]=mean(last10stats$`AVG...47`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...51==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...51==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...51==Total_Stats$Name[i]), 10)
  Total_Stats$`Par 4 Scoring`[i]=mean(stats$`AVG...52`)
  Total_Stats$`Last 5 Par 4 Scoring`[i]=mean(last5stats$`AVG...52`)
  Total_Stats$`Last 10 Par 4 Scoring`[i]=mean(last10stats$`AVG...52`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...56==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...56==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...56==Total_Stats$Name[i]), 10)
  Total_Stats$`Par 5 Scoring`[i]=mean(stats$`AVG...57`)
  Total_Stats$`Last 5 Par 5 Scoring`[i]=mean(last5stats$`AVG...57`)
  Total_Stats$`Last 10 Par 5 Scoring`[i]=mean(last10stats$`AVG...57`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...61==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...61==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...61==Total_Stats$Name[i]), 10)
  Total_Stats$`Total Putting`[i]=mean(stats$`AVG...62`)
  Total_Stats$`Last 5 Total Putting`[i]=mean(last5stats$`AVG...62`)
  Total_Stats$`Last 10 Total Putting`[i]=mean(last10stats$`AVG...62`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...67==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...67==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...67==Total_Stats$Name[i]), 10)
  Total_Stats$`Birdies`[i]=mean(stats$`%...68`)
  Total_Stats$`Last 5 Birdies`[i]=mean(last5stats$`%...68`)
  Total_Stats$`Last 10 Birdies`[i]=mean(last10stats$`%...68`)
  stats <- head(filter(Unfiltered_Stats, PLAYER...73==Total_Stats$Name[i]),20)
  last5stats <- head(filter(Unfiltered_Stats, PLAYER...73==Total_Stats$Name[i]), 5)
  last10stats <- head(filter(Unfiltered_Stats, PLAYER...73==Total_Stats$Name[i]), 10)
  Total_Stats$`Bogeys`[i]=mean(stats$`% MAKES BOGEY`)
  Total_Stats$`Last 5 Bogeys`[i]=mean(last5stats$`% MAKES BOGEY`)
  Total_Stats$`Last 10 Bogeys`[i]=mean(last10stats$`% MAKES BOGEY`)
}
for (i in 1:nrow(Odds)) {
  p <- filter(Total_Stats, Name==Odds$Golfer[i])
  if (nrow(p)>0) {
    Odds$Name[i]="Yes"
  } else {
    Odds$Name[i]="No"
  }
}
Odds <- filter(Odds, Name=="Yes") %>% 
  select(-Name)
Tournament_Data <- read_excel("Golf/TourneyStats.xlsx", sheet = Tournament) %>% 
  arrange(-YEAR)
if (Tournament=="Masters" | Tournament=="The Open") {
  Tournament_Leaderboard <- Tournament_Data %>% 
    select(Rank, Player, Score, Putt, ARG, APR, OTT, T2G)
  Tournament_DrivingDistance <- Tournament_Data %>% 
    select(RANK...9, PLAYER...10, AVG...11)
  Tournament_DrivingDistance <- na.omit(Tournament_DrivingDistance)
  colnames(Tournament_DrivingDistance) <- c("Rank", "Player", "Drive Distance")
  Tournament_DrivingAccuracy <- Tournament_Data %>% 
    select(RANK...14, PLAYER...15, `%...16`)
  Tournament_DrivingAccuracy <- na.omit(Tournament_DrivingAccuracy)
  colnames(Tournament_DrivingAccuracy) <- c("Rank", "Player", "Drive Accuracy")
  Tournament_Scrambling <- Tournament_Data %>% 
    select(RANK...19, PLAYER...20, `%...21`)
  Tournament_Scrambling <- na.omit(Tournament_Scrambling)
  colnames(Tournament_Scrambling) <- c("Rank", "Player", "Scrambling")
  Tournament_OnePutt <- Tournament_Data %>% 
    select(RANK...24, PLAYER...25, `%...26`)
  Tournament_OnePutt <- na.omit(Tournament_OnePutt)
  colnames(Tournament_OnePutt) <- c("Rank", "Player", "One Putt")
  Tournament_ThreePutt <- Tournament_Data %>% 
    select(RANK...29, PLAYER...30, `%...31`)
  Tournament_ThreePutt <- na.omit(Tournament_ThreePutt)
  colnames(Tournament_ThreePutt) <- c("Rank", "Player", "Three Putt")
  Tournament_Par3 <- Tournament_Data %>% 
    select(RANK...34, PLAYER...35, `AVG...36`)
  Tournament_Par3 <- na.omit(Tournament_Par3)
  colnames(Tournament_Par3) <- c("Rank", "Player", "Par 3")
  Tournament_Par4 <- Tournament_Data %>% 
    select(RANK...39, PLAYER...40, `AVG...41`)
  Tournament_Par4 <- na.omit(Tournament_Par4)
  colnames(Tournament_Par4) <- c("Rank", "Player", "Par 4")
  Tournament_Par5 <- Tournament_Data %>% 
    select(RANK...44, PLAYER...45, `AVG...46`)
  Tournament_Par5 <- na.omit(Tournament_Par5)
  colnames(Tournament_Par5) <- c("Rank", "Player", "Par 5")
  Tournament_TotalPutting <- Tournament_Data %>% 
    select(RANK...49, PLAYER...50, `AVG...51`)
  Tournament_TotalPutting <- na.omit(Tournament_TotalPutting)
  colnames(Tournament_TotalPutting) <- c("Rank", "Player", "Total Putting")
  Tournament_Birdies <- Tournament_Data %>% 
    select(RANK...55, PLAYER...56, `%...57`)
  Tournament_Birdies <- na.omit(Tournament_Birdies)
  colnames(Tournament_Birdies) <- c("Rank", "Player", "Birdies")
  Tournament_Bogeys <- Tournament_Data %>% 
    select(RANK...61, PLAYER...62, `% MAKES BOGEY`)
  Tournament_Bogeys <- na.omit(Tournament_Bogeys)
  colnames(Tournament_Bogeys) <- c("Rank", "Player", "Bogeys")
  Tournament_Data_Total <- data.frame()
  Total_Tournaments <- which(Tournament_Leaderboard$Rank=="1")
  if (length(Total_Tournaments)>5) {
    for (i in length(Total_Tournaments):length(Total_Tournaments)-4) {
      z <- data.frame()
      a <- data.frame()
      d <- data.frame()
      e <- data.frame()
      f <- data.frame()
      g <- data.frame()
      h <- data.frame()
      s <- data.frame()
      u <- data.frame()
      q <- data.frame()
      v <- data.frame()
      if (i>=length(Total_Tournaments)) {
        leaderboard <- tail(Tournament_Leaderboard, Total_Tournaments[i])
        drivedist <- tail(Tournament_DrivingDistance, Total_Tournaments[i])
        driveacc <- tail(Tournament_DrivingAccuracy, Total_Tournaments[i])
        scrambling <- tail(Tournament_Scrambling, Total_Tournaments[i])
        oneputt <- tail(Tournament_OnePutt, Total_Tournaments[i])
        threeputt <- tail(Tournament_ThreePutt, Total_Tournaments[i])
        par3 <- tail(Tournament_Par3, Total_Tournaments[i])
        par4 <- tail(Tournament_Par4, Total_Tournaments[i])
        par5 <- tail(Tournament_Par5, Total_Tournaments[i])
        totalputt <- tail(Tournament_TotalPutting, Total_Tournaments[i])
        birdies <- tail(Tournament_Birdies, Total_Tournaments[i])
        bogeys <- tail(Tournament_Bogeys, Total_Tournaments[i])
      } else {
        putt <- Tournament_Putting[Total_Tournaments[i-1]:Total_Tournaments[i]-1,] 
        leaderboard <- Tournament_Leaderboard[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        drivedist <- Tournament_DrivingDistance[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        driveacc <- Tournament_DrivingAccuracy[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        scrambling <- Tournament_Scrambling[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        oneputt <- Tournament_OnePutt[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        threeputt <- Tournament_ThreePutt[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        par3 <- Tournament_Par3[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        par4 <- Tournament_Par4[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        par5 <- Tournament_Par5[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        totalputt <- Tournament_TotalPutting[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        birdies <- Tournament_Birdies[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
        bogeys <- Tournament_Bogeys[Total_Tournaments[i-1]:Total_Tournaments[i]-1,]
      }
      z <- merge(leaderboard, drivedist, by.x = "Player", by.y = "Player")
      a <- merge(z, driveacc, by.x = "Player", by.y = "Player")
      d <- merge(a, scrambling, by.x = "Player", by.y = "Player")
      e <- merge(d, oneputt, by.x = "Player", by.y = "Player")
      f <- merge(e, threeputt, by.x = "Player", by.y = "Player")
      g <- merge(f, par3, by.x = "Player", by.y = "Player")
      h <- merge(g, par4, by.x = "Player", by.y = "Player")
      s <- merge(h, par5, by.x = "Player", by.y = "Player")
      u <- merge(s, totalputt, by.x = "Player", by.y = "Player")
      q <- merge(u, birdies, by.x = "Player", by.y = "Player")
      v <- merge(q, bogeys, by.x = "Player", by.y = "Player")
      Tournament_Data_Total <- rbind(Tournament_Data_Total, v)
    }
  } else {
    for (i in 1:length(Total_Tournaments)) {
      z <- data.frame()
      a <- data.frame()
      d <- data.frame()
      e <- data.frame()
      f <- data.frame()
      g <- data.frame()
      h <- data.frame()
      s <- data.frame()
      u <- data.frame()
      q <- data.frame()
      v <- data.frame()
      if (i>=length(Total_Tournaments)) {
        leaderboard <- Tournament_Leaderboard[Total_Tournaments[i]:nrow(Tournament_Leaderboard),]
        drivedist <- Tournament_DrivingDistance[Total_Tournaments[i]:nrow(Tournament_DrivingDistance),]
        driveacc <- Tournament_DrivingAccuracy[Total_Tournaments[i]:nrow(Tournament_DrivingAccuracy),]
        scrambling <- Tournament_Scrambling[Total_Tournaments[i]:nrow(Tournament_Scrambling),]
        oneputt <- Tournament_OnePutt[Total_Tournaments[i]:nrow(Tournament_OnePutt),]
        threeputt <- Tournament_ThreePutt[Total_Tournaments[i]:nrow(Tournament_ThreePutt),]
        par3 <- Tournament_Par3[Total_Tournaments[i]:nrow(Tournament_Par3),]
        par4 <- Tournament_Par4[Total_Tournaments[i]:nrow(Tournament_Par4),]
        par5 <- Tournament_Par5[Total_Tournaments[i]:nrow(Tournament_Par5),]
        totalputt <- Tournament_TotalPutting[Total_Tournaments[i]:nrow(Tournament_TotalPutting),]
        birdies <- Tournament_Birdies[Total_Tournaments[i]:nrow(Tournament_Birdies),]
        bogeys <- Tournament_Bogeys[Total_Tournaments[i]:nrow(Tournament_Bogeys),]
      } else {
        leaderboard <- Tournament_Leaderboard[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        drivedist <- Tournament_DrivingDistance[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        driveacc <- Tournament_DrivingAccuracy[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        scrambling <- Tournament_Scrambling[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        oneputt <- Tournament_OnePutt[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        threeputt <- Tournament_ThreePutt[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        par3 <- Tournament_Par3[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        par4 <- Tournament_Par4[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        par5 <- Tournament_Par5[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        totalputt <- Tournament_TotalPutting[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        birdies <- Tournament_Birdies[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
        bogeys <- Tournament_Bogeys[Total_Tournaments[i]:Total_Tournaments[i+1]-1,]
      }
      z <- merge(leaderboard, drivedist, by.x = "Player", by.y = "Player")
      a <- merge(z, driveacc, by.x = "Player", by.y = "Player")
      d <- merge(a, scrambling, by.x = "Player", by.y = "Player")
      e <- merge(d, oneputt, by.x = "Player", by.y = "Player")
      f <- merge(e, threeputt, by.x = "Player", by.y = "Player")
      g <- merge(f, par3, by.x = "Player", by.y = "Player")
      h <- merge(g, par4, by.x = "Player", by.y = "Player")
      s <- merge(h, par5, by.x = "Player", by.y = "Player")
      u <- merge(s, totalputt, by.x = "Player", by.y = "Player")
      q <- merge(u, birdies, by.x = "Player", by.y = "Player")
      v <- merge(q, bogeys, by.x = "Player", by.y = "Player")
      Tournament_Data_Total <- rbind(Tournament_Data_Total, v)
    }
  }
  colnames(Tournament_Data_Total) <- c("Player", "Rank", "Score", "Putt", "ARG", "APR", "OTT" , "T2G", "Drive Distance Rank", "Drive Distance", "Drive Accuracy Rank", "Drive Accuracy", "Scrambling Rank", "Scrambling", "One Putt Rank", "One Putt", "Three Putt Rank", "Three Putt", "Par 3 Rank", "Par 3", "Par 4 Rank", "Par 4", "Par 5 Rank", "Par 5", "Total Putting Rank", "Total Putting", "Birdies Rank", "Birdies", "Bogeys Rank", "Bogeys")
  Tournament_Data_Total <- unique.array(Tournament_Data_Total)
  model  <- lm(as.numeric(Score) ~ as.numeric(Putt) + as.numeric(ARG) + as.numeric(APR) + as.numeric(OTT) + as.numeric(Scrambling) + as.numeric(`One Putt`) + as.numeric(`Three Putt`) + as.numeric(`Par 3`) + as.numeric(`Par 4`) + as.numeric(`Par 5`) + as.numeric(`Drive Distance`) + as.numeric(`Drive Accuracy`) + as.numeric(`Total Putting`) + as.numeric(`Birdies`) + as.numeric(`Bogeys`), data = Tournament_Data_Total)
  Tournament_Data_Total$Player[Tournament_Data_Total$Player %in% old] <- new[match(Tournament_Data_Total$Player, old, nomatch = 0)]
  for (i in 1:nrow(Total_Stats)) {
    p = filter(Tournament_Data_Total, Player==Total_Stats$Name[i])
    if (nrow(p)>0) {
      Total_Stats$TourneyPutt[i]=mean(p$Putt)
      Total_Stats$TourneyOTT[i]=mean(p$OTT)
      Total_Stats$TourneyAPR[i]=mean(p$APR)
      Total_Stats$TourneyARG[i]=mean(p$ARG)
      Total_Stats$TourneyT2G[i]=mean(p$T2G)
      Total_Stats$TourneyDriveDistance[i]=mean(p$`Drive Distance`)
      Total_Stats$TourneyDriveAccuracy[i]=mean(p$`Drive Accuracy`)
      Total_Stats$TourneyScrambling[i]=mean(p$Scrambling)
      Total_Stats$TourneyOnePutt[i]=mean(p$`One Putt`)
      Total_Stats$TourneyThreePutt[i]=mean(p$`Three Putt`)
      Total_Stats$TourneyPar3[i]=mean(p$`Par 3`)
      Total_Stats$TourneyPar4[i]=mean(p$`Par 4`)
      Total_Stats$TourneyPar5[i]=mean(p$`Par 5`)
      Total_Stats$TourneyTotalPutting[i]=mean(p$`Total Putting`)
      Total_Stats$TourneyBirdies[i]=mean(p$`Birdies`)
      Total_Stats$TourneyBogeys[i]=mean(p$`Bogeys`)
    } else {
      Total_Stats$TourneyPutt[i]=NA
      Total_Stats$TourneyOTT[i]=NA
      Total_Stats$TourneyAPR[i]=NA
      Total_Stats$TourneyARG[i]=NA
      Total_Stats$TourneyT2G[i]=NA
      Total_Stats$TourneyDriveDistance[i]=NA
      Total_Stats$TourneyDriveAccuracy[i]=NA
      Total_Stats$TourneyScrambling[i]=NA
      Total_Stats$TourneyOnePutt[i]=NA
      Total_Stats$TourneyThreePutt[i]=NA
      Total_Stats$TourneyPar3[i]=NA
      Total_Stats$TourneyPar4[i]=NA
      Total_Stats$TourneyPar5[i]=NA
      Total_Stats$TourneyTotalPutting[i]=NA
      Total_Stats$TourneyBirdies[i]=NA
      Total_Stats$TourneyBogeys[i]=NA
    }
  }
  Total_Stats <- Total_Stats %>%
    mutate(Scrambling=Scrambling*100,
           `Last 5 Scrambling`=`Last 5 Scrambling`*100,
           `Last 10 Scrambling`=`Last 10 Scrambling`*100,
           `One Putt`=`One Putt`*100,
           `Last 5 One Putt`=`Last 5 One Putt`*100,
           `Last 10 One Putt`=`Last 10 One Putt`*100,
           `Three Putt Avoidance`=`Three Putt Avoidance`*100,
           `Last 5 Three Putt Avoidance`=`Last 5 Three Putt Avoidance`*100,
           `Last 10 Three Putt Avoidance`=`Last 10 Three Putt Avoidance`*100) %>% 
    mutate(Projected_Score=case_when(is.na(TourneyPutt)~(model$coefficients[1] + model$coefficients[2]*((.166*Putt + .5*`Last 5 Putt` + .333*`Last 10 Putt`)) + model$coefficients[3]*((.166*ARG + .5*`Last 5 ARG` + .333*`Last 10 ARG`)) + model$coefficients[4]*((.166*APR + .5*`Last 5 APR` + .333*`Last 10 APR`)) + model$coefficients[5]*((.166*OTT + .5*`Last 5 OTT` + .333*`Last 10 OTT`)) + model$coefficients[6]*((.166*Scrambling + .5*`Last 5 Scrambling` + .333*`Last 10 Scrambling`)) + model$coefficients[7]*((.166*`One Putt` + .5*`Last 5 One Putt` + .333*`Last 10 One Putt`)) + model$coefficients[8]*((.166*`Three Putt Avoidance` + .5*`Last 5 Three Putt Avoidance` + .333*`Last 10 Three Putt Avoidance`)) + model$coefficients[9]*((.166*`Par 3 Scoring` + .5*`Last 5 Par 3 Scoring` + .333*`Last 10 Par 3 Scoring`)) + model$coefficients[10]*((.166*`Par 4 Scoring` + .5*`Last 5 Par 4 Scoring` + .333*`Last 10 Par 4 Scoring`)) + model$coefficients[11]*((.166*`Par 5 Scoring` + .5*`Last 5 Par 5 Scoring` + .333*`Last 10 Par 5 Scoring`)) + model$coefficients[12]*((.166*`Driving Distance` + .5*`Last 5 Driving Distance` + .333*`Last 10 Driving Distance`)) + model$coefficients[13]*((.166*`Driving Accuracy` + .5*`Last 5 Driving Accuracy` + .333*`Last 10 Driving Accuracy`))  + model$coefficients[14]*((.166*`Total Putting` + .5*`Last 5 Total Putting` + .333*`Last 10 Total Putting`)) +   + model$coefficients[15]*((.166*`Birdies` + .5*`Last 5 Birdies` + .333*`Last 10 Birdies`))  + model$coefficients[16]*((.166*`Bogeys` + .5*`Last 5 Bogeys` + .333*`Last 10 Bogeys`))),
                                     !is.na(TourneyPutt)~(model$coefficients[1] + model$coefficients[2]*((.1*Putt + .3*`Last 5 Putt` + .2*`Last 10 Putt` + .4*TourneyPutt)) + model$coefficients[3]*((.1*ARG + .3*`Last 5 ARG` + .2*`Last 10 ARG` + .4*TourneyARG)) + model$coefficients[4]*((.1*APR + .3*`Last 5 APR` + .2*`Last 10 APR` + .4*TourneyAPR)) + model$coefficients[5]*((.1*OTT + .3*`Last 5 OTT` + .2*`Last 10 OTT` + .4*TourneyOTT)) + model$coefficients[6]*((.1*Scrambling + .3*`Last 5 Scrambling` + .2*`Last 10 Scrambling` + .4*TourneyScrambling)) + model$coefficients[7]*((.1*`One Putt` + .3*`Last 5 One Putt` + .2*`Last 10 One Putt` + .4*TourneyOnePutt)) + model$coefficients[8]*((.1*`Three Putt Avoidance` + .3*`Last 5 Three Putt Avoidance` + .2*`Last 10 Three Putt Avoidance` + .4*TourneyThreePutt)) + model$coefficients[9]*((.1*`Par 3 Scoring` + .3*`Last 5 Par 3 Scoring` + .2*`Last 10 Par 3 Scoring` + .4*TourneyPar3)) + model$coefficients[10]*((.1*`Par 4 Scoring` + .3*`Last 5 Par 4 Scoring` + .2*`Last 10 Par 4 Scoring` + .4*TourneyPar4)) + model$coefficients[11]*((.1*`Par 5 Scoring` + .3*`Last 5 Par 5 Scoring` + .2*`Last 10 Par 5 Scoring` + .4*TourneyPar5)) + model$coefficients[12]*((.1*`Driving Distance` + .3*`Last 5 Driving Distance` + .2*`Last 10 Driving Distance` + .4*TourneyDriveDistance)) + model$coefficients[13]*((.1*`Driving Accuracy` + .3*`Last 5 Driving Accuracy` + .2*`Last 10 Driving Accuracy` + .4*TourneyDriveAccuracy)) + model$coefficients[14]*((.1*`Total Putting` + .3*`Last 5 Total Putting` + .2*`Last 10 Total Putting` + .4*TourneyTotalPutting)) + model$coefficients[15]*((.1*`Birdies` + .3*`Last 5 Birdies` + .2*`Last 10 Birdies` + .4*TourneyBirdies)) + model$coefficients[16]*((.1*`Bogeys` + .3*`Last 5 Bogeys` + .2*`Last 10 Bogeys` + .4*TourneyBogeys)))))
  
} else {
  Total_Tournaments <- which(Tournament_Data$POS=="1")
    Tournament_Leaderboard <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, POS, PLAYER...3, SCORE)
    colnames(Tournament_Leaderboard) <- c("Year", "Rank", "Player", "Score")
    Tournament_T2G <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...5, PLAYER...6, `AVG...7`, `SG:OTT`, `SG:APR`, `SG:ARG`)
    Tournament_T2G <- na.omit(Tournament_T2G)
    colnames(Tournament_T2G) <- c("Year", "Rank", "Player", "T2G", "OTT", "APR", "ARG")
    Tournament_Putting <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...12, PLAYER...13, AVG...14)
    Tournament_Putting <- na.omit(Tournament_Putting)
    colnames(Tournament_Putting) <- c("Year", "Rank", "Player", "Putt")
    Tournament_DrivingDistance <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...17, PLAYER...18, AVG...19)
    Tournament_DrivingDistance <- na.omit(Tournament_DrivingDistance)
    colnames(Tournament_DrivingDistance) <- c("Year", "Rank", "Player", "Drive Distance")
    Tournament_DrivingAccuracy <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...22, PLAYER...23, `%...24`)
    Tournament_DrivingAccuracy <- na.omit(Tournament_DrivingAccuracy)
    colnames(Tournament_DrivingAccuracy) <- c("Year", "Rank", "Player", "Drive Accuracy")
    Tournament_150175APR <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...27, PLAYER...28, `AVG RTP...29`)
    Tournament_150175APR <- na.omit(Tournament_150175APR)
    colnames(Tournament_150175APR) <- c("Year", "Rank", "Player", "150-175")
    Tournament_125150APR <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...30, PLAYER...31, `AVG RTP...32`)
    Tournament_125150APR <- na.omit(Tournament_125150APR)
    colnames(Tournament_125150APR) <- c("Year", "Rank", "Player", "125-150")
    Tournament_Scrambling <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...33, PLAYER...34, `%...35`)
    Tournament_Scrambling <- na.omit(Tournament_Scrambling)
    colnames(Tournament_Scrambling) <- c("Year", "Rank", "Player", "Scrambling")
    Tournament_OnePutt <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...38, PLAYER...39, `%...40`)
    Tournament_OnePutt <- na.omit(Tournament_OnePutt)
    colnames(Tournament_OnePutt) <- c("Year", "Rank", "Player", "One Putt")
    Tournament_ThreePutt <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...43, PLAYER...44, `%...45`)
    Tournament_ThreePutt <- na.omit(Tournament_ThreePutt)
    colnames(Tournament_ThreePutt) <- c("Year", "Rank", "Player", "Three Putt")
    Tournament_Par3 <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...48, PLAYER...49, `AVG...50`)
    Tournament_Par3 <- na.omit(Tournament_Par3)
    colnames(Tournament_Par3) <- c("Year", "Rank", "Player", "Par 3")
    Tournament_Par4 <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...53, PLAYER...54, `AVG...55`)
    Tournament_Par4 <- na.omit(Tournament_Par4)
    colnames(Tournament_Par4) <- c("Year", "Rank", "Player", "Par 4")
    Tournament_Par5 <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...58, PLAYER...59, `AVG...60`)
    Tournament_Par5 <- na.omit(Tournament_Par5)
    colnames(Tournament_Par5) <- c("Year", "Rank", "Player", "Par 5")
    Tournament_TotalPutting <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...63, PLAYER...64, `AVG...65`)
    Tournament_TotalPutting <- na.omit(Tournament_TotalPutting)
    colnames(Tournament_TotalPutting) <- c("Year", "Rank", "Player", "Total Putting")
    Tournament_Birdies <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...69, PLAYER...70, `%...71`)
    Tournament_Birdies <- na.omit(Tournament_Birdies)
    colnames(Tournament_Birdies) <- c("Year", "Rank", "Player", "Birdies")
    Tournament_Bogeys <- filter(Tournament_Data, YEAR>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]]) %>% 
      select(YEAR, RANK...75, PLAYER...76, `% MAKES BOGEY`)
    Tournament_Bogeys <- na.omit(Tournament_Bogeys)
    colnames(Tournament_Bogeys) <- c("Year", "Rank", "Player", "Bogeys")
  Tournament_Data_Total <- data.frame()
  for (i in 1:length(Total_Tournaments)) {
    x <- data.frame()
    y <- data.frame()
    z <- data.frame()
    a <- data.frame()
    b <- data.frame()
    c <- data.frame()
    d <- data.frame()
    e <- data.frame()
    f <- data.frame()
    g <- data.frame()
    h <- data.frame()
    s <- data.frame()
    u <- data.frame()
    q <- data.frame()
    v <- data.frame()
    t2g <- filter(Tournament_T2G, Year==Tournament_Data$YEAR[Total_Tournaments[i]]) %>% 
      select(-Year)
    putt <- filter(Tournament_Putting, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    leaderboard <- filter(Tournament_Leaderboard, Year==Tournament_Data$YEAR[Total_Tournaments[i]])
    drivedist <- filter(Tournament_DrivingDistance, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    driveacc <- filter(Tournament_DrivingAccuracy, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    `150-175` <- filter(Tournament_150175APR, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    `125-150` <- filter(Tournament_125150APR, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    scrambling <- filter(Tournament_Scrambling, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    oneputt <- filter(Tournament_OnePutt, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    threeputt <- filter(Tournament_ThreePutt, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    par3 <- filter(Tournament_Par3, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    par4 <- filter(Tournament_Par4, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    par5 <- filter(Tournament_Par5, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    totalputt <- filter(Tournament_TotalPutting, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    birdies <- filter(Tournament_Birdies, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    bogeys <- filter(Tournament_Bogeys, Year==Tournament_Data$YEAR[Total_Tournaments[i]])%>% 
      select(-Year)
    x <- merge(leaderboard, t2g, by.x = "Player", by.y = "Player")
    y <- merge(x, putt, by.x = "Player", by.y = "Player")
    z <- merge(y, drivedist, by.x = "Player", by.y = "Player")
    a <- merge(z, driveacc, by.x = "Player", by.y = "Player")
    b <- merge(a, `150-175`, by.x = "Player", by.y = "Player")
    c <- merge(b, `125-150`, by.x = "Player", by.y = "Player")
    d <- merge(c, scrambling, by.x = "Player", by.y = "Player")
    e <- merge(d, oneputt, by.x = "Player", by.y = "Player")
    f <- merge(e, threeputt, by.x = "Player", by.y = "Player")
    g <- merge(f, par3, by.x = "Player", by.y = "Player")
    h <- merge(g, par4, by.x = "Player", by.y = "Player")
    s <- merge(h, par5, by.x = "Player", by.y = "Player")
    u <- merge(s, totalputt, by.x = "Player", by.y = "Player")
    q <- merge(u, birdies, by.x = "Player", by.y = "Player")
    v <- merge(q, bogeys, by.x = "Player", by.y = "Player")
    Tournament_Data_Total <- rbind(Tournament_Data_Total, v)
  }
  colnames(Tournament_Data_Total) <- c("Player", "Year", "Rank", "Score", "T2G Rank", "T2G", "OTT", "APR" , "ARG", "Putt Rank", "Putt", "Drive Distance Rank", "Drive Distance", "Drive Accuracy Rank", "Drive Accuracy", "150-175 Apr Rank", "150-175", "125-150 Apr Rank", "125-150", "Scrambling Rank", "Scrambling", "One Putt Rank", "One Putt", "Three Putt Rank", "Three Putt", "Par 3 Rank", "Par 3", "Par 4 Rank", "Par 4", "Par 5 Rank", "Par 5", "Total Putting Rank", "Total Putting", "Birdies Rank", "Birdies", "Bogeys Rank", "Bogeys")
  Tournament_Data_Total <- unique.array(Tournament_Data_Total) %>% 
    mutate(`150-175`=case_when(`150-175`=="E"~"0",
                              TRUE~`150-175`),
          `125-150`=case_when(`125-150`=="E"~"0",
                              TRUE~`125-150`))
  for (i in 1:nrow(Total_Stats)) {
    p = filter(Tournament_Data_Total, Player==Total_Stats$Name[i])
    if (nrow(p)>0) {
      Total_Stats$TourneyPutt[i]=mean(p$Putt)
      Total_Stats$TourneyOTT[i]=mean(p$OTT)
      Total_Stats$TourneyAPR[i]=mean(p$APR)
      Total_Stats$TourneyARG[i]=mean(p$ARG)
      Total_Stats$TourneyT2G[i]=mean(p$T2G)
      Total_Stats$TourneyDriveDistance[i]=mean(p$`Drive Distance`)
      Total_Stats$TourneyDriveAccuracy[i]=mean(p$`Drive Accuracy`)
      Total_Stats$Tourney150175APR[i]=mean(as.numeric(p$`150-175`))
      Total_Stats$Tourney125150APR[i]=mean(as.numeric(p$`125-150`))
      Total_Stats$TourneyScrambling[i]=mean(p$Scrambling)
      Total_Stats$TourneyOnePutt[i]=mean(p$`One Putt`)
      Total_Stats$TourneyThreePutt[i]=mean(p$`Three Putt`)
      Total_Stats$TourneyPar3[i]=mean(p$`Par 3`)
      Total_Stats$TourneyPar4[i]=mean(p$`Par 4`)
      Total_Stats$TourneyPar5[i]=mean(p$`Par 5`)
      Total_Stats$TourneyTotalPutting[i]=mean(p$`Total Putting`)
      Total_Stats$TourneyBirdies[i]=mean(p$`Birdies`)
      Total_Stats$TourneyBogeys[i]=mean(p$`Bogeys`)
    } else {
      Total_Stats$TourneyPutt[i]=NA
      Total_Stats$TourneyOTT[i]=NA
      Total_Stats$TourneyAPR[i]=NA
      Total_Stats$TourneyARG[i]=NA
      Total_Stats$TourneyT2G[i]=NA
      Total_Stats$TourneyDriveDistance[i]=NA
      Total_Stats$TourneyDriveAccuracy[i]=NA
      Total_Stats$Tourney150175APR[i]=NA
      Total_Stats$Tourney125150APR[i]=NA
      Total_Stats$TourneyScrambling[i]=NA
      Total_Stats$TourneyOnePutt[i]=NA
      Total_Stats$TourneyThreePutt[i]=NA
      Total_Stats$TourneyPar3[i]=NA
      Total_Stats$TourneyPar4[i]=NA
      Total_Stats$TourneyPar5[i]=NA
      Total_Stats$TourneyTotalPutting[i]=NA
      Total_Stats$TourneyBirdies[i]=NA
      Total_Stats$TourneyBogeys[i]=NA
    }
  }
  if (length(Total_Tournaments)>5) {
    Tournament_Data_Total <- filter(Tournament_Data_Total, Year>=Tournament_Data$YEAR[Total_Tournaments[5]])
  } else {
    Tournament_Data_Total <- filter(Tournament_Data_Total, Year>=Tournament_Data$YEAR[Total_Tournaments[length(Total_Tournaments)]])
  }
  model  <- lm(as.numeric(Score) ~ as.numeric(Putt) + as.numeric(ARG) + as.numeric(APR) + as.numeric(OTT) + as.numeric(`150-175`) + as.numeric(`125-150`) + as.numeric(Scrambling) + as.numeric(`One Putt`) + as.numeric(`Three Putt`) + as.numeric(`Par 3`) + as.numeric(`Par 4`) + as.numeric(`Par 5`) + as.numeric(`Drive Distance`) + as.numeric(`Drive Accuracy`) + as.numeric(`Total Putting`) + as.numeric(`Birdies`) + as.numeric(`Bogeys`) + as.numeric(T2G), data = Tournament_Data_Total)
  Total_Stats <- Total_Stats %>%
    mutate(Projected_Score=case_when(is.na(TourneyPutt)~(model$coefficients[1] + model$coefficients[2]*((.166*Putt + .5*`Last 5 Putt` + .333*`Last 10 Putt`)) + model$coefficients[3]*((.166*ARG + .5*`Last 5 ARG` + .333*`Last 10 ARG`)) + model$coefficients[4]*((.166*APR + .5*`Last 5 APR` + .333*`Last 10 APR`)) + model$coefficients[5]*((.166*OTT + .5*`Last 5 OTT` + .333*`Last 10 OTT`)) + model$coefficients[6]*((.166*`150-175 APR` + .5*`Last 5 150-175 APR` + .333*`Last 10 150-175 APR`)) + model$coefficients[7]*((.166*`125-150 APR` + .5*`Last 5 125-150 APR` + .333*`Last 10 125-150 APR`)) + model$coefficients[8]*((.166*Scrambling + .5*`Last 5 Scrambling` + .333*`Last 10 Scrambling`)) + model$coefficients[9]*((.166*`One Putt` + .5*`Last 5 One Putt` + .333*`Last 10 One Putt`)) + model$coefficients[10]*((.166*`Three Putt Avoidance` + .5*`Last 5 Three Putt Avoidance` + .333*`Last 10 Three Putt Avoidance`)) + model$coefficients[11]*((.166*`Par 3 Scoring` + .5*`Last 5 Par 3 Scoring` + .333*`Last 10 Par 3 Scoring`)) + model$coefficients[12]*((.166*`Par 4 Scoring` + .5*`Last 5 Par 4 Scoring` + .333*`Last 10 Par 4 Scoring`)) + model$coefficients[13]*((.166*`Par 5 Scoring` + .5*`Last 5 Par 5 Scoring` + .333*`Last 10 Par 5 Scoring`)) + model$coefficients[14]*((.166*`Driving Distance` + .5*`Last 5 Driving Distance` + .333*`Last 10 Driving Distance`)) + model$coefficients[15]*((.166*`Driving Accuracy` + .5*`Last 5 Driving Accuracy` + .333*`Last 10 Driving Accuracy`))  + model$coefficients[16]*((.166*`Total Putting` + .5*`Last 5 Total Putting` + .333*`Last 10 Total Putting`)) +   + model$coefficients[17]*((.166*`Birdies` + .5*`Last 5 Birdies` + .333*`Last 10 Birdies`))  + model$coefficients[18]*((.166*`Bogeys` + .5*`Last 5 Bogeys` + .333*`Last 10 Bogeys`)) + model$coefficients[19]*((.166*`T2G` + .5*`Last 5 T2G` + .333*`Last 10 T2G`))),
                                     !is.na(TourneyPutt)~(model$coefficients[1] + model$coefficients[2]*((.1*Putt + .3*`Last 5 Putt` + .2*`Last 10 Putt` + .4*TourneyPutt)) + model$coefficients[3]*((.1*ARG + .3*`Last 5 ARG` + .2*`Last 10 ARG` + .4*TourneyARG)) + model$coefficients[4]*((.1*APR + .3*`Last 5 APR` + .2*`Last 10 APR` + .4*TourneyAPR)) + model$coefficients[5]*((.1*OTT + .3*`Last 5 OTT` + .2*`Last 10 OTT` + .4*TourneyOTT)) + model$coefficients[6]*((.1*`150-175 APR` + .3*`Last 5 150-175 APR` + .2*`Last 10 150-175 APR` + .4*Tourney150175APR)) + model$coefficients[7]*((.1*`125-150 APR` + .3*`Last 5 125-150 APR` + .2*`Last 10 125-150 APR` + .4*Tourney125150APR)) + model$coefficients[8]*((.1*Scrambling + .3*`Last 5 Scrambling` + .2*`Last 10 Scrambling` + .4*TourneyScrambling)) + model$coefficients[9]*((.1*`One Putt` + .3*`Last 5 One Putt` + .2*`Last 10 One Putt` + .4*TourneyOnePutt)) + model$coefficients[10]*((.1*`Three Putt Avoidance` + .3*`Last 5 Three Putt Avoidance` + .2*`Last 10 Three Putt Avoidance` + .4*TourneyThreePutt)) + model$coefficients[11]*((.1*`Par 3 Scoring` + .3*`Last 5 Par 3 Scoring` + .2*`Last 10 Par 3 Scoring` + .4*TourneyPar3)) + model$coefficients[12]*((.1*`Par 4 Scoring` + .3*`Last 5 Par 4 Scoring` + .2*`Last 10 Par 4 Scoring` + .4*TourneyPar4)) + model$coefficients[13]*((.1*`Par 5 Scoring` + .3*`Last 5 Par 5 Scoring` + .2*`Last 10 Par 5 Scoring` + .4*TourneyPar5)) + model$coefficients[14]*((.1*`Driving Distance` + .3*`Last 5 Driving Distance` + .2*`Last 10 Driving Distance` + .4*TourneyDriveDistance)) + model$coefficients[15]*((.1*`Driving Accuracy` + .3*`Last 5 Driving Accuracy` + .2*`Last 10 Driving Accuracy` + .4*TourneyDriveAccuracy)) + model$coefficients[16]*((.1*`Total Putting` + .3*`Last 5 Total Putting` + .2*`Last 10 Total Putting` + .4*TourneyTotalPutting)) + model$coefficients[17]*((.1*`Birdies` + .3*`Last 5 Birdies` + .2*`Last 10 Birdies` + .4*TourneyBirdies)) + model$coefficients[18]*((.1*`Bogeys` + .3*`Last 5 Bogeys` + .2*`Last 10 Bogeys` + .4*TourneyBogeys)) + model$coefficients[19]*((.1*`T2G` + .3*`Last 5 T2G` + .2*`Last 10 T2G` + .4*TourneyT2G)))))
}


Results <- filter(Odds, !is.na(Odds$Win)) %>% 
  select(Golfer, Win)

for (i in 1:nrow(Results)) {
  p = filter(Total_Stats, Name==Results$Golfer[i])
  if (nrow(p)>=1) {
    Results$TournamentsPlayed[i]=p$`Tournaments Played`
    Results$ProjectedScore[i]=p$Projected_Score
  } else {
    Results$TournamentsPlayed[i]=""
    Results$ProjectedScore[i]=""
  }
}

if (Tournament=="Tour Championship") {
  ProjectedResults <- Results
  TourChampLeaderboard <- read_excel("Golf/TourneyStats.xlsx", sheet = "Tour Championship Leaderboard")
  for (i in 1:nrow(ProjectedResults)) {
    stats <- filter(TourChampLeaderboard, Player==ProjectedResults$Golfer[i])
    ProjectedResults$ProjectedScore[i]=ProjectedResults$ProjectedScore[i]+stats$Score
  }
  ProjectedResults <- ProjectedResults %>% 
    arrange(as.numeric(ProjectedScore))
} else {
  ProjectedResults <- Results %>% 
    arrange(as.numeric(ProjectedScore))
}

Results <- head(filter(ProjectedResults, TournamentsPlayed >= 8 & Win <=9000),7) %>% 
  select(-TournamentsPlayed)

Money <- data.frame(c("$3", "$2.5", "$2", "$1.5", "$1", "$1", "$1"))
Results <- cbind(Results, Money)
colnames(Results) <- c("Golfer", "Odds", "Projected Score", "$")


view(Results)
test <- data.frame(model$coefficients)
view(test)