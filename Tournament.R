players <- data.frame(Num = c(1:100), TeamNum = rep(c(1:25),4))
plHistory<-data.frame(PlayerNum = integer(), Played = integer(), Repeats = integer())
placedPlayers <- data.frame(Num = integer(), TeamNum = integer())

tables <- array(rep(0,1000), dim=c(10,10,10))

avPlayers<-players

for (rounds in 1:dim(tables)[3]) {
  avPlayers<-players
  placedPlayers <- data.frame(Num = integer(), TeamNum = integer())
  for (tableNum in 1:dim(tables)[2]) {
    seated<-integer()
    avPlayers<-subset(players, !(players$Num %in% placedPlayers$Num))
    for (places in 1:dim(tables)[1]) {
      teamRest<-aggregate(data = avPlayers, .~TeamNum, FUN = length)
      if (max(teamRest$Num) == 10 - nrow(placedPlayers) %/% 10) {
          critTeams<-subset(teamRest, teamRest$Num==max(teamRest$Num))
          critTeamPlayers<-subset(avPlayers,avPlayers$TeamNum %in% critTeams$TeamNum)
          if (nrow(critTeamPlayers) > 1) {
            setPlayer<-critTeamPlayers[sample(nrow(critTeamPlayers),1),]
          }
          else {
            setPlayer<-critTeamPlayers[1,]
          }
      }
      else {
        if (nrow(avPlayers) > 1) {
          setPlayer<-avPlayers[sample(nrow(avPlayers),1),]
        }
        else { 
          setPlayer<-avPlayers[1,]
          browser()
        }
      }
      placedPlayers<-rbind(placedPlayers,setPlayer)
      avPlayers<-subset(avPlayers, avPlayers$TeamNum != setPlayer$TeamNum)
      played<-subset(plHistory, plHistory$PlayerNum ==  setPlayer$Num && plHistory$Repeats > 1)
      avPlayers<-subset(avPlayers, !(avPlayers$Num %in% played$Played))
      tables[places, tableNum, rounds]<-setPlayer$Num
      seated<-c(seated,rep(setPlayer$Num,10))
    }
    seatedFrame<-data.frame(PlayerNum = seated, Played=rep(tables[c(1:10), tableNum, rounds], 10), Repeats = rep(1,100))
    seatedFrame<-subset(seatedFrame, seatedFrame$PlayerNum != seatedFrame$Played)
    plHistory<-rbind(plHistory,seatedFrame)
    plHistory<-aggregate(data = plHistory, Repeats ~ PlayerNum + Played, FUN = sum)
  }
}

