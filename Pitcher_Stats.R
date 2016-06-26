library(rvest)
library(dplyr)
library(reshape2)

### Grabbing Pitcher Data
### Not complete yet

teams <- c("HOU","SEA","TEX","LAA","OAK")
href_vector <- character(0)
team_vector <- character(0)
pitcher_vector <- character(0)
name_vector <- character(0)

for(i in 1:length(teams)){
  val <- teams[i]
  url <- paste("http://www.baseball-reference.com/teams/",val,"/2016-roster.shtml",sep="")
  
  read_data <- read_html(url)
  
  ## Pitchers
  test1 <- read_data %>%
    html_nodes("#40man") %>%
    html_table()
  
  test1 <- as.data.frame(test1)
  test1 <- test1 %>% select(Name,Var.5)
  test1 <- test1[-nrow(test1),]
  test1 <- test1 %>% filter(Var.5 == "Pitcher")
  ## All Players
  test2 <- read_data %>%
    html_nodes("#appearances") %>%
    html_table()
  
  test2 <- as.data.frame(test2)
  test2 <- test2[-nrow(test2),]
  test2 <- test2 %>% select(Name)
  
  test3 <- read_data %>% 
    html_nodes("#div_appearances , a") %>%
    html_attr("href")
  
  test3 <- data.frame(test3)
  test3 <- filter(test3, grepl('/players/',test3))
  test3[] <- sapply(test3,as.character)
  href_vec <- sapply(seq(1:nrow(test3)), function(j){
    name <- unlist(strsplit(test3[j,],"/"))[4]
    id <- unlist(strsplit(name,"[.]"))[1]
    return(id)
  })
  href_vec <- href_vec[-1]
  
  team_vector <- c(team_vector,rep(val,length(href_vec)))
  href_vector <- c(href_vector,href_vec)
  pitcher_vector <- c(pitcher_vector,test1$Name)
  name_vector <- c(name_vector,test2$Name)
}
# Merge vectors into dataframe
player_data <- data.frame(name_vector,team_vector,href_vector,stringsAsFactors = FALSE)
colnames(player_data)[1] <- "Name"
pitcher_df <- data.frame(pitcher_vector,stringsAsFactors = FALSE)
colnames(pitcher_df) <- "Name"
# Select data only for pitchers
pitcher_data <- merge(pitcher_df,player_data,by="Name")
colnames(pitcher_data) <- c("Name","Team","href")


gamelog_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",pitcher_data$href[1],"&year=2016&t=p",sep="")
game_data <- read_html(gamelog_url)
test <- game_data %>% 
  html_nodes("#total_extra , #total") %>% 
  html_table()

data <- data.frame(test[[1]],stringsAsFactors = FALSE)
data2 <- data.frame(test[[2]],stringsAsFactors = FALSE)
combine <- as.data.frame(c(data[2,],data2[2,]))
vec <- as.vector(combine)
vec$ID <- pitcher_data$href[1] 

for(i in 2:nrow(pitcher_data)){
  gamelog_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",pitcher_data$href[i],"&year=2016&t=p",sep="")
  game_data <- read_html(gamelog_url)
  test <- game_data %>% 
    html_nodes("#total_extra , #total") %>% 
    html_table()
  if(length(test)>=2){
    data <- data.frame(test[[1]],stringsAsFactors = FALSE)
    data2 <- data.frame(test[[2]],stringsAsFactors = FALSE)
    combine <- as.data.frame(c(data[2,],data2[2,]))
    vec[i,] <- as.vector(combine)
    vec$ID[i] <- pitcher_data$href[i] 
  }
}

name_vec <- character(0)
for(i in 1:nrow(vec)){
  name_vec <- c(name_vec,as.character(pitcher_data$Name[vec$ID[i] ==pitcher_data$href]))
}
vec$Name <-name_vec


pitch_data <- vec %>% select(Name,ID,Split,G,GS,GF,IP,W,L,ERA,R,H,PA,AB,HR,BB,SO,SO.W,
                             BA,OBP,SLG,OPS,BAbip,SHO,tOPS.,sOPS.)

final_pitch_data <- pitch_data[!(is.na(pitch_data$Split)),]

write.csv(final_pitch_data,"Weekly_pitching.csv")


write.csv(vec,"Second_Weekly_pitching.csv")
