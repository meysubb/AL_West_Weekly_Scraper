library(rvest)
library(dplyr)
library(ggplot2)


teams <- c("HOU","SEA","TEX","LAA","OAK")
href_vector <- character(0)
team_vector <- character(0)
name_vector <- character(0)
batting_vector <- character(0)

for(i in 1:length(teams)){
  val <- teams[i]
  url <- paste("http://www.baseball-reference.com/teams/",val,"/2016-roster.shtml",sep="")
  
  read_data <- read_html(url)
  ### Position Players
  test1 <- read_data %>%
    html_nodes("#40man") %>%
    html_table()
  
  test1 <- as.data.frame(test1)
  test1 <- test1 %>% select(Name,Var.5)
  test1 <- test1[-nrow(test1),]
  test1 <- test1 %>% filter(Var.5 == "Position")
  ## All Players
  test2 <- read_data %>%
    html_nodes("#appearances") %>%
    html_table()
  
  test2 <- as.data.frame(test2)
  test2 <- test2[-nrow(test2),]
  test2 <- test2 %>% select(Name)
  
  test <- read_data %>% 
    html_nodes("td a") %>%
    html_attr("href")
  test <- data.frame(test)
  test <- filter(test, grepl('/players/',test))
  test[] <- sapply(test,as.character)
  href_vec <- sapply(seq(1:nrow(test)), function(j){
    name <- unlist(strsplit(test[j,],"/"))[4]
    id <- unlist(strsplit(name,"[.]"))[1]
    return(id)
  })
  team_vector <- c(team_vector,rep(val,length(href_vec)))
  href_vector <- c(href_vector,href_vec)
  name_vector <- c(name_vector,test2$Name)
  batting_vector <- c(batting_vector,test1$Name)
}
player_data <- data.frame(name_vector,team_vector,href_vector)
colnames(player_data)[1] <- "Name"
batter_df <- data.frame(batting_vector)
colnames(batter_df) <- "Name"
team_df <- merge(batter_df,player_data,by="Name",stringsAsFactors = FALSE)
colnames(team_df) <- c("Name","Team","href")

#team_df <- data.frame(team_vector,href_vector,name_vector,stringsAsFactors = FALSE)
#names(team_df) <- c("Team","href","Name")
#team_df <- filter(team_df, grepl('01',href))

gamelog_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",team_df$href[1],"&year=2016&t=b#total",sep="")
game_data <- read_html(gamelog_url)
test <- game_data %>% 
  html_nodes("div table") %>% 
  html_table()

data <- data.frame(test[[2]],stringsAsFactors = FALSE)
vec <- as.vector(data[2,])
vec$ID <- team_df$href[1] 

  
for(i in 2:nrow(team_df)){
  gamelog_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",team_df$href[i],"&year=2016&t=b#total",sep="")
  game_data <- read_html(gamelog_url)
  test <- game_data %>% 
    html_nodes("div table") %>% 
    html_table()
  if(length(test)>2){
    data <- data.frame(test[[2]],stringsAsFactors = FALSE)
    vec[i,] <- as.vector(data[2,])
    vec$ID[i] <- team_df$href[i]   
  }
}

vec <- vec %>% na.omit()

name_vec <- character(0)
for(i in 1:nrow(vec)){
  name_vec <- c(name_vec,as.character(team_df$Name[vec$ID[i] == team_df$href]))
}
vec$Name <-name_vec

# Get Names
#g_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",vec$ID[1],"&year=2016&t=b#total",sep="")
#g_url1 <- read_html(g_url)
#test <- g_url1 %>% 
#  html_nodes("#player_name") %>% 
#  html_text()
#vec$Name[1] <- test
#for(k in 2:nrow(vec)){
#  g_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",vec$ID[k],"&year=2016&t=b#total",sep="")
#  g_url1 <- read_html(g_url)
#  test <- g_url1 %>% 
#    html_nodes("#player_name") %>% 
#    html_text()
#  vec$Name[k] <- test
#}

batting_data <- vec %>% 
  select(ID,Name,G,GS,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,BAbip)

write.csv(batting_data,"Weekly_batting.csv")

ggplot(batting_data,aes(x=OBP,y=R)) + 
  geom_point() + 
  geom_text(aes(label=ifelse(R>10,as.character(Name),"")),hjust=-.05) + 
  geom_text(aes(label=ifelse(OBP>0.6,as.character(Name),"")),vjust=-1,hjust=0.5) +
  theme_bw() + 
  labs(title = "OBP vs. R")  + 
  theme(axis.text = element_text(size = 13, face = "bold"), axis.title = element_text(size = 16, color = "grey50", face = "bold"), plot.title = element_text(size = 35, face = "bold", vjust = 1))
  

ggplot(batting_data,aes(x=OPS,y=R)) + 
  geom_point() + 
  geom_text(aes(label=ifelse(R>10,as.character(Name),"")),hjust=-.05) + 
  geom_text(aes(label=ifelse(OPS>1.5,as.character(Name),"")),vjust=-1,hjust=0.5) +
  theme_bw() + 
  labs(title = "OPS vs. R") + 
  theme(axis.text = element_text(size = 13, face = "bold"), axis.title = element_text(size = 16, color = "grey50", face = "bold"), plot.title = element_text(size = 35, face = "bold", vjust = 1))


