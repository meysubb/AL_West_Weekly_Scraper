library(rvest)


teams <- c("HOU","SEA","TEX","LAA","OAK")
href_vector <- character(0)
team_vector <- character(0)

for(i in 1:length(teams)){
  val <- teams[i]
  url <- paste("http://www.baseball-reference.com/teams/",val,"/2016-roster.shtml",sep="")
  
  read_data <- read_html(url)
  
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
}
team_df <- data.frame(team_vector,href_vector,stringsAsFactors = FALSE)
names(team_df) <- c("Team","href")

team_df <- filter(team_df, grepl('01',href))

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

g_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",vec$ID[1],"&year=2016&t=b#total",sep="")
g_url1 <- read_html(g_url)
test <- g_url1 %>% 
  html_nodes("#player_name") %>% 
  html_text()
vec$Name[1] <- test


# Get Names
for(k in 2:nrow(vec)){
  g_url <- paste("http://www.baseball-reference.com/players/split.cgi?id=",vec$ID[k],"&year=2016&t=b#total",sep="")
  g_url1 <- read_html(g_url)
  test <- g_url1 %>% 
    html_nodes("#player_name") %>% 
    html_text()
  vec$Name[k] <- test
}

batting_data <- vec %>% 
  select(ID,Name,G,GS,PA,AB,R,H,X2B,X3B,HR,RBI,SB,CS,BB,SO,BA,OBP,SLG,OPS,BAbip)

write.csv(batting_data,"Weekly_batting.csv")

