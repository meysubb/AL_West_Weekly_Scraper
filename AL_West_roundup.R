library(XML)
library(lubridate)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

## Corresponding Divisions 
# AL EAST
# AL Central
# Al WEST (AL-W)
# NL EAST
# NL Central
# NL WEST

date_scrape <- function(y,m,d,div) {
  div <- paste0("standings-upto-",div)
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[div])
  rm_cn <- paste0(gsub("-",".",div),".")
  colnames(d) <- gsub(rm_cn,"",colnames(d))
  d
}


current_date <- Sys.Date()
year <- year(current_date)
month <- month(current_date)
day <- day(current_date)


multiple_date_scrape <- function(length,interest_date){
  # Length should be a character of "weeks" or "days
  # Seq of Dates
  date_df <- as.data.frame(seq(as.Date("2017/04/02"), interest_date, by = length))
  names(date_df) <- "dates" 
  date_df <- colsplit(date_df$dates, "-", c("y", "m", "d"))
  standings <- date_df %>% group_by(y,m,d) %>% do(date_scrape(.$y, .$m, .$d, "AL-W"))
  standings
}


out <- multiple_date_scrape("weeks",current_date)
# Sundays between weeks
len <- length(unique(out$d))
first_sunday <- unique(out$d)[len-1]
last_sunday  <- unique(out$d)[len]

## Need to do something for month checks. 
week_record <- out %>% filter(d==first_sunday | d==last_sunday)

# convert values to numeric
week_record[,c(5,6,9,10)] <- sapply(week_record[,c(5,6,9,10)],as.numeric)
week_record$Tm <- as.factor(week_record$Tm)

## Group-By has errors when plyr is loaded
detach("package:plyr", unload=TRUE)
weekly <- week_record %>% 
  group_by(Tm) %>% 
  mutate(Week_W= W - lag(W),
         Week_L = L - lag(L),
         RS_Week = RS - lag(RS), 
         RA_Week = RA-lag(RA)) %>%
  na.omit() %>% 
  select(Tm,Week_W,Week_L,RS_Week,RA_Week) %>% 
  mutate(W.L = signif(Week_W / (Week_W + Week_L),digits=3)) %>% 
  mutate(pythW.L. = signif(RS_Week^1.83/(RS_Week^1.83 + RA_Week^1.83),digits=3)) %>% 
  arrange(desc(Week_W))

overall_standings <- date_scrape(year,month,day,div)
## Save data into csv
write.csv(c(overall_standings,weekly),"Week_Roundup.csv")

### Create Plot
out1 <- multiple_date_scrape("days",current_date)

## Clean up
alW_standings_2017 <-
  ungroup(out1) %>% mutate(Date = paste0(y, sep = "-", m, sep = "-", d)) %>% select(Date, Tm, GB)
alW_standings_2017$GB <- as.numeric(alW_standings_2017$GB) 
alW_standings_2017$Date <- as.Date(alW_standings_2017$Date)
alW_standings_2017$Tm <- as.factor(alW_standings_2017$Tm)

alW_standings_2017$GB <- ifelse(is.na(alW_standings_2017$GB), 0, alW_standings_2017$GB)
# set Team colors
team_colors = c("SEA" = "#01487E", "TEX" = "#0482CC", "HOU" = "#F7742C", "LAA" = "#CA1F2C", "OAK" = "#003300")


ggplot(alW_standings_2017, aes(Date, GB, colour = Tm)) + 
  geom_line(size = 1.25, alpha = .75) + 
  scale_colour_manual(values = team_colors, name = "Team") + 
  scale_y_reverse(breaks = 0:25) + 
  scale_x_date() + 
  geom_text(aes(label=ifelse(Date == as.character(current_date), as.character(GB),'')),hjust=-.35, size = 4, show.legend = FALSE) +
  labs(title = paste("AL West Race through",month.abb[month(current_date)],year(current_date)),
       caption = "@msubbaiah1") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        panel.grid.major.x = element_line(colour="grey", size=0.5))
  


## Cumulative Wins above .500

out$W <- as.numeric(out$W)
out$L <- as.numeric(out$L)
out$cum_500 <- with(out,W-L)
cum_game_500 <- ungroup(out) %>% mutate(Date = paste0(y, sep = "-", m, sep = "-", d)) %>% select(Date, Tm, cum_500)
cum_game_500$Date <- as.Date(cum_game_500$Date)

  
ggplot(cum_game_500, aes(Date, cum_500, colour = Tm)) + 
  geom_line(size = 1.25, alpha = .75) + 
  geom_hline(yintercept = 0) + 
  scale_colour_manual(values = team_colors, name = "Team") + 
  scale_x_date() + 
  geom_text(aes(label=ifelse(Date == as.character(current_date), as.character(cum_500),'')),hjust=-.35, size = 4, show.legend = FALSE) +
  labs(title = paste("AL West Race through",month.abb[month(current_date)],year(current_date)),
       y="Cumulative games over .500",
       caption = "@msubbaiah1") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        panel.grid.major.x = element_line(colour="grey", size=0.5))


## Try to Animate the GB's plot.
library(gganimate)

p <- ggplot(alW_standings_2017,aes(Date,GB,color=Tm)) + 
  geom_line(aes(frame = Date,cumulative = TRUE), data = alW_standings_2017,size = 1.25, alpha = .75) + 
  scale_colour_manual(values = team_colors, name = "Team") + 
  scale_y_reverse(breaks = 0:25) + 
  scale_x_date() + 
  labs(title = "AL West Race through",
       caption = "@msubbaiah1") +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour="grey", size=0.5),
        panel.grid.major.x = element_line(colour="grey", size=0.5)) + 
  coord_fixed()

gganimate(p,interval=0.5,"Animated_gb.gif",ani.width=1000,ani.height=300)

  