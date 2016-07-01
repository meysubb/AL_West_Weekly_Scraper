library(XML)
library(dplyr)
library(reshape2)
library(ggplot2)


## Corresponding Divisions 
# div 2 AL EAST
# 3  AL Central
# 4 Al WEST 
# 5 NL EAST
# 6 NL Central
# 7 NL WEST

date_scrape <- function(y,m,d,div) {
  url <- paste0("http://www.baseball-reference.com/games/standings.cgi?year=",y,"&month=",m, "&day=",d,"&submit=Submit+Date")
  d <- readHTMLTable(url, stringsAsFactors = FALSE)
  d <- as.data.frame(d[div])
  d
}

year <- 2016
month <- 6
day <- 26 
div <- 4

date <- paste0(year,"/",month,"/",day)
overall_standings <- date_scrape(year,month,day,div)

# create a complete sequence of dates you want to scrape data for
dates <- as.data.frame(seq(as.Date("2016/04/03"), as.Date(date), by = "weeks"))
names(dates) <- "dates" 

# split the dates so that there are three separate inputs to feed the function
dates <- colsplit(dates$dates, "-", c("y", "m", "d"))

# use the do() function to iterate the scrape function over all the dates

out <- dates %>% group_by(y,m,d) %>% do(date_scrape(.$y, .$m, .$d, 4))

# Sundays between weeks
len <- nrow(dates)
first_sunday <- dates$d[len-1]
last_sunday  <- dates$d[len]

week_record <- out %>% filter(m==month & d>=first_sunday & d<=last_sunday)

# convert values to numeric
week_record[,c(5,6,9,10)] <- sapply(week_record[,c(5,6,9,10)],as.numeric)

weekly <- week_record %>% 
  group_by(Tm) %>% 
  mutate(Week_W= W - lag(W),Week_L = L - lag(L),RS_Week = RS - lag(RS), RA_Week = RA-lag(RA)) %>%
  na.omit() %>% 
  select(Tm,Week_W,Week_L,RS_Week,RA_Week) %>% 
  mutate(W.L = signif(Week_W / (Week_W + Week_L),digits=3))

dates2 <- as.data.frame(seq(as.Date("2016/04/03"), as.Date(date), by = "days"))
names(dates2) <- "days" 

# split the dates so that there are three separate inputs to feed the function
dates2 <- colsplit(dates2$days, "-", c("y", "m", "d"))

# use the do() function to iterate the scrape function over all the dates

out1 <- dates2 %>% group_by(y,m,d) %>% do(date_scrape(.$y, .$m, .$d, 4))


alW_standings_2016 <- ungroup(out1) %>% mutate(Date = paste0(y, sep = "-", m, sep = "-", d)) %>% select(Date, Tm, GB)
alW_standings_2016$GB <- as.numeric(alW_standings_2016$GB) 
alW_standings_2016$Date <- as.Date(alW_standings_2016$Date)
alW_standings_2016$Tm <- as.factor(alW_standings_2016$Tm)

alW_standings_2016$GB <- ifelse(is.na(alW_standings_2016$GB), 0, alW_standings_2016$GB)
# List all team Names
unique(alW_standings_2016$Tm)
# set Team colors
team_colors = c("SEA" = "#01487E", "TEX" = "#0482CC", "HOU" = "#F7742C", "LAA" = "#CA1F2C", "OAK" = "#003300")


ggplot(alW_standings_2016, aes(Date, GB, colour = Tm)) + 
  geom_line(size = 1.25, alpha = .75) + 
  scale_colour_manual(values = team_colors, name = "Team") + 
  scale_y_reverse(breaks = 0:25) + 
  scale_x_date() + 
  geom_text(aes(label=ifelse(Date == "2016-06-26", as.character(GB),'')),hjust=-.5, size = 4, show.legend = FALSE) +
  labs(title = "AL West Race through June 2016") + 
  theme(legend.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 12)) + 
  theme(axis.text = element_text(size = 13, face = "bold"), axis.title = element_text(size = 16, color = "grey50", face = "bold"), plot.title = element_text(size = 35, face = "bold", vjust = 1))

## Save data into csv
write.csv(c(overall_standings,weekly),"Week_Roundup.csv")


## Cumulative Wins above .500

out$W <- as.numeric(out$W)
out$L <- as.numeric(out$L)
out$Tot <- out$W + out$L
out$PDiff <- as.numeric(out$W.L.) - 0.500
out$cum_500 <- ifelse(out$Tot * out$PDiff>0,ceiling(out$Tot * out$PDiff),floor(out$Tot * out$PDiff))

cum_game_500 <- ungroup(out) %>% mutate(Date = paste0(y, sep = "-", m, sep = "-", d)) %>% select(Date, Tm, cum_500)
cum_game_500$Date <- as.Date(cum_game_500$Date)
  
  
ggplot(cum_game_500, aes(Date, cum_500, colour = Tm)) + 
  geom_line(size = 1.25, alpha = .75) + 
  scale_colour_manual(values = team_colors, name = "Team") + 
  scale_x_date() + 
  geom_text(aes(label=ifelse(Date == "2016-05-15", as.character(cum_500),'')),hjust=-.5, size = 4, show.legend = FALSE) +
  labs(title = "AL West Race through May 2016") + 
  theme(legend.title = element_text(size = 12)) + 
  theme(legend.text = element_text(size = 12)) + 
  theme(axis.text = element_text(size = 13, face = "bold"), axis.title = element_text(size = 16, color = "grey50", face = "bold"), plot.title = element_text(size = 35, face = "bold", vjust = 1))
