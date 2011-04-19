## Ryan Elmore
## 15 March 2011
## NBA Stuff

## Copyright (c) 2011, under the Simplified BSD License.  
## For more information on FreeBSD see: 
##      http://www.opensource.org/licenses/bsd-license.php
## All rights reserved.                      

wd.path <- "~/Side_Projects/Sports/NBA/NBA_PER/"

## Dependencies:
source(paste(wd.path, "src/load.R", sep=""))

  

out.string <- paste(Sys.time(), " -- Starting", sep="")
print(out.string)


# teams <- c("hawks", "celtics", "bulls")
teams <- c("hawks", "celtics", "bobcats", "bulls", "cavaliers", "mavericks",
          "nuggets", "pistons", "warriors", "rockets", "pacers", "clippers",
          "lakers", "grizzlies", "heat", "bucks", "timberwolves", "nets", 
          "hornets", "knicks", "thunder", "magic", "sixers", "suns", "blazers",
          "kings", "spurs", "raptors", "jazz", "wizards")

df.statistics <- data.frame()

for (team in teams){
  url.tmp <- paste("http://www.nba.com/", team, sep="")
  nba.url <- paste(url.tmp, "/stats/", sep="")
  if(team %in% c("bulls", "raptors")) {
    table.stats <- readHTMLTable(nba.url)
    stats.one <- table.stats[1]$`NULL`[-1, ]
    names(stats.one) <- table.stats[1]$`NULL`[1, ]
    stats.one$Player[stats.one$Player == "Team Averages"] <- "Team Statistics"
    stats.two <- table.stats[2]$`NULL`[-1, ]
    names(stats.two) <- table.stats[2]$`NULL`[1, ]
    stats.two$Player[stats.two$Player == "Team Totals"] <- "Team Statistics"
    df.team <- merge(stats.one, stats.two[, -(2:3)], by.x="Player", 
                     by.y="Player")
    df.team$team <- team
  }
  else {
    doc <- htmlTreeParse(nba.url, useInternalNodes=T)
    nset.stats <- getNodeSet(doc, "//table[@class=' gSGTable']")    
    table.stats <- lapply(nset.stats, readHTMLTable, header=F)
    stats.one <- table.stats[[1]][-(1:3), ]
    names(stats.one) <- table.stats[[1]][3, ]
    stats.one$Player[stats.one$Player == "Team Averages"] <- "Team Statistics"
    nRecords <- dim(stats.one)[1]
    stats.two <- table.stats[[2]][-(1:3), -(2:3)]
    names(stats.two) <- table.stats[[2]][3, -(2:3)]
    stats.two$Player[stats.two$Player == "Team Totals"] <- "Team Statistics"
    df.team <- merge(stats.one[-nRecords, ], stats.two[-nRecords, ], 
                     by.x="Player", by.y="Player")
    df.team$team <- team
  }
  df.statistics <- rbind(df.statistics, df.team)
  out.string.2 <- paste(Sys.time(), paste(" -- Team: ", team, sep=""), 
                      sep="")
  print(out.string.2)  
}

out.file <- paste(wd.path, "data/nba_", 
                  paste(format(Sys.Date(), "%Y%m%d"), ".csv", sep=""), 
                  sep ="")
write.table(df.statistics, file=out.file, sep=";", col.names=TRUE)


## grep("(TOT)", df.statistics$Player)

## Get the team data
standings.url <- 
  "http://www.nba.com/standings/team_record_comparison/conferenceNew_Std_Alp.html"

table.standings <- readHTMLTable(standings.url)[[3]]
names(table.standings) <- table.standings[2, ]
team.standings <- table.standings[-(1:2), ]
out.team <- paste(wd.path, "data/team_", 
                  paste(format(Sys.Date(), "%Y%m%d"), ".csv", sep=""), 
                  sep ="")
write.table(team.standings, file=out.team, sep=";", col.names=TRUE)

team.standings$mascot <- unique(df.statistics$team)



## Read in Data
df.statistics <- read.csv(paste(wd.path, "data/nba_20110329.csv", sep=""), 
                          sep=";")
team.standings <- read.csv(paste(wd.path, "data/team_20110329.csv", sep=""),
                           sep=";")
team.dat <- df.statistics[grep("Team Statistics", df.statistics$Player), ]
opp.dat <- df.statistics[grep("Opponents", df.statistics$Player), ]
#team.dat$mascot <- unique(df.statistics$team)
team.dat$win <- as.numeric(team.standings$W)
team.dat$loss <- as.numeric(team.standings$L)

total.dat <- merge(team.dat, opp.dat, by.x="team", by.y="team")
total.dat[, "APG.x"] <- as.numeric(total.dat$APG.x)
total.dat[, "SPG.x"] <- as.numeric(total.dat$SPG.x)
total.dat[, "BPG.x"] <- as.numeric(total.dat$BPG.x)

## Model for winning %age
win.glm.1 <- glm(cbind(as.numeric(win),as.numeric(loss)) ~ as.numeric(APG.x) + 
                 as.numeric(SPG.x) + as.numeric(BPG.x) + as.numeric(OFF.x.x) +
                 as.numeric(SPG.y) + as.numeric(BPG.y) + as.numeric(OFF.y.y),
                 family=binomial, data=total.dat)

win.glm.2 <- glm(cbind(as.numeric(win), as.numeric(loss)) ~
                 as.numeric(FG..x) + as.numeric(X3p..x) + as.numeric(FT..x) +
                 as.numeric(FG..y) + as.numeric(X3p..y) + as.numeric(FT..y),
                 family=binomial, data=total.dat)

## Models for wins
pairs(total.dat[, c("win", "APG.x", "BPG.x", "SPG.x")])

win.lm.1 <- lm(win ~ APG.x + BPG.x + SPG.x, data=total.dat)

win.glm.1 <- glm(cbind(as.numeric(win),as.numeric(loss)) ~ as.numeric(APG.x) + 
                 as.numeric(SPG.x) + as.numeric(BPG.x) + as.numeric(OFF.x.x) +
                 as.numeric(TO.x.x) + as.numeric(PF.x.x) +
                 as.numeric(SPG.y) + as.numeric(BPG.y) + as.numeric(OFF.y.y) +
                 as.numeric(TO.x.y) + as.numeric(PF.x.y) +
                 as.numeric(APG.y), family=binomial, data=total.dat)

+ as.numeric(PPG.x)
+ as.numeric(PPG.y) 
summary(win.glm.1)
final.string <- paste(Sys.time(), " -- Finished :)", sep="")
print(final.string)

## Mean substitution
set.seed(1234)
x <- data.frame(x=round(rnorm(500, m=69, s=3)))

x.mean <- round(mean(x$x))

mean(c(x$x, rep(x.mean, 500)))

var(x)
var(c(x$x, rep(x.mean, 500)))

p <- ggplot(data=x, aes(x))
p + geom_density() +
  scale_x_continuous("height (in inches)")
  
ggsave("../fig/height.png", hei=7, wid=7)

xx <- data.frame(x=c(rep(x$x, 2), rep(x.mean, 500)), 
                 dataset=c(rep("original", 500), rep("imputed", 1000)))

p <- ggplot(data=xx, aes(x, fill=dataset))
p + geom_density(alpha = I(0.35)) +
  scale_x_continuous("height (in inches)")
  
ggsave("../fig/height_total.png", hei=, wid=7)

  