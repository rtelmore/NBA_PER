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
    stats.two <- table.stats[2]$`NULL`[-1, ]
    names(stats.two) <- table.stats[2]$`NULL`[1, ]
    df.team <- cbind(stats.one, stats.two[, -(2:3)])
    df.team$team <- team
  }
  else {
    doc <- htmlTreeParse(nba.url, useInternalNodes=T)
    nset.stats <- getNodeSet(doc, "//table[@class=' gSGTable']")    
    table.stats <- lapply(nset.stats, readHTMLTable, header=F)
    stats.one <- table.stats[[1]][-(1:3), ]
    names(stats.one) <- table.stats[[1]][3, ]
    nRecords <- dim(stats.one)[1]
    stats.two <- table.stats[[2]][-(1:3), -(2:3)]
    names(stats.two) <- table.stats[[2]][3, -(2:3)]
    df.team <- cbind(stats.one[-nRecords, ], stats.two[-nRecords, ])
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
write.table(df.statistics, file=out.file, sep=";", col.names=FALSE)

final.string <- paste(Sys.time(), " -- Finished :)", sep="")
print(final.string)

