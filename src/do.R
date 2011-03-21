## Ryan Elmore
## 15 March 2011
## NBA Stuff

## Copyright (c) 2011, under the Simplified BSD License.  
## For more information on FreeBSD see: 
##      http://www.opensource.org/licenses/bsd-license.php
## All rights reserved.                      

wd.path <- "~/Side_Projects/Sports/NBA/NBA_PER/"

## Dependencies:
source(paste(wd.path, "src/load.R", sep = ""))

  

out.string <- paste(Sys.time(), " -- Starting", sep = "")
print(out.string)


wd.path <- "~/Side_Projects/Sports/NBA/NBA_PER/"

# teams <- c("hawks", "celtics")
teams <- c("hawks", "celtics", "bobcats", "bulls", "cavaliers", "mavericks",
          "nuggets", "pistons", "warriors", "rockets", "pacers", "clippers",
          "lakers", "grizzlies", "heat", "bucks", "timberwolves", "nets", 
          "hornets", "knicks", "thunder", "magic", "sixers", "suns", "blazers",
          "kings", "spurs", "raptors", "jazz", "wizards")

teams <- c("bulls")
df.statistics <- data.frame()

for (team in teams){
  url.tmp <- paste("http://www.nba.com/", team, sep = "")
  nba.url <- paste(url.tmp, "/stats/", sep = "")
  # celts <- "~/Side_Projects/Sports/NBA/NBA_PER/data/celts.html"
  doc <- htmlTreeParse(nba.url, useInternalNodes=T)
  # doc <- htmlTreeParse(celts, useInternalNodes=T)
  nset.stats <- getNodeSet(doc, 
    "//table[@class=' gSGTable']")
  table.stats <- lapply(nset.stats, readHTMLTable, header = F)
  stats.one <- table.stats[[1]][-(1:3), ]
  names(stats.one) <- table.stats[[1]][3, ]
  nRecords <- dim(stats.one)[1]
  stats.one$team <- team
  stats.two <- table.stats[[2]][-(1:3), -(1:2)]
  names(stats.two) <- table.stats[[2]][3, -(1:2)]
  df.team <- cbind(stats.one[-nRecords, ], stats.two[-nRecords, -1])
  df.statistics <- rbind(df.statistics, df.team)
  out.string.2 <- paste(Sys.time(), paste(" -- Team: ", team, sep = ""), 
                      sep = "")
  print(out.string.2)  
}

out.file <- paste(wd.path, "data/nba_", 
              paste(format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), sep ="")
write.table(df.statistics, file = out.file, sep = ";", col.names = FALSE)

final.string <- paste(Sys.time(), " -- Finished :)", sep = "")
print(final.string)


# p + geom_line(lty = 2) + 
#   geom_point(colour="grey60", size = 4) +
#   geom_point(aes(colour = type)) +
#   scale_x_discrete("timed category") +
#   scale_y_continuous("time (in sec)", limits = c(0, 3)) +
#   scale_colour_manual("language", values = c("forestgreen", "darkred"))
# 
# ggsave(file = paste(wd.path, "fig/time.png", sep = ""), hei = 7, wid = 7)
