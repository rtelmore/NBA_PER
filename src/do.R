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

  
r.scrape <- system.time(source(paste(wd.path, "src/scrape.R", sep = "")))

out.string <- paste(Sys.time(), " -- Starting", sep = "")
print(out.string)


wd.path <- "~/Side_Projects/Sports/Scraping/"

years <- seq(2005, 2009, by = 1)

df.final <- data.frame()
for (i in 1:5){
  url.tmp <- paste("http://www.majorleaguesoccer.com/stats/", years[i], 
    sep = "")
  mls.url <- paste(url.tmp, "/reg", sep = "")
  doc <- htmlTreeParse(mls.url, useInternalNodes=T)
  nset.stats <- getNodeSet(doc, 
    "//div/table[@class='stats sortable-first team-totals']")
  table.stats <- lapply(nset.stats, readHTMLTable, header = FALSE)
  nset.attendance <- getNodeSet(doc, 
    "//div/table[@class='sortable-first stats attendance']")
  table.attendance <- lapply(nset.attendance, readHTMLTable, header = FALSE)
  df.stats <- table.stats[[1]]
  df.attendance <- table.attendance[[1]]
  df.stats[order(df.stats$V1), ]
  df.attendance[order(df.attendance$V1), ]
  df.total <- merge(df.stats, df.attendance, by = "V1", all = TRUE)
  df.total$year <- rep(years[i], length(df.total$V1))  
  df.final <- rbind(df.final, df.total)
  out.string.2 <- paste(Sys.time(), paste(" -- Year: ", years[i], sep = ""), 
                      sep = "")
  print(out.string.2)  
}

# source(paste(wd.path, "load.R", sep=""))

out.file <- paste(wd.path, "data/R_", 
              paste(format(Sys.Date(), "%Y%m%d"), ".csv", sep = ""), sep ="")
write.table(df.final, file = out.file, sep = ";", col.names = FALSE)

final.string <- paste(Sys.time(), " -- Finished :)", sep = "")
print(final.string)


p + geom_line(lty = 2) + 
  geom_point(colour="grey60", size = 4) +
  geom_point(aes(colour = type)) +
  scale_x_discrete("timed category") +
  scale_y_continuous("time (in sec)", limits = c(0, 3)) +
  scale_colour_manual("language", values = c("forestgreen", "darkred"))

ggsave(file = paste(wd.path, "fig/time.png", sep = ""), hei = 7, wid = 7)
