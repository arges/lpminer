# Launchpad Data mining in R - Data Exploration
#
# 2012(c)
# Written by Chris J Arges <christopherarges@gmail.com>
#

require(corrplot) 
require(ggplot2)
require(gridExtra)
require(lubridate)
require(portfolio)
require(RColorBrewer)
require(reshape)
require(stats)
require(tm)
require(wordcloud)
require(scales)

# Parameters for visualizations.
log_cols <- c("bug_message_count", "bug_users_affected_count", "bug_users_affected_count_with_dupes",
  "bug_number_of_duplicates", "description_wc", "title_wc", "days_from_created_to_close",
  "days_from_created_to_triaged", "days_from_assigned_to_closed",
  "days_from_triaged_to_assigned", "days_from_created_to_fix_released")
reg_cols <- c("bugtask_status", "bugtask_importance")
all_cols <- c(log_cols, reg_cols, "is_invalid", "is_fix_released")

# Create Labels for Release Dates
release_dates <- as.POSIXct(c(
  "2004-10-20", "2005-04-08", "2005-10-13", "2006-06-01", 
  "2006-10-26", "2007-04-19", "2007-10-18", "2008-04-24",
  "2008-10-30", "2009-04-23", "2009-10-29", "2010-04-29",
  "2010-10-10", "2011-04-28", "2011-10-13", "2012-04-26",
  "2012-10-18"))
dev_dates <- as.POSIXct(c(
  "2001-01-01", "2004-10-20", "2005-04-08", "2005-10-13",
  "2006-06-01", "2006-10-26", "2007-04-19", "2007-10-18",
  "2008-04-24", "2008-10-30", "2009-04-23", "2009-10-29",
  "2010-04-29", "2010-10-10", "2011-04-28", "2011-10-13",
  "2012-04-26"))
release_names <- c(
  "warty", "hoary", "breezy", "dapper",
  "edgy", "feisty", "gutsy", "hardy",
  "intrepid", "jaunty", "karmic", "lucid",
  "maverick", "natty", "oneiric", "precise",
  "quantal")
rls.df <- data.frame(dev_date=dev_dates, date=release_dates, name=release_names)
  
# Colors
status_colors = list(
  "Fix Released"="#008000",
  "Fix Committed"="#005500",
  "In Progress"="#160000",
  "Triaged"="#FF6600",
  "Confirmed"="#FF0000",
  "Won't Fix"="#444444",
  "Invalid"="#666666",
  "Opinon"="#0033AA",
  "Incomplete"="#FF0000",
  "New"="#993300"
)

importance_colors = list(
  "Unknown"="#999999",
  "Critical"="#FF0000",
  "High"="#FF660E",
  "Medium"="#008007",
  "Low"="#270000",
  "Wishlist"="#0000FF",
  "Undecided"="#999999"
)

# WORD CLOUD -------------------------------------------------------------------
# Warning: This is REALLY slow for large datasets.
# Adapted from http://addictedtor.free.fr/graphiques/RGraphGallery.php?graph=162
# http://www.r-bloggers.com/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/
do_wordcloud <- function(data) {
  str.corpus = Corpus(VectorSource(data))
  str.corpus <- tm_map(str.corpus, removePunctuation)
  str.corpus <- tm_map(str.corpus, removeNumbers)
  str.corpus <- tm_map(str.corpus, function(x)removeWords(x,stopwords())) 
  str.corpus <- tm_map(str.corpus, tolower)
  tdm <- TermDocumentMatrix(str.corpus)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  max_terms <- 300
  v.r <- v[seq(1,max_terms)] # reduce the number of terms
  d <- data.frame(word = names(v.r),freq=v.r)
  #pal <- brewer.pal(5,"OrRd")
  #pal <- pal[c(0,1,2,5)]
  wordcloud(d$word,d$freq,max.words=300)
}

# HISTOGRAMS --------------------------------------------------------------------
do_histogram_qplot <- function(bd, column, title) {
  hist_data <- na.omit(data.frame(data=bd[[column]], status=factor(bug_statuses[bd[["bugtask_status"]],]$name,names(status_colors))  ))
  hist_data <- hist_data[which(hist_data$data > 0),]
  s<-summary(hist_data$data)
  return(
  ggplot(hist_data, aes(x=data,fill=status)) +
    geom_bar() + opts(title=paste(title, "( Mean:", s[["Mean"]], ")")) + labs(y="Count", x=title) +
    scale_fill_manual(limits=names(status_colors), values=as.character(status_colors))
  )
}

#--------------------------------------------------------------------------------

# HISTORICAL graph
do_historical_all <- function() {
  # Find minimal/maximal date
  # TODO: Ensure minimal date is : 2003-12-14 20:41:00, found bogus data.
  start_date = trunc(min(bd_all$bugtask_date_created), "day")
  end_date = trunc(max(bd_closed$bugtask_date_closed), "day")
  
  # get the bugtasks that are open on a given date 
  getOpenBugsOnThatDate <- function(dt, bd) {
    return(length(which(
      (bd$bugtask_date_created <= dt & dt < bd$bugtask_date_closed) |
      (bd$bugtask_date_created <= dt & is.na(bd$bugtask_date_closed))
    )))
  }
  
  # get closed bugs
  getClosedBugsOnThatDate <- function(dt, bd) {
    return(length(which(bd$bugtask_date_closed <= dt)))
  }
  
    # get closed bugs
  getFixReleasedBugsOnThatDate <- function(dt, bd) {
    return(length(which((bd$bugtask_date_closed <= dt) & (bd$bugtask_status == 11))))
  }
  
  # Get bugs by range of dates.
  getBugsCreatedWithinDates <- function(start, end) {
    return(which(start <= bd_all$bugtask_date_created & bd_all$bugtask_date_created < end))
  }

  # Split bugs by series.
  series <- data.frame(name=release_names)
  series$bugs<-by(rls.df, 1:nrow(rls.df), function(row){getBugsCreatedWithinDates(row$dev_date, row$date)})
  
  # Create bug history.
  bd_history <- data.frame(date = seq(from = start_date, to = end_date, by = "days"))
  bd_history$total <- lapply(bd_history$date, getOpenBugsOnThatDate, bd=bd_all)
  
  # Get closed bugs
  bd_history$closed <- lapply(bd_history$date, getClosedBugsOnThatDate, bd=bd_all)
  
  bd_history$closed_fixed <- lapply(bd_history$date, getFixReleasedBugsOnThatDate, bd=bd_all)


  
  # Create bug history per series.
  for (idx in row(series[1])) {
    #idx <- which(series[1] == as.character(name))
    name<-as.character(series[1][idx,])
    bd_history[[name]] <- lapply(bd_history$date, getOpenBugsOnThatDate, bd=bd_all[unlist(series[2][idx,]),])
  }

  # http://sape.inf.usi.ch/quick-reference/ggplot2/geom_vline
  # Plot data
  p1<-ggplot(bd_history, aes(x=date, label=date)) + 
    geom_line(aes(y=as.numeric(warty), col="Warty")) +  
    geom_line(aes(y=as.numeric(hoary), col="Hoary")) +  
    geom_line(aes(y=as.numeric(breezy), col="Breezy")) +  
    geom_line(aes(y=as.numeric(dapper), col="Dapper"), size=1.5) +  
    geom_line(aes(y=as.numeric(edgy), col="Edgy")) +  
    geom_line(aes(y=as.numeric(feisty), col="Feisty")) +  
    geom_line(aes(y=as.numeric(gutsy), col="Gutsy")) +  
    geom_line(aes(y=as.numeric(hardy), col="Hardy"), size=1.5) +  
    geom_line(aes(y=as.numeric(lucid), col="Intrepid")) +  
    geom_line(aes(y=as.numeric(jaunty), col="Jaunty")) +  
    geom_line(aes(y=as.numeric(karmic), col="Karmic")) +  
    geom_line(aes(y=as.numeric(lucid), col="Lucid"), size=1.5) +  
    geom_line(aes(y=as.numeric(maverick), col="Maverick")) +  
    geom_line(aes(y=as.numeric(natty), col="Natty")) +  
    geom_line(aes(y=as.numeric(oneiric), col="Oneiric")) + 
    geom_line(aes(y=as.numeric(precise), col="Precise"), size=1.5) +  
    geom_line(aes(y=as.numeric(quantal), col="Quantal")) +  
    geom_text(data=rls.df, mapping=aes(x=as.numeric(date), y=0, label=name), angle=90, size=4, vjust=-0.4, hjust=0) +
    opts(title="Bugs Opened During Each Series Development Cycle") + labs(y="Count", x="Date") +
    geom_vline(xintercept=as.numeric(release_dates), color="darkgray", linetype="dashed") +
    scale_colour_discrete(name="Data")
  plot(p1)
  
  # Get person data
  people_history <- data.frame(date = seq(from = start_date, to = end_date, by = "days"))
  people_history$total <- lapply(people_history$date, function(dt) { z<- length(which(people$date_created <= dt)) } )
  
  # http://sape.inf.usi.ch/quick-reference/ggplot2/geom_vline
  # Plot data
  p1<-ggplot(bd_history, aes(x=date, label=date)) + 
    geom_line(aes(y=as.numeric(total), col="Open Bugs"), size=1.5 ) +
    geom_line(aes(y=as.numeric(closed), col="Closed Bugs"), size=1.5) +
    geom_line(aes(y=as.numeric(closed_fixed), col="Fix Released Bugs"), size=1.5) +
    geom_line(data=people_history, aes(y=as.numeric(total),col="LP Users"), linetype="dashed") +
    geom_text(data=rls.df, mapping=aes(x=as.numeric(date), y=0, label=name), angle=90, size=4, vjust=-0.4, hjust=0) +
    opts(title="All Open Bugs Historically") + labs(y="Count", x="Date") +
    geom_vline(xintercept=as.numeric(release_dates), color="gray", linetype="dashed") +
    scale_colour_manual(name="Data", values=c("yellow", "green", "blue", "red")) +
    scale_y_continuous(labels=comma)
  plot(p1)
  
}

do_correlation <- function() {
  cols <- c("bug_message_count", "bug_users_affected_count", "bug_users_affected_count_with_dupes",
    "bug_number_of_duplicates", "description_wc", "title_wc", "days_from_created_to_close","bugtask_importance",  

  # Sort bugs by series.
    "is_invalid", "is_won't_fix", "is_expired","is_fix_released")        
  bd_c.corr <- cor(bd_closed[cols])
  corrplot(bd_c.corr, method="shade")
}

do_find_most_frequent_tags <- function(N, bd) {
  # Get non-empty tags
  bd.non_empty <- which(bd$bug_tags != "")
  bd.tags <- bd[bd.non_empty,]$bug_tags
  tags_list <- unlist(strsplit(bd.tags,perl=TRUE, ","))
  return (head(sort(table(tags_list),decreasing=TRUE),N))
}

panel_values <- function(...) {
  # http://stackoverflow.com/questions/3220702/display-values-in-stacked-lattice-barchart
  # Rather convoluted way to add value labels to a barchart.
  panel.barchart(...) 
  tmp <- list(...)
  tmp <- data.frame(x=tmp$x, y=tmp$y)

  # calculate positions of text labels
  df <- ddply(tmp, .(y),function(x) {
    data.frame(x, pos=0)
  })
  panel.text(x=df$pos, y=df$y,label=df$x,cex=0.8,pos=2,side=4)
}

do_plot_barchart <- function(data,title,ylab,xlab) {
  df<-data.frame(names=factor(names(data),names(data)),value=data)
  p<-ggplot(df, aes(x=names,y=value,fill=names)) + geom_bar() + 
    geom_text(mapping=aes(x=names, y=0, label=value), size=3, vjust=2, hjust=0.5) +
    opts(title=title) + labs(y=ylab, x=xlab) + guides(fill=FALSE)
  return(p)
 }

do_treemap <- function() {
  data<-data.frame(bd_closed)
  map.market(id=data[["id"]], area=data[["days_from_created_to_close"]]+1, group=as.character(data[["package_type"]]), color=data[["bugtask_importance"]], main="Bugs")
}

do_plots <- function() {
  # Histogram Plots
  p1<-do_histogram_qplot(bd_all, "bug_message_count","Bug Messages Count") + scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,1000)) + guides(fill=FALSE)
  p2<-do_histogram_qplot(bd_all, "bug_users_affected_count_with_dupes", "Users Affected") + scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,1000)) + guides(fill=FALSE)
  p3<-do_histogram_qplot(bd_all, "bug_number_of_duplicates", "Number of Duplicates") + scale_x_log10(breaks=c(0,1,2,5,10,20,50,100,1000)) + guides(fill=FALSE)
  p4<-do_histogram_qplot(bd_all, "description_wc", "Description Character Count") + scale_x_log10(breaks=c(3,50,300,1000,10000,50000)) + guides(fill=FALSE)
  p5<-do_histogram_qplot(bd_all, "title_wc", "Title Character Count") + scale_x_log10(breaks=c(2,10,25,50,100,250,500)) + guides(fill=FALSE)
  p6<-do_histogram_qplot(bd_all, "bug_heat", "Bug Heat") + scale_x_log10(breaks=c(2,10,25,250))
  grid.arrange(p1,p2,p3,p4,p5,p6,nrow=3,ncol=2)
  
  p7<-(do_histogram_qplot(bd_closed, "days_from_created_to_close", "Days From Created To Closed")+scale_x_log10(breaks=c(0,1,2,5,10,30,125,2500)))   + guides(fill=FALSE) 
  p8<-(do_histogram_qplot(bd_closed, "days_from_created_to_assigned", "Days From Created To Assigned")+scale_x_log10(breaks=c(0,0.01,0.25,2,50,2000))) + guides(fill=FALSE)
  p9<-(do_histogram_qplot(bd_closed, "days_from_assigned_to_closed", "Days From Assigned To Closed")+scale_x_log10(breaks=c(0,1,2,10,100,2000))) + guides(fill=FALSE)	
  p10<-(do_histogram_qplot(bd_closed, "days_from_created_to_triaged", "Days From Created To Triaged")+scale_x_log10(breaks=c(0,0.01,0.25,2,70,2000)))  + guides(fill=FALSE)
  p11<-(do_histogram_qplot(bd_closed, "days_from_triaged_to_assigned", "Days From Triaged To Assigned")+scale_x_log10(breaks=c(0,0.01,3,25,500))) + guides(fill=FALSE)
  p12<-(do_histogram_qplot(bd_closed, "days_from_created_to_fix_released", "Days From Created To Fix Released") + scale_x_log10(breaks=c(0,1,5,25,150,2000))) 
  grid.arrange(p7,p8,p9,p10,p11,p12,nrow=3,ncol=2)

  do_correlation()
  
  weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  clt<-table(bd_closed$bugtask_weekday_created)[weekdays]
  crt<-table(bd_closed$bugtask_weekday_closed)[weekdays]

  a<-data.frame(Weekday=factor(weekdays, weekdays), Closed=as.vector(clt), Created=as.vector(crt))
  mx<-melt(a, id.vars=1)
  p0<-ggplot(mx, aes(x=Weekday, y=value, fill=variable)) + geom_bar(stat="identity",position=position_dodge())  + scale_colour_discrete(name="Bug Activity") +
      opts(title="Bug Activity Per Day of Week") + labs(y="Count", x="Weekday")
  plot(p0)
  
  # bar plots
  p1<-do_plot_barchart(head(sort(table(as.factor(as.character(bd_all[["package_type"]]))),decreasing=TRUE),10),"Most Frequent Bugs Per Team Type","Count","Team Type")
  p2<-do_plot_barchart(sort(table(bug_statuses[bd_all[["bugtask_status"]],]$name),decreasing=TRUE),"Number of Bugtasks By Status","","Status")    
  p3<-do_plot_barchart(sort(table(bug_importances[bd_all[["bugtask_importance"]],]$name),decreasing=TRUE),"Number of Bugtasks By Importance","Count","Importance")
  vals_all<-do_find_most_frequent_tags(10, bd_all)
  p4<-do_plot_barchart(vals_all,"Most Frequent Tags in All Bugs","","Bug Tag")
  grid.arrange(p1,p2,nrow=2)
  grid.arrange(p3,p4,nrow=2)
}

# Do visualizations here! ------------------------------------------
do_visualizations <- function() {
  #pdf(file="plot%03d.pdf",onefile=FALSE,paper="special",pointsize=9,width=10,height=16)
  #pdf(file="plot.pdf", pointsize=9, width=16,height=12)
  png(file="plot%03d.png",pointsize=48, width=1080, height=768)

  #do_plots()
  do_historical_all()

  # the following plots take a long time to produce (these CAN lock up your machine)
  #do_treemap()
  #do_wordcloud(bd_all)

  dev.off()
}
