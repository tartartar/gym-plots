gym_orig <- read.csv("gymusage.csv")
# > head(gym_orig, 1)
#            Entry.Date           Exit.Date Visit.Duration
# 1 28/11/2016 20:25:16 28/11/2016 21:20:22        55 mins

convertToTime <- function(x) {
  as.POSIXct(x, tz = "", format = "%d/%m/%Y %T")
}

gym <- lapply(gym_orig[1], convertToTime)
gym[2] <- lapply(gym_orig[2], convertToTime)

ripMins <- function(x) {
  gsub(" mins", "", x)
}

gym[3] <- lapply(gym_orig[3], ripMins)

gym[[3]] <- as.numeric(gym[[3]])

gym <- data.frame(gym)

names(gym)[1] <- "Entry date"
names(gym)[2] <- "Exit date"
names(gym)[3] <- "Duration (in mins)"

minsToHrs <- function(x){
  round(x / 60, 1)
}

gym[4] <- lapply(gym[3], minsToHrs)
names(gym)[4] <- "Duration (in hours)"

library(ggplot2)
library(scales)

# Date vs Duration (mins)
date_vs_duration_dotplot <- ggplot(gym, aes(x = gym[1], y = gym[3])) + 
  geom_point() +
  scale_x_datetime(names(gym)[1], date_breaks = "2 weeks", date_labels = "%d %b") + 
  scale_y_continuous(names(gym)[3], limits = c(0, max(gym[3])*1.1), breaks = seq(0, max(gym[3])*1.1, 10), expand = c(0,0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # stat_smooth(method = "lm", se = F, col = "red")

gym[5] <- as.factor(format(gym[[1]], format = "%a"))
names(gym)[5] <- "Day of week"

# # Entry day vs duration, jitterplot
# entry_vs_duration_jitterplot <- ggplot(gym, aes(x = gym[5], y = gym[3])) + 
#   scale_y_continuous(names(gym)[3], limits = c(0, max(gym[3])*1.1), breaks = seq(0, max(gym[3])*1.1, 10), expand = c(0,0)) +
#   geom_jitter(position = position_jitter(width = 0.1), alpha = 0.5) +
#   scale_x_discrete("Entry day", limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) + # providing an explicit factor ordering
#   theme_bw()

# Entry day vs duration, boxplot
entry_vs_duration_boxplot <- ggplot(gym, aes(x = gym$`Day of week`, y = gym$`Duration (in mins)`)) +
  scale_y_continuous(names(gym)[3], limits = c(0, max(gym[3])*1.1), breaks = seq(0, max(gym[3])*1.1, 10), expand = c(0,0)) +
  geom_boxplot(fill = "grey80") +
  scale_x_discrete("Entry day", limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) + # providing an explicit factor ordering
  theme_bw()

# Entry day vs duration, violin plot
entry_vs_duration_violinplot <- ggplot(gym, aes(x = gym$`Day of week`, y = gym$`Duration (in mins)`)) +
  geom_violin(fill = "grey80", alpha = 0.5) +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.5) +
  scale_y_continuous(names(gym)[3], limits = c(0, max(gym[3])*1.1), breaks = seq(0, max(gym[3])*1.1, 10), expand = c(0,0)) +
  scale_x_discrete("Entry day", limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) + # providing an explicit factor ordering
  theme_bw()

library(plyr)
stat_days <- count(gym$`Day of week`)
stat_days$percent <- paste(round(stat_days$freq / sum(stat_days$freq)*100, 1), sep = "", "%")

# Count of entries by day of the week
entry_by_day <- ggplot(gym, aes(x = gym[5])) +
  geom_bar(fill = "grey70") +
  scale_y_continuous("Count of entries", limits = c(0,max(stat_days$freq)+2), breaks = c(0:max(stat_days$freq)+2, 3), expand = c(0,0)) +
  scale_x_discrete("Entry day", limits=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")) + # providing an explicit factor ordering
  geom_text(data = stat_days, aes(x = stat_days$x, y = stat_days$freq + 0.4, label = stat_days$percent)) +
  theme_bw()

# Histogram of duration (mins)
duration_histogram <- ggplot(gym, aes(x = gym$`Duration (in mins)`)) + 
  geom_histogram(binwidth = 5, fill = "grey70", col = "black") +
  scale_x_continuous("Duration (in mins)", breaks = seq(round(min(gym$`Duration (in mins)`),-1), round(max(gym$`Duration (in mins)`),-1) + 5, 5)) + 
  scale_y_continuous("Count of entries", breaks = c(0:max(stat_days$freq)), limits = c(0,max(stat_days$freq)), expand = c(0,0)) +
  theme_bw()

# ggplot(stat_days, aes(x = "", y = stat_days$freq, fill = stat_days$x)) +
#   geom_bar(stat = "identity") + coord_flip() +
#   scale_fill_brewer(palette="Dark2") +
#   geom_text(aes(y = stat_days$freq/7 + c(0, cumsum(stat_days$freq)[-length(stat_days$freq)]), 
#                 label = percent(stat_days$freq/100)), size=5)


pdf("date_vs_duration_dotplot.pdf")
print(date_vs_duration_dotplot)
dev.off()

pdf("duration_histogram.pdf")
print(duration_histogram)
dev.off()

pdf("entry_by_day.pdf")
print(entry_by_day)
dev.off()

pdf("entry_vs_duration_boxplot.pdf")
print(entry_vs_duration_boxplot)
dev.off()

# pdf("entry_vs_duration_jitterplot.pdf")
# print(entry_vs_duration_jitterplot)
# dev.off()

pdf("entry_vs_duration_violinplot.pdf")
print(entry_vs_duration_violinplot)
dev.off()

# write.csv(gym, file = "gym.csv")
