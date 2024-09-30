df <- read.csv("EFB390_AssignmentForm1.csv")

df$ID <- as.factor(df$ID)

require(reshape2)
require(stringr)

# Specify id.vars: the variables to keep but not split apart on
df_long <-  melt(df, id.vars=c("ID", "Start.time", "Completion.time", "Email"))

df_long$Article <- str_split_fixed(df_long$variable, "_", 2)[,1]
df_long$param <- str_split_fixed(df_long$variable, "_", 2)[,2]

df_longish <- df_long[,c("ID", "value", "Article", "param")]

# From the source:
# "subject" and "sex" are columns we want to keep the same
# "condition" is the column that contains the names of the new column to put things in
# "measurement" holds the measurements
library(reshape2)

data_wide <- dcast(olddata_long, subject + sex ~ condition, value.var="measurement")
df_longish <- dcast(df_longish, ID + Article ~ param, value.var = "value")

df_longish$title <- gsub('<93>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<84>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<91>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<92>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<94>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<e1>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<97>','',df_longish$title) #84, 91,92,93, 94, e1

df_longish$Interaction2 <- "other"
df_longish$Interaction2[df_longish$Interaction == "Conflict"] <- "Conflict" 
df_longish$Interaction2[df_longish$Interaction == "Intervention"] <- "Intervention" 
df_longish$Interaction2[df_longish$Interaction == "Conservation"] <- "Conservation" 

lbls <- paste(names(table(df_longish$Interaction2)), "\n", table(df_longish$Interaction2), sep="")

#pie(table(df_longish$Interaction2), label = lbls)

ggplot(data.frame(table(df_longish$Interaction2)), aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none",
        text = element_text(size=20)) +
  geom_text(aes(label = lbls),
            position = position_stack(vjust = 0.5),
            size = 8) +
  labs(title = "Interaction type:") +
  scale_fill_brewer(palette="Dark2")

write.csv(df_longish, "Assgn1SurveyResponses.csv")

involved <- paste0(df_longish$Involved, collapse = "")
involved <- strsplit(involved, ";")
lbls <- paste(names(table(involved), "\n", table(involved), sep=""))

involved2 <- data.frame(table(involved))
otherRow <- data.frame(involved = "other", Freq = nrow(involved2[involved2$Freq == 1,]))
involved2 <- involved2[involved2$Freq > 1,]
involved2 <- rbind(involved2, otherRow)
involved2$involved <- as.character(involved2$involved)

lbls2 <- paste(names(table(involved2$involved)), "\n", involved2$Freq, sep="")
library(ggplot2)

ggplot(data=involved2, aes(x=involved, y=Freq, fill = involved)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_brewer(palette="Dark2") + 
  theme_bw() +
  theme(legend.position="none",
        text = element_text(size=20)) +
  labs(title = "Who was involved?", x = "", y = "")

# word clouds
