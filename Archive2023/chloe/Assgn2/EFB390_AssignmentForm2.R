df <- read.csv("EFB390 Weekly Assignment #3(1-107).csv")

df$ID <- as.factor(df$ID)

require(reshape2)
require(stringr)

# Specify id.vars: the variables to keep but not split apart on
df_long <-  melt(df[,-c(2:3)], id.vars=c("ID", "Name..first.and.last.."))
names(df_long)[names(df_long) == "Name..first.and.last.."] <- "Name"

df_long$variable <- gsub("Article.1.", "Article1_", df_long$variable)
df_long$variable <- gsub("Article.2.", "Article2_", df_long$variable)
df_long$variable <- gsub("Article.3.", "Article3_", df_long$variable)

df_long$variable <- gsub("upper.confidence.interval.", "Upper.confidence.interval.", df_long$variable)
df_long$variable <- gsub("lower.confidence.interval.", "Lower.confidence.interval.", df_long$variable)
df_long$variable <- gsub("Coefficient.of.variation.or.standard.error.", "Standard.error.or.coefficient.of.variation.", df_long$variable)
df_long$variable <- gsub("standard.error.or.coefficient.of.variation.", "Standard.error.or.coefficient.of.variation.", df_long$variable)

df_long$Article <- str_split_fixed(df_long$variable, "_", 2)[,1]
df_long$param <- str_split_fixed(df_long$variable, "_", 2)[,2]

df_long$param <- gsub("point.estimate..enter.one.number.only..no.dashes...you.can.average.or.select.one.representative.number.if.needed.",
                      "point.estimate", df_long$param)
df_longish <- dcast(df_long[,c("ID", "value", "Article", "param")], ID + Article ~ param, value.var = "value")

df_longish$Name <- df_long$Name[match(df_longish$ID, df_long$ID)]

df_longish <- subset(df_longish, select = c(ID, Name, Article, species., population., point.estimate, 
                                            Did.your.article.report., Lower.confidence.interval., Upper.confidence.interval.,
                                            Standard.error.or.coefficient.of.variation.,
                                            What.are.the.units.of.the.estimate...ind..1000.ind..10.6.ind..ind.km.2..etc..,
                                            What.type.of.estimate.))
df_longish$point.estimate <- as.numeric(df_longish$point.estimate)
hist(df_longish$point.estimate[df_longish$point.estimate < 751527.00], 
     col = "skyblue3", breaks = 40, xlab = "Point Estimate", 
     main = paste0("Distribution of reported population abundace estimates \n without eels, kangaroos, penguins
                   \n median = ",
                   median(df_longish$point.estimate[df_longish$point.estimate < 751527.00], na.rm = TRUE)))
abline(v= median(df_longish$point.estimate[df_longish$point.estimate < 751527.00], na.rm = TRUE),
       col="blue",lwd=2)#vline at median? (623)

df_longish[is.na(df_longish)] <- ""

write.csv(df_longish, "EFB390_Assgn3_Longish.csv", row.names = F)

df_longish$est <- as.numeric(df_longish$point.estimate)
df_longish$Standard.error.or.coefficient.of.variation. <- as.numeric(df_longish$Standard.error.or.coefficient.of.variation.)

plot(log(df_longish$est), 
     log(df_longish$Standard.error.or.coefficient.of.variation.),
     xlab = "Point estimate (log)",
     ylab = "Standard error (log)")

hist(df_longish$est, 
     col = "skyblue3", 
     breaks = 40, xlab = "Point Estimate", 
     main = "Distribution of reported population abundace estimates \n with eels, kangaroos, penguins")

#What type of estimate
require(dplyr)
int <- table(df_longish %>% select("What.type.of.estimate.")) %>% data.frame

int <- int[-1,]

ggplot(data=int, aes(x=What.type.of.estimate., y=Freq, fill = What.type.of.estimate.)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_brewer(palette="Dark2") + 
    theme_bw() +
    theme(legend.position="none",
          text = element_text(size=30)) +
    labs(title = "Estimate type:", x = "", y = "")

#Did your article report
df_longish$Did.your.article.report.[df_longish$Did.your.article.report. %in% "article didn't report" |
                                 df_longish$Did.your.article.report. %in% "none" |
                                 df_longish$Did.your.article.report. %in% "None Stated"] <- "None stated"
df_longish$Did.your.article.report.[df_longish$Did.your.article.report. %in% "p value= .001"] <- "P-value"

int <- table(df_longish %>% select("Did.your.article.report.")) %>% data.frame
int <- int[-1,]

ggplot(data=int[int$Freq > 1,], 
       aes(x=Did.your.article.report., y=Freq, fill = Did.your.article.report.)) +
    geom_bar(stat="identity") +
    coord_flip() +
    scale_fill_brewer(palette="Dark2") + 
    theme_bw() +
    theme(legend.position="none",
          text = element_text(size=30)) +
    labs(title = "Did your article report?", x = "", y = "")

ggplot(data=int, 
       aes(x=Did.your.article.report., y=Freq, fill = Did.your.article.report.)) +
    geom_bar(stat="identity") +
    coord_flip() +
    #scale_fill_brewer(palette="Dark2") + 
    theme_bw() +
    theme(legend.position="none",
          text = element_text(size=20)) +
    labs(title = "Did your article report?", x = "", y = "")
