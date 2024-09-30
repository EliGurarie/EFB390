df <- read.csv("EFB390Assignment5.csv")

df$ID <- as.factor(df$ID)

require(reshape2)
require(stringr)

# Specify id.vars: the variables to keep but not split apart on
df_long <-  melt(df[,-c(2:5)], id.vars=c("ID", "Name..first.and.last.."))
names(df_long)[names(df_long) == "Name..first.and.last.."] <- "Name"

df_long$variable <- gsub("Article.1.", "Article1_", df_long$variable)
df_long$variable <- gsub("Article.2.", "Article2_", df_long$variable)

df_long$variable <- gsub("habitat.definition..if.provided..", "HabitatDefinition", df_long$variable)
df_long$variable <- gsub("DOI..Digital.object.identifier..can.be.found.in.Zotero.", "DOI", df_long$variable)
df_long$variable <- gsub("score.", "score", df_long$variable)
df_long$variable <- gsub("species.", "species", df_long$variable)

df_long$value <- gsub('<93>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<84>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<91>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<92>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<94>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<e1>','',df_long$value) #84, 91,92,93, 94, e1
df_long$value <- gsub('<97>','',df_long$value) #84, 91,92,93, 94, e1

df_long$Article <- str_split_fixed(df_long$variable, "_", 2)[,1]
df_long$param <- str_split_fixed(df_long$variable, "_", 2)[,2]

df_longish <- dcast(df_long[,c("ID", "value", "Article", "param")], ID + Article ~ param, value.var = "value")

df_longish$title <- gsub('<93>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<84>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<91>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<92>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<94>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<e1>','',df_longish$title) #84, 91,92,93, 94, e1
df_longish$title <- gsub('<97>','',df_longish$title) #84, 91,92,93, 94, e1


df_longish$Name <- df_long$Name[match(df_longish$ID, df_long$ID)]

df_longish$HabitatDefined <- 1
df_longish$HabitatDefined[df_longish$HabitatDefinition == "N/A" |
                            df_longish$HabitatDefinition == "" |
                            df_longish$HabitatDefinition == "not provided" |
                            df_longish$HabitatDefinition == "n/a" |
                            df_longish$HabitatDefinition == "No definition" |
                            df_longish$HabitatDefinition == "No definition provided, but focused on geographical location as well as resources." |
                            df_longish$HabitatDefinition == "No definition provided, focused on vegetation." |
                            df_longish$HabitatDefinition == "none" |
                            df_longish$HabitatDefinition == "None" |
                            df_longish$HabitatDefinition == "non provided" |
                            df_longish$HabitatDefinition == "none provided" |
                            df_longish$HabitatDefinition == "None provided" |
                            df_longish$HabitatDefinition == "None specifically given" |
                            df_longish$HabitatDefinition == "not defined" |
                            df_longish$HabitatDefinition == "not directly provided" |
                            df_longish$HabitatDefinition == "not provided" |
                            df_longish$HabitatDefinition == "Not provided" |
                            df_longish$HabitatDefinition == "Not Provided" |
                            df_longish$HabitatDefinition == "none " |
                            df_longish$HabitatDefinition == "N/a" |
                            df_longish$HabitatDefinition == "none provided " |
                            df_longish$HabitatDefinition == "not provided " |
                            df_longish$HabitatDefinition == "non provided " |
                            df_longish$HabitatDefinition == "not defined " |
                            df_longish$HabitatDefinition == "not defined " ] <- 0

hist(as.numeric(df_longish$score), col = "skyblue3", 
     breaks = c(0,1,2,3,4),
     labels = c(1,2,3,4), include.lowest = F,
     xlab = "score", main = "Distribution of Habitat Scores")

h <- hist(as.numeric(df_longish$score), breaks = c(0,1,2,3,4), plot = F)
plot(h, xaxt = "n", xlab = "Habitat score", ylab = "Counts",
     main = "", col = "pink")
axis(1, h$mids, labels = 1:4, tick = F, padj = -1.5)

pie(c(sum(df_longish$HabitatDefined == 1), sum(df_longish$HabitatDefined == 0)),
    labels = c(paste("Habitat Defined \n", sum(df_longish$HabitatDefined == 1), "articles"), 
               paste("Not Defined \n", sum(df_longish$HabitatDefined == 0), "articles")),
    col = c("pink", "white"))

pie(c(length(unique(df_longish$ID)), 81 - length(unique(df_longish$ID))),
    labels = c(paste("Completed survey \n n =", length(unique(df_longish$ID))),
               paste("Didn't complete survey \n n =", 81 - length(unique(df_longish$ID)), ":(")),
    col = c("skyblue", "white"),
    main = "Survey completion")


defs <- c(df_longish$HabitatDefinition[df_longish$HabitatDefined == 1])

defs <- gsub('habitat','',defs)
defs <- gsub('Habitat','',defs)
defs <- gsub('definition','',defs)
defs <- gsub('Defined','',defs)
defs <- gsub('Species','',defs)
defs <- gsub('species','',defs)
defs <- gsub('Article','',defs)
defs <- gsub('score','',defs)
defs <- gsub('article','',defs)

clipr::write_clip(defs)
