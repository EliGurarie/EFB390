df <- read.csv("EFB390Assignment6.csv", fileEncoding = "latin1")

df$ID <- as.factor(df$ID)

require(reshape2)
require(stringr)

# Specify id.vars: the variables to keep but not split apart on
df_long <-  melt(df[,-c(2:5)], id.vars=c("ID", "Name..first.and.last.."))
names(df_long)[names(df_long) == "Name..first.and.last.."] <- "Name"

df_long$variable <- gsub("Article.1.", "Article1_", df_long$variable)
df_long$variable <- gsub("Article.2.", "Article2_", df_long$variable)

df_long$variable <- gsub("DOI..Digital.object.identifier..can.be.found.in.Zotero.", "DOI", df_long$variable)
df_long$variable <- gsub("species.", "species", df_long$variable)


df_long$Article <- str_split_fixed(df_long$variable, "_", 2)[,1]
df_long$param <- str_split_fixed(df_long$variable, "_", 2)[,2]

df_longish <- dcast(df_long[,c("ID", "value", "Article", "param")], ID + Article ~ param, value.var = "value")

df_longish$Name <- df_long$Name[match(df_longish$ID, df_long$ID)]

# from asignment 5

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
