df <- read.csv("EFB390_AssignmentForm1_20230905.csv")

df <- df[df$Name..first.and.last. != "",]

df$ID <- as.factor(df$Name..first.and.last.)

df <- df[,colSums(is.na(df))<nrow(df)] %>%
    select(-c("Name", "Name..first.and.last.", "Start.time", "Completion.time", "Email"))

require(reshape2)
require(stringr)
require(TuktuTools)

# Specify id.vars: the variables to keep but not split apart on
df_long <-  melt(df, id.vars="ID") %>% mutate(variable = as.character(variable))

df_long$Article <- substr(df_long$variable, 9, 9)
df_long$param <- substr(df_long$variable, 11, nchar(df_long$variable))

df_long$param <- gsub('2','',df_long$param)
df_long$param <- gsub('3','',df_long$param)
df_long$param <- gsub('4','',df_long$param)

unique(df_long$param)



#pie(table(df_longish$Interaction2), label = lbls)

int <- df_long %>% subset(param == "Interaction.type.") %>%
    group_by(value) %>%
    summarize(count = n()) %>%
    mutate(value = ifelse(count <=2, "other", value)) %>%
    group_by(value) %>%
    summarise(count = sum(count))

lbls <- paste0(int$value, "\n", int$count)

ggplot(int, aes(x="", y=count, fill=value)) +
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

write.csv(df_long, "Assgn1SurveyResponses_20230905.csv")

unique(df_long$param)

involved <- df_long %>% subset(param == "Check.all.who.were.involved.") %>%
    select(value)

involved <- paste0(involved$value, collapse = "")
involved <- data.frame(value = strsplit(involved, ";"))
names(involved)[1] <- "value"

involved.df <- involved %>% group_by(value) %>%
    summarize(count = n()) %>%
    mutate(value = ifelse(count <=2, "other", value)) %>%
    group_by(value) %>%
    summarise(count = sum(count))
    
lbls <- paste0(involved.df$value, "\n", involved.df$count)

library(ggplot2)

ggplot(data=involved.df, aes(x=value, y=count, fill = value)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_brewer(palette="Dark2") + 
  theme_bw() +
  theme(legend.position="none",
        text = element_text(size=30)) +
  labs(title = "Who was involved?", x = "", y = "")

# word clouds #https://www.wordclouds.com/
unique(df_long$param)
writeClipboard(df_long$value[df_long$param == "title."])

writeClipboard(df_long$value[df_long$param == "Geographical.location."])
writeClipboard(df_long$value[df_long$param == "Species.involved."])
writeClipboard(df_long$value[df_long$param == "news.source."])
