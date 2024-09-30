exam <- read.csv("ExamOneGrades.csv")

hist(exam$Total)
mean(exam$Total)
median(exam$Total)

require(tidyr)
long <- exam %>% pivot_longer(
  cols = c("Part.I", 
           "Part.II", 
           "EC1", "Part.III",
           "EC2", "Part.IV", "Total"),
  names_to = "question",
  values_to = "score"
)

require(ggplot2)

ggplot(long %>%
         subset(question != "EC1" &
                  question != "EC2"), 
       aes(x = score)) +
  geom_histogram() +
  facet_wrap(~question, 
             ncol = 1,
             scales = "free_x") +
  theme_bw()
