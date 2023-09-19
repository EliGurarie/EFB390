a <- read.csv("clipboard")
lastnames <- a[,1]


tolower(lastnames)

counts <- lapply(letters, grepl, tolower(lastnames))
names(counts) <- letters
sapply(counts, sum) %>% sort
