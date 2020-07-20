library(fuzzyjoin)
library(dplyr)

a <- data.frame(name = c('Ace Co', 'Bayes Inc.', 'asdf'),
                price = c(9, 99, 10))

b<- data.frame(name = c('Ace Co', 'Bayes', 'asd', 'Bcy', 'Baes', 'Bays'),
               price = c(10, 13, 2, 1, 15, 1))

final_result = stringdist_join(a, b, by = "name", mode = "full",
                               ignore_case = F, method = "jw", p=.15, max_dist=4,
                               distance_col = 'dist') %>% group_by(name.x) %>%top_n(1, -dist)

final_result$dist = 1 - final_result$dist
View(final_result)
