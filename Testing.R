library(dslabs)
data("movielens")
str(movielens)
nrow(movielens)
nlevels(movielens$genres)
index <- order(murders$total)
murders$state[index]
