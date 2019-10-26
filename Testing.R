library(dslabs)
data("movielens")
str(movielens)
nrow(movielens)
nlevels(movielens$genres)
index <- order(murders$total)
murders$state[index]
print(max(murders$total))
print(murders$state[which.max(murders$total)])
print(rank(murders$total))
murder_rate = (murders$total/murders$population)*100000
print(murders$state[order(murder_rate,decreasing = TRUE)])
safe <- murder_rate <= 1
region <- murders$region == "West"
index <- safe & region
print(murders$state[index])
