
# simulating N trees in area of (0, 1)x(0, 1) uniformly
N = (rgamma(1, shape = 36, rate = 1))
N = 36
x <- runif(N,0,1)
y <- runif(N,0,1)

plot(x,y, pch=2, ylim=c(0,1), xlim=c(0,1))


avg_distance <- function(x_values, y_values) {
    avg = 0
    sum = 0
    
    k = length(x_values)

    for(i in 1:k){
    # find minimum
        min = 10
        for(j in 1:k)
            if(i != j){
                new_min = sqrt((x_values[i] - x_values[j])**2 + (y_values[i] - y_values[j])**2)
                if(new_min < min)
                    min = new_min
            }       
        sum = min + sum
    }
    avg = sum/k
    return(avg)
}


# # simulate
simulate_e <- function(nb_simulation) {
    vals <- c()
    for(i in 1:nb_simulation){
        nb_tree = (rgamma(1, shape = 36, rate = 1))
        x <- runif(nb_tree,0,1)
        y <- runif(nb_tree,0,1)
        vals[i] <- avg_distance(x, y)
    }

    return(vals)
}

# vals = simulate_d(10000)
# # x=rgamma(200,4)

# # #sample from it
# sample_vals = sample(vals, 1000, replace = FALSE, prob = NULL)
# xhist=hist(sample_vals,freq=FALSE)


simulate_g <- function(nb_simulation) {

Y <- c()
sum <- 0
    for(i in 1:nb_simulation){
        nb_tree = (rgamma(1, shape = 36, rate = 1))
        x <- runif(nb_tree,0,1)
        y <- runif(nb_tree,0,1)
        Y[i] = sum(((x-0.5)**2 + (y-0.5)**2)<=0.1**2)
    }

    return(Y)
}

library(fitdistrplus)

y_vals <- simulate_g(10000)
# print(y_vals)
hist(y_vals)
print(fitdistr(y_vals, "exponential"))
