# Create a function to print squares of numbers in sequence.
simulate <- function(N) {
    # set.seed(1)
    lambda <- rgamma(N, shape = 3, rate = 4)
    y <- rpois(N, lambda=lambda)
    lambdas_two <- c()
    count <- 0; j <- 1
    for (i in 1:N){
        if (y[i] == 2){
            count <- count + 1 
            lambdas_two[j] <- lambda[i]
            j <- j+1 
        }
    }

    # simulated expected proportion
    print(count/N)
    
    pois <- function(x, lambda) (exp(-lambda)* lambda ^(x))/factorial(x)
    # theoretically expected proportion
    print(pois(2, 3/4))

    plot(hist(lambdas_two))

}


simulate_c <- function(N) {
    lambda <- rgamma(N, shape = 3, rate = 4)
    lambdas_sample <- c()
    j <- 0
    for (i in 1:N){
        # generate 3 values for each lambda
        y1 <- rpois(1, lambda=lambda[i])
        y2 <- rpois(1, lambda=lambda[i])
        y3 <- rpois(1, lambda=lambda[i])
        if ((y1 == 2) & (y2 == 0) & (y3 == 1)){
            # keep lambda in a new sample 
            lambdas_sample[j] <- lambda[i]
            j <- j + 1
        }
    }
    mean_lambda <- mean(lambdas_sample)
    cat( "mean lambda value: ",mean_lambda)
    return(mean_lambda)
}

numerical_integration <- function(ynew){
    f1 <- function(lambda) {dpois(ynew, lambda) * dpois(3, lambda) * dgamma(lambda, shape = 3, scale = 4)}
    f2 <- function(lambda) {dpois(3, lambda) * dgamma(lambda, shape = 3, scale = 4)}

    return(integrate(f1, 0, 1)$value/integrate(f2, 0, 1)$value)
}



mean_lambda <- simulate_c(1000000)
cat("\napproximative answer using samples\n")
print(dpois(0, mean_lambda))
print(dpois(1, mean_lambda))
print(dpois(2, mean_lambda))
print(dpois(3, mean_lambda))
print(ppois(3, mean_lambda, lower=FALSE))

print("----------------------------------------")
print("answer using numerical integration")
y_new0 = numerical_integration(0)
y_new1 = numerical_integration(1)
y_new2 = numerical_integration(2)
y_new3 = numerical_integration(3)
y_new4 = 1 - (y_new0 + y_new1 + y_new2 + y_new3)

print(y_new0)
print(y_new1)
print(y_new2)
print(y_new3)
print(y_new4)