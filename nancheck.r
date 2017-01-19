
nancheck <- function(x, algo, iter)
  .Internal(nancheck(x, algo, iter))

B <- rep(runif(1000))
for(a in 1:7)
    # vector of 1K elements
    # 10G elements checked
    print(system.time( nancheck(B, a, 10000000) ))


A <- rep(runif(1000), 100000)
for(a in 1:7)
    # vector of 100M elements
    # 10G elements checked
    print(system.time( nancheck(A, a, 100) ))
    
    
