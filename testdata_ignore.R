power = 0.8           # power
d = 1               # MRES
sig.level = 0.05 # significance level
alternative = "two.sided" # type of H1
test.type = "t.test"      # type of test
se.max = 1           # desired (max) standard error
dif = "simple"             # difference ("simple" "perc")
method = "param" # method ("param" "boot")
nstart = 20      # initial number of samples
nmax   = 1000    # maximum allowed sample size
seed   = 1234    # seed for PRNG
boot.R = 499     # number of bootstrap resamples
force.balanced = FALSE # force balanced sampling
ncpus  = 1             # number of cores to use
save.partial.results = FALSE # save tmp files?
folder = "./nreps_files"

algorithms <- mapply(FUN = function(i, m, s){
                          list(FUN   = "dummyalgo",
                               alias = paste0("algo", i),
                               distribution.fun  = "rnorm",
                               distribution.pars = list(mean = m, sd = s))},
                     i = c(alg1 = 1, alg2 = 2, alg3 = 3, alg4 = 4),
                     m = c(15, 10, 30, 15),
                     s = c(2, 4, 6, 8),
                     SIMPLIFY = FALSE)
#'
# Just generate the same instance, 100 times
instances <- lapply(1:100,
             function(i) {list(FUN   = "dummyinstance",
                               alias = paste0("Inst. ", i))})
