algorithms <- mapply(FUN = function(i, m, s){
                          list(FUN   = "dummyalgo",
                               alias = paste0("algo", i),
                               distribution.fun  = "rnorm",
                               distribution.pars = list(mean = m, sd = s))},
                     i = c(alg1 = 1, alg2 = 2, alg3 = 3, alg4 = 4),
                     m = c(15, 10, 30, 15),
                     s = c(2, 4, 6, 8),
                     SIMPLIFY = FALSE)

instances <- lapply(1:100,
                    function(i) {rate <- runif(1, 1, 10)
                    list(FUN   = "dummyinstance",
                         alias = paste0("Inst.", i),
                         distr = "rexp", rate = rate,
                         bias  = -1 / rate)})


d = .5
se.max = .5
power = 0.8
sig.level = 0.05
power.target = "mean"
dif = "simple"
comparisons = "all.vs.all"
alternative = "two.sided"
test = "t.test"
method = "param"
nstart = 20
nmax = 600
force.balanced = FALSE
ncpus = 1#parallel::detectCores() - 1
boot.R = 499
seed = NULL
save.partial.results = FALSE
folder = "./nreps_files"


my.results <- run_experiment(instances, algorithms, d, se.max,
                             power, sig.level, power.target, dif, comparisons,
                             alternative, test, method, nstart, nmax,
                             force.balanced, ncpus, boot.R, seed,
                             save.partial.results, folder)
