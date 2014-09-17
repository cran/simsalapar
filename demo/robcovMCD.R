require("simsalapar")

if(!require("robustbase"))
    stop("Needs the robustbase package; please install it, typically via\n",
	 "   install.packages(\"robustbase\")\n")
if(package_version(packageDescription("robustbase")$Version) < "0.9.8"
   || !any("raw.only" == names(formals(covMcd))))
    stop("Need a version of 'robustbase' where covMcd(*,raw.only=TRUE) works")

## Here, we will redo part of the simulations done (and reported on) in
##
## Pison, G., Van Aelst, S., and Willems, G. (2002),
## Small Sample Corrections for LTS and MCD,
## _Metrika_ *55*, 111--123.
##
## ~/save/papers/robust-diverse/Pison_VanAelst_Willems.pdf

## build grid (for the *real* simulation
varList <- varlist(n.sim = list(type="N", expr = quote(N[sim]), value = 1000),
		   n     = list(type="grid", expr = quote(n),
				value = c(10, 15, 20, 30, 35, 40, 60, 100, 200)),
		   ## Note:  p < n is really "practically" required ...
		   p     = list(type="grid", expr = quote(p),
				value = c(1:5, 7, 10, 15, 25)),
		   alpha = list(type="grid", expr = quote(alpha),
				value = 1/2 + (0:8)/16)
		   )

if(interactive() || exists(".checking")) { ## i.e., when run as a demo, need much smaller sizes
    message("Redimensioning the variable list in order to finish  ``in time'' ..")
    varList.full <- varList
    varList <- set.n.sim(varList, 512)
    varList$ n    $value <- local({n <- varList$n$value; n[n <= 60]})
    varList$ p    $value <- c(1:4, 7)
    varList$ alpha$value <- c(0.5, 0.75)
}

(pGrid <- mkGrid(varList))
ng <- get.nonGrids(varList)

## do1mcd() : to be applied to one line of the grid
do1mcd <- function(n, p, alpha)
{
    stopifnot(length(alpha) == 1, 0.5 <= alpha, alpha <= 1,
	      length(n) == 1, n == round(n), n > 0,
	      length(p) == 1, p == round(p), n > 0)
    X <- matrix(rnorm(n*p), n,p)
    ## Compute  Det(\hat\Sigma) ^ {1/p} :
    mcd <- covMcd(X, alpha=alpha, use.correction=FALSE, raw.only=TRUE)
    lD <- determinant(mcd$raw.cov)
    exp(as.vector(lD$modulus) / p) ## == Det(.)^{1/p}
}

##---- apply  do1mcd()  {*not* in parallel, for now}: ---------------------------

do1mcd(n=10, p=1, alpha=0.5)
do1mcd(n=30, p=2, alpha=0.5)

## a dummy small list, for testing -- this one with some p >= n:
vl.sm0 <- varlist(n.sim = list(expr = quote(N[sim]), value = 8),
		  n     = list(type="grid", expr= quote(n), value= 5*(2:4)),
		  ## NB: p >= n gives errors {but we can deal w/ them, after all!}
		  p     = list(type="grid", expr= quote(p), value = c(1:2,10,15)),
		  alpha = list(type="grid", expr= quote(alpha),value = c(.5, .75))
		  )
mkGrid(vl.sm0)# (as basic check) -> 24 rows

options(error = recover)
system.time(
r.sm0 <- doLapply(vl.sm0, seed = "seq", subjob.=subjob,
		  doOne=do1mcd, timer=mkTimer(gcFirst=FALSE))
)#
## currently *with* errors :
str(e <- getArray(r.sm0,"error"))
print.table(e2 <- apply(e, 1:2, sum), zero=".")# error exactly  iff  p >= n
stopifnot(identical(which(e2 > 0), c(7L, 10:11)))

## a too small list; for testing (type = "N" is automatic for 'n.sim'):
vl.sml <- varlist(n.sim = list(expr = quote(N[sim]), value = 64),
		  n     = list(type="grid", expr = quote(n),
		  value = c(10, 15, 20:24, 30)),
		  p     = list(type="grid", expr = quote(p), value = c(1:3, 5)),
		  alpha = list(type="grid", expr = quote(alpha),
			       value = 1/2 + (0:2)/4)
		  )

system.time(
r.sml <- doLapply(vl.sml, seed = "seq",
	       subjob. = subjob, doOne=do1mcd, timer=mkTimer(gcFirst=FALSE))
)# ~ 23 sec (MH: 68s :-( )

save(r.sml, vl.sml, file = "robcovMCD-sml-sim.rda")

(v.sml <- getArray(r.sml))
mayplot(v.sml, vl.sml, row.vars = NULL, col.vars = "alpha", xvar = "n")
mayplot(v.sml, vl.sml, row.vars = "p", col.vars = "alpha", xvar = "n")


## now the "big" simulation:
system.time({
res <- doForeach(varList, block.size = 64,
                 seed = "seq", #-> no 'seed' needed
                 subjob. = subjob, doOne=do1mcd, timer=mkTimer(FALSE))
})

## now save "all", including some platform info:
sess.I <- sessionInfo()
node <- Sys.info()[c("nodename","release")]
node.I <- c(node, cores = parallel::detectCores())
save(r.sml, vl.sml, res, varList, do1mcd, sess.I, node.I,
     file = sprintf("robcovMCD-sim_%s_%s.rda",
     node[1], format(Sys.Date())))

## convert array of nice simulated values
str(val <- getArray(res))
apply(val, 1:2, mean)## -- see boxplots below
apply(val, 1:2,   sd)## comparable

mayplot(val, varList, pcol="tomato",
	row.vars = NULL, col.vars = "meth", xvar = "n")

str(times <- getArray(res, "time"))
table(round(times)) # most are '0'; rest very platform dependent..

## check
stopifnot(val["100", "robust", 1:3] == res["100", "robust", 1:3]$value)

## Errors (trivial here, due to getArray()'s default FUN for comp="error"):
errs <- getArray(res, "error")
summary(errs)
n.err <- apply(errs, 1:2, sum) ## number of errors :
n.err                          ## ==> none at all for this example

## compute percentages of errors:
n.sim <- ng$n.sim
ftable(n.err/n.sim * 100)
