## Copyright (C) 2012-13 Marius Hofert and Martin Maechler
##
## This program is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License along with
## this program; if not, see <http://www.gnu.org/licenses/>.

##' @title Function for Iterating Over All Subjobs (Non-Parallel)
##' @param vList list of variable specifications
##' @param seed repFirst: see subjob()
##' @param repFirst see subjob()
##' @param sfile see saveSim()
##' @param check see saveSim()
##' @param doAL see saveSim()
##' @param subjob. function for computing a subjob (one row of the virtual grid);
##'        typically subjob()
##' @param doOne user-supplied function for computing one row of the (physical)
##'        grid
##' @param ... additional arguments passed to subjob() (typically further
##'        passed on to doOne())
##' @return the result of applying subjob() to all subjobs, converted with saveSim()
##' @author Marius Hofert and Martin Maechler
##' @note Works *sequentially*
##' { doLapply }
doLapply <- function(vList, seed="seq", repFirst=TRUE, sfile=NULL,
                     check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
                     doOne, ...)
{
    if(!is.null(r <- maybeRead(sfile))) return(r)
    stopifnot(is.function(subjob.), is.function(doOne))
    if(!(is.null(seed) || is.na(seed) || is.numeric(seed) ||
         (is.list(seed) && all(vapply(seed, is.numeric, NA))) ||
         is.character(seed) ))
        stop(.invalid.seed.msg)
    if(check) doCheck(doOne, vList, nChks=1, verbose=FALSE)

    ## monitor checks {here, not in subjob()!}
    if(!(is.logical(monitor) || is.function(monitor)))
        stop(gettextf("'monitor' must be logical or a function like %s",
                      'printInfo[["default"]]'))

    ## variables
    pGrid <- mkGrid(vList)
    ngr <- nrow(pGrid)
    ng <- get.nonGrids(vList) # => n.sim >= 1
    n.sim <- ng$n.sim # get n.sim

    ## actual work
    res <- lapply(seq_len(ngr * n.sim), subjob.,
                  pGrid=pGrid, nonGrids = ng$nonGrids, repFirst=repFirst,
                  n.sim=n.sim, seed=seed, doOne=doOne, monitor=monitor, ...)

    ## convert result and save
    saveSim(res, vList=vList, repFirst=repFirst,sfile=sfile,check=check,doAL=doAL)
}
##' { end } doLapply

##' @title Function for Iterating Over All Subjobs in Parallel Using Foreach
##' @param vList list of variable specifications
##' @param doCluster logical indicating whether the sub jobs are run on a cluster
##'        or rather several cores
##' @param spec if doCluster=TRUE : number of nodes; passed to parallel's
##'                                 makeCluster()
##'             if doCluster=FALSE: number of cores
##' @param type cluster type, see parallel's ?makeCluster
##' @param block.size size of blocks of rows in the virtual grid which are computed
##'        simultaneously
##' @param seed see subjob()
##' @param repFirst see subjob()
##' @param sfile see saveSim()
##' @param check see saveSim()
##' @param doAL see saveSim()
##' @param subjob. function for computing a subjob (one row of the virtual grid);
##'        typically subjob()
##' @param doOne user-supplied function for computing one row of the (physical)
##'        grid
##' @param extraPkgs character vector of packages to be made available on the nodes
##' @param exports character vector of functions to export
##' @param ... additional arguments passed to subjob() (typically further
##'        passed on to doOne())
##' @return result of applying subjob() to all subjobs, converted with saveSim()
##' @author Marius Hofert and Martin Maechler
##' @note Works on multiple nodes or cores
##' { doForeach }
doForeach <-
    function(vList, doCluster = !(missing(spec) && missing(type)),
             spec = detectCores(),
             type = if(.Platform$OS.type == "windows") "PSOCK" else "MPI",
             block.size=1,
             seed="seq", repFirst=TRUE,
             sfile=NULL, check=TRUE, doAL=TRUE,
             subjob.=subjob, monitor=FALSE, doOne,
             extraPkgs=character(), exports=character(), ...)
{
    ## Unfortunately, imports() ends not finding 'iter' from pkg "iterators":
    ## --> rather strictly require things here:
    stopifnot(require("foreach"), require("doParallel"))
    if(!is.null(r <- maybeRead(sfile))) return(r)
    stopifnot(is.function(subjob.), is.function(doOne))
    if(!(is.null(seed) || is.na(seed) || is.numeric(seed) ||
         (is.list(seed) && all(vapply(seed, is.numeric, NA))) ||
         is.character(seed) ))
        stop(.invalid.seed.msg)
    if(check) doCheck(doOne, vList, nChks=1, verbose=FALSE)

    ## monitor checks {here, not in subjob()!}
    if(!(is.logical(monitor) || is.function(monitor)))
        stop(gettextf("'monitor' must be logical or a function like %s",
                      'printInfo[["default"]]'))

    ## variables
    pGrid <- mkGrid(vList)
    ngr <- nrow(pGrid)
    ng <- get.nonGrids(vList) # => n.sim >= 1
    n.sim <- ng$n.sim
    stopifnot(1 <= block.size, block.size <= n.sim, n.sim %% block.size == 0)

    ## Two main cases for parallel computing
    if(!doCluster) { # multiple cores
        ## ?registerDoParallel -> Details -> Unix + multiple cores => 'fork' is used
        stopifnot(is.numeric(spec), length(spec) == 1)
        registerDoParallel(cores=spec) # register doParallel to be used with foreach
    }
    else { # multiple nodes
        ## One actually only needs makeCluster() when setting up a *cluster*
        ## for working on different nodes. In this case, the 'spec' argument
        ## specifies the number of nodes.
        ## The docu about registerDoParallel() might be slightly misleading...
        cl <- makeCluster(spec, type=type) # create cluster
        on.exit(stopCluster(cl)) # shut down cluster and execution environment
        registerDoParallel(cl)  # register doParallel to be used with foreach
    }
    if(check) cat(sprintf("getDoParWorkers(): %d\n", getDoParWorkers()))

    ## actual work
    n.block <- n.sim %/% block.size
    i <- NULL ## <- dirty but required for R CMD check ...
    res <- ul(foreach(i=seq_len(ngr * n.block),
                      .packages=c("simsalapar", extraPkgs),
                      .export=c(".Random.seed", "iter", "mkTimer", exports)) %dopar%
          {
              lapply(seq_len(block.size), function(k)
                     subjob.((i-1)*block.size+k, pGrid=pGrid,
                             nonGrids=ng$nonGrids, repFirst=repFirst,
                             n.sim=n.sim, seed=seed, doOne=doOne,
                             monitor=monitor, ...))})
    ## convert result and save
    saveSim(res, vList, repFirst=repFirst, sfile=sfile, check=check, doAL=doAL)
}
##' { end } doForeach

##' @title Function for Iterating Over All Subjobs in Parallel Using Rmpi
##' @param vList list of variable specifications
##' @param spec cluster specification (number of workers)
##' @param load.balancing logical indicating whether to use mpi.applyLB() instead of mpi.apply()
##' @param block.size size of blocks of rows in the virtual grid which are computed
##'        simultaneously
##' @param seed see subjob()
##' @param repFirst see subjob()
##' @param sfile see saveSim()
##' @param check see saveSim()
##' @param doAL see saveSim()
##' @param subjob. function for computing a subjob (one row of the virtual grid);
##'        typically subjob()
##' @param doOne user-supplied function for computing one row of the (physical)
##'        grid
##' @param exports vector of objects to export
##' @param ... additional arguments passed to subjob() (typically further
##'        passed on to doOne())
##' @return the result of applying subjob() to all subjobs, converted with saveSim()
##' @author Marius Hofert and Martin Maechler
##' @note Works on multiple nodes or cores
##' Email from Rmpi maintainer (hyu@stats.uwo.ca) on 2013-06-10:
##' If you are using OpenMPI, then mpi.universe.size() will always return 1
##' unless R is launched through mpirun.
##' Yes. You can use the option nslaves to launch slaves as many as you want.
##' How those slave processes assigned to nodes/cores are  controlled by
##' OpenMPI (different MPIs have different ways of assigning slave processes
##' but most recycle available notes/cores). In your cases, you probably
##' choose nslaves=4 so that all cores are running in parallel. However,
##' setting nslaves to be higher than the available notes/codes achieves some
##' kind loading balancing. For example, nslaves = 8 essentially spreads an
##' entire job into 8 small ones instead of 4 small ones. This gives some
##' advantages if one of the original 4 small jobs runs much longer than
##' others.
##' mpi.universe.size() # => 1; the total number of CPUs available in a cluster
##' mpi.spawn.Rslaves() # spawn as many slaves as the MPI environment knows (=> 1 master, 1 slave)
##' mpi.close.Rslaves()
##' mpi.spawn.Rslaves(nslaves=17) # spawn more slaves than possible (?) (=> 1
##'                                 master, 17 slaves) => calculations are still
##'                                 only done on the max. available cores; see
##'                                 test script http://collaborate.bu.edu/linga/ParallelMCMC
##' mpi.close.Rslaves()
##' Note: spawning more slaves than available may lead to errors (MH)
##' { doRmpi }
doRmpi <- function(vList,
                   nslaves = if((sz <- mpi.universe.size()) <= 1) detectCores()
                             else sz,
                   load.balancing=TRUE, block.size=1, seed="seq", repFirst=TRUE,
                   sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
                   doOne, exports=character(), ...)
{
    if(!require("Rmpi"))
        stop("You must install the CRAN package 'Rmpi' before you can use doRmpi()")

    if(!is.null(r <- maybeRead(sfile))) return(r)
    stopifnot(is.function(subjob.), is.function(doOne))
    if(!(is.null(seed) || is.na(seed) || is.numeric(seed) ||
         (is.list(seed) && all(vapply(seed, is.numeric, NA))) ||
         is.character(seed) ))
        stop(.invalid.seed.msg)
    if(check) doCheck(doOne, vList, nChks=1, verbose=FALSE)

    ## monitor checks {here, not in subjob()!}
    if(!(is.logical(monitor) || is.function(monitor)))
        stop(gettextf("'monitor' must be logical or a function like %s",
                      'printInfo[["default"]]'))

    ## variables
    pGrid <- mkGrid(vList)
    ngr <- nrow(pGrid)
    ng <- get.nonGrids(vList) # => n.sim >= 1
    n.sim <- ng$n.sim
    stopifnot(1 <= block.size, block.size <= n.sim, n.sim %% block.size == 0)

    ## use as many workers as available
    ## Note: mpi.comm.size(comm) returns the total number of members in a comm
    comm <- 1  ## communicator number
    if (!mpi.comm.size(comm)) ## <==> no slaves are running
        mpi.spawn.Rslaves(nslaves=nslaves)
    ## quiet = TRUE would omit successfully spawned slaves
    on.exit(mpi.close.Rslaves()) # close slaves spawned by mpi.spawn.Rslaves()
    ## pass global required objects to cluster (required by mpi.apply())
    mpi.bcast.Robj2slave(.Random.seed)
    mpi.bcast.Robj2slave(mkTimer)
    for(e in exports) {
        ee <- substitute(mpi.bcast.Robj2slave(EXP), list(EXP = as.symbol(e)))
        eval(ee)
    }

    ## instead of initExpr, this needs a 'initFunction' + 'initArgs'
    ## if(!missing(initExpr)) do.call(mpi.bcast.cmd, c(list(initFunction), ...))

    ## actual work
    n.block <- n.sim %/% block.size
    res <- ul((if(load.balancing) mpi.applyLB else mpi.apply)(
        seq_len(ngr * n.block), function(i)
        lapply(seq_len(block.size), function(k)
            subjob.((i-1)*block.size+k, pGrid=pGrid,
                    nonGrids=ng$nonGrids, repFirst=repFirst,
                    n.sim=n.sim, seed=seed, doOne=doOne, monitor=monitor, ...))))

    ## convert result and save
    saveSim(res, vList, repFirst=repFirst, sfile=sfile, check=check, doAL=doAL)
}
##' { end } doRmpi

##' @title Function for Iterating Over All Subjobs in Parallel Using mclapply()
##' @param vList list of variable specifications
##' @param cores number of cores
##' @param load.balancing logical indicating whether to use mpi.applyLB() instead of mpi.apply()
##' @param block.size size of blocks of rows in the virtual grid which are computed
##'        simultaneously
##' @param seed see subjob()
##' @param repFirst see subjob()
##' @param sfile see saveSim()
##' @param check see saveSim()
##' @param doAL see saveSim()
##' @param subjob. function for computing a subjob (one row of the virtual grid);
##'        typically subjob()
##' @param doOne user-supplied function for computing one row of the (physical)
##'        grid
##' @param ... additional arguments passed to subjob() (typically further
##'        passed on to doOne())
##' @return the result of applying subjob() to all subjobs, converted with saveSim()
##' @author Marius Hofert and Martin Maechler
##' @note Works on multiple cores (but runs *sequentially* on Windows)
##' { doMclapply }
doMclapply <-
    function(vList,
             cores = if(.Platform$OS.type == "windows") 1 else detectCores(),
             load.balancing=TRUE, block.size=1, seed="seq", repFirst=TRUE,
             sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob,
             monitor=FALSE, doOne, ...)
{
    if(!is.null(r <- maybeRead(sfile))) return(r)
    stopifnot(is.function(subjob.), is.function(doOne))
    if(!(is.null(seed) || is.na(seed) || is.numeric(seed) ||
         (is.list(seed) && all(vapply(seed, is.numeric, NA))) ||
         is.character(seed) ))
        stop(.invalid.seed.msg)
    if(check) doCheck(doOne, vList, nChks=1, verbose=FALSE)

    ## variables
    pGrid <- mkGrid(vList)
    ngr <- nrow(pGrid)
    ng <- get.nonGrids(vList) # => n.sim >= 1
    n.sim <- ng$n.sim
    stopifnot(1 <= block.size, block.size <= n.sim, n.sim %% block.size == 0)

    ## monitor checks
    if(!(is.logical(monitor) || is.function(monitor)))
        stop(gettextf("'monitor' must be logical or a function like %s",
                      'printInfo[["default"]]'))

    ## actual work
    n.block <- n.sim %/% block.size
    res <- ul(mclapply(seq_len(ngr * n.block), function(i)
                       lapply(seq_len(block.size), function(k)
                              subjob.((i-1)*block.size+k, pGrid=pGrid,
                                      nonGrids=ng$nonGrids, repFirst=repFirst,
                                      n.sim=n.sim, seed=seed, doOne=doOne,
                                      monitor=monitor, ...)),
                       mc.cores = cores,
                       mc.preschedule = !load.balancing, mc.set.seed=FALSE))

    ## convert result and save
    saveSim(res, vList, repFirst=repFirst, sfile=sfile, check=check, doAL=doAL)
}
##' { end } doMclapply

##' @title Function for Iterating Over All Subjobs in Parallel Using clusterApply()
##' @param vList list of variable specifications
##' @param spec cluster specification (number of workers)
##' @param type cluster type, see parallel's ?makeCluster (basically snow's makeCluster)
##' @param load.balancing logical indicating whether to use clusterApplyLB()
##'     instead of clusterApply()
##' @param block.size size of blocks of rows in the virtual grid which are computed
##'        simultaneously
##' @param seed see subjob()
##' @param repFirst see subjob()
##' @param sfile see saveSim()
##' @param check see saveSim()
##' @param doAL see saveSim()
##' @param subjob. function for computing a subjob (one row of the virtual grid);
##'        typically subjob()
##' @param doOne user-supplied function for computing one row of the (physical)
##'        grid
##' @param initExpr expression initially evaluated on the cluster (can be missing)
##' @param ... additional arguments passed to subjob() (typically further
##'        passed on to doOne())
##' @return the result of applying subjob() to all subjobs, converted with saveSim()
##' @author Marius Hofert and Martin Maechler
##' @note Works on multiple nodes or cores
##' { doClusterApply }
doClusterApply <-
    function(vList, spec=detectCores(),
             type = if(.Platform$OS.type == "windows") "PSOCK" else "MPI",
             load.balancing=TRUE, block.size=1, seed="seq", repFirst=TRUE,
             sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
             doOne, initExpr, exports=character(), ...)
{
    if(!is.null(r <- maybeRead(sfile))) return(r)
    stopifnot(is.function(subjob.), is.function(doOne))
    if(!(is.null(seed) || is.na(seed) || is.numeric(seed) ||
         (is.list(seed) && all(vapply(seed, is.numeric, NA))) ||
         is.character(seed) ))
        stop(.invalid.seed.msg)
    if(check) doCheck(doOne, vList, nChks=1, verbose=FALSE)

    ## variables
    pGrid <- mkGrid(vList)
    ngr <- nrow(pGrid)
    ng <- get.nonGrids(vList) # => n.sim >= 1
    n.sim <- ng$n.sim
    stopifnot(1 <= block.size, block.size <= n.sim, n.sim %% block.size == 0)

    ## create cluster object
    cl <- makeCluster(spec, type=type)
    on.exit(stopCluster(cl)) ## shut down cluster and execution environment

    ## monitor checks
    if(!(is.logical(monitor) || is.function(monitor)))
        stop(gettextf("'monitor' must be logical or a function like %s",
                      'printInfo[["default"]]'))

    clusterExport(cl, varlist=c(".Random.seed", "mkTimer", exports))
    if(!missing(initExpr)) clusterCall(cl, eval, substitute(initExpr))

    ## actual work
    n.block <- n.sim %/% block.size
    res <- ul((if(load.balancing) clusterApplyLB else clusterApply)(
        cl, seq_len(ngr * n.block), function(i)
        lapply(seq_len(block.size), function(k)
               subjob.((i-1)*block.size+k, pGrid=pGrid,
                       nonGrids=ng$nonGrids, repFirst=repFirst,
                       n.sim=n.sim, seed=seed, doOne=doOne, monitor=monitor, ...))))

    ## convert result and save
    saveSim(res, vList, repFirst=repFirst, sfile=sfile, check=check, doAL=doAL)
}
##' { end } doClusterApply

##' Function for comparing do*Apply() results:
##' { doRes.equal }
doRes.equal <- function(x,y, tol=1e-15, ...)
    all.equal(lapply(x, `[`, 1:3),
              lapply(y, `[`, 1:3), tol=tol, ...)
##' { end }

