\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\name{doApply}
\title{Functions for Iterating Over All Subjobs}
\alias{doLapply}
\alias{doForeach}
\alias{doRmpi}
\alias{doMclapply}
\alias{doClusterApply}
\alias{doRes.equal}
\description{
  \code{doLapply()} iterates over all subjobs (using the non-parallel
  \code{\link{lapply}()}). Similarly, but in parallel, for
  \code{doForeach} (based on CRAN package \CRANpkg{foreach}'s \code{\link[foreach]{foreach}()}),
  \code{doRmpi} (based on \CRANpkg{Rmpi}'s \code{\link[Rmpi]{mpi.apply}()}),
  \code{doMclapply} (based on \pkg{parallel}'s \code{\link{mclapply}()}), and
  \code{doClusterApply} (based on \pkg{parallel}'s \code{\link{clusterApply}()}).

  \code{doRes.equal()} is simple convenience wrapper for
  \code{\link{all.equal}()}, for comparing two results (from the same
  \code{\link{varlist}} and \code{doOne} arguments) of the \code{do*}
  lapply-like functions above.
  }
\usage{
doLapply(vList, seed="seq", repFirst=TRUE,
       sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
       doOne, ...)
doForeach(vList, doCluster = !(missing(spec) && missing(type)),
       spec=detectCores(), type="MPI",
       block.size = 1, seed="seq", repFirst=TRUE, sfile=NULL,
       check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
       doOne, extraPkgs=character(), exports=character(), ...)
doRmpi(vList,
       nslaves = if((sz <- mpi.universe.size()) <= 1) detectCores() else sz,
       load.balancing=TRUE, block.size = 1, seed="seq", repFirst=TRUE,
       sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
       doOne, exports=character(), ...)
doMclapply(vList, cores = if(.Platform$OS.type == "windows") 1 else detectCores(),
       load.balancing=TRUE, block.size = 1, seed="seq", repFirst=TRUE,
       sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
       doOne, ...)
doClusterApply(vList, spec=detectCores(), type="MPI",
       load.balancing=TRUE, block.size = 1, seed="seq", repFirst=TRUE,
       sfile=NULL, check=TRUE, doAL=TRUE, subjob.=subjob, monitor=FALSE,
       doOne, initExpr, exports=character(), ...)

doRes.equal(x, y, tol = 1e-15, ...)
}
\arguments{
  \item{vList}{a \code{\link{list}} of variable specifications. Each
    variable spec is itself a named list which must contain a
    \code{"value"} component.}
  \item{doCluster}{logical indicating if \code{\link{makeCluster}(spec,
      type)} should be called and registered.  If false as by default,
    \code{spec} is used to specify the number of cores to be used.}
  \item{spec}{a specification appropriate to the type of cluster (see
    \code{type}); if \code{doCluster} is false, and for the default
    \code{type = "MPI"}, the number of nodes.}
  \item{type}{the cluster type, see \code{\link{makeCluster}()} from
    package \pkg{parallel}.  For reasons why we choose \code{"MPI"}
    as default, see the discussion starting at
    \url{https://stat.ethz.ch/pipermail/r-sig-hpc/2013-April/001647.html}}
  \item{cores}{the number of cores.}
  \item{nslaves}{the number of \dQuote{slaves} for \code{doRmpi}, passed
    to package \CRANpkg{Rmpi}'s \code{\link[Rmpi]{mpi.spawn.Rslaves}} when no
    running slaves are found.}
  \item{load.balancing}{\code{\link{logical}} indicating whether
    load balancing is used:
    \describe{
      \item{doRmpi()}{\code{\link[Rmpi]{mpi.applyLB}()} is used instead of
	\code{\link[Rmpi]{mpi.apply}()}.}
      \item{doMclapply()}{here, \code{mc.preschedule=!load.balancing}
	determines load balancing.}
      \item{doClusterApply()}{\code{\link{clusterApplyLB}()} instead of
	\code{\link{clusterApply}()}.}
  }}
  \item{block.size}{size of blocks of rows in the virtual grid which are
    computed simultaneously (load-balancing).}
  \item{seed, repFirst}{see \code{\link{subjob}()}.}
  \item{sfile, check, doAL}{see \code{\link{saveSim}()}.}
  \item{subjob.}{a \code{\link{function}} for computing a subjob (one
    row of the virtual grid). Typically \code{\link{subjob}()}.}
  \item{doOne}{a user-supplied \code{\link{function}} for computing one
    row of the (physical) grid.}
  \item{monitor}{a logical or a \code{\link{function}} for producing
    \dQuote{monitoring} output; the function argument list must contain
    the one of \code{\link{printInfo}[["default"]]}.}
  \item{extraPkgs}{\code{\link{character}} vector of packages to be made
    available on the nodes.}
  \item{exports}{\code{\link{character}} vector of functions (for
    \code{\link{doForeach}()} and \code{\link{doClusterApply}()}) or
    objects (for \code{\link{doRmpi}()}) to export.}
  \item{initExpr}{expression initially evaluated on the cluster
    (can be missing).}
  \item{\dots}{additional arguments passed to \code{subjob()} (typically
    further passed on to \code{doOne()}), or, for \code{doRes.equal()}, to
    \code{\link{all.equal}(*)}.}

%% For doRes.equal():
  \item{x,y}{each a result of, say \code{doLapply()} which should be
    compared where sensible, i.e., the first three components
    \code{"value"},\code{"error"},\code{"warning"}, using
    \code{\link{all.equal}}.}
  \item{tol}{passed to \code{\link{all.equal}(*)}.}
}
\value{
  The result of applying \code{subjob()} to all subjobs, converted with
  \code{\link{saveSim}()}.
}
\details{
  See the vignette or references in \code{\link{simsalapar-package}} for
  how to use these functions.
}
\author{Marius Hofert and Martin Maechler.}
\seealso{
  \code{\link{subjob}()} for computing a subjob.  \code{\link{doCallWE}()}
  for the return value of \code{doOne()}.  \code{\link{.Random.seed}} for
  information about random number generators and seeds.
}
\examples{
 if(simsalapar:::doExtras()) { ## needs some CPU
   demo(robust.mean) # 512 simulations, differing block sizes, ...
 }
}
\keyword{utilities}
