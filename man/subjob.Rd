\name{subjob}
\alias{printInfo}
\alias{subjob}
\title{Function for Computing one Row of the Virtual Grid (Subjob)}
\description{
  \code{subjob()} computes one row of the virtual grid in a simulation
  study, provides several seeding methods, and sub-job monitoring
  (information about the sub-job just finished).

  \code{printInfo} is a named list of functions optionally to be used as
  \code{monitor} in \code{subjob()} for printing information at the end of
  each sub-job.
}
\usage{
subjob(i, pGrid, nonGrids, n.sim, seed, keepSeed = FALSE,
       repFirst = TRUE, doOne,
       timer = mkTimer(gcFirst=FALSE), monitor = FALSE, ...)

printInfo # or
# printInfo[["default"]]
}
\arguments{
  %% \item{i.sim, j}{\code{\link{integer}} running in 1 to \code{n.sim} and
  %% 1 to number of rows in the physical grid \code{pGrid}, respectively,
  %% indicating the specific sub-job for which a monitor is to be
  %% computed (some information about the job).}
  \item{i}{row number of the virtual grid. \code{i.sim} and \code{j}
    together determine \code{i}.}
  \item{pGrid}{\dQuote{physical grid} of all combinations of variables
    of type \code{"grid"}, as returned by \code{\link{mkGrid}()}.}
  \item{nonGrids}{values of non-"grid"-variables (if provided, passed to
    \code{doOne()}), i.e., typically
    \code{\link{get.nonGrids}(*)[["nonGrids"]]}.}
  %% \item{res4}{4-\code{\link{list}} containing value, error, warning, and
  %%   run time.}
  \item{n.sim}{number of simulation replications.}
  \item{seed}{one of:
    \describe{
      \item{\code{\link{NULL}}}{\code{\link{.Random.seed}} remains
	untouched. If it does not exist, generate it by calling
	\code{runif(1)}. This case typically leads to non-reproducible
	results.}
      \item{\code{\link{numeric}(n.sim)}}{a \code{\link{numeric}} vector
	of length \code{n.sim} containing the seed for each simulation
	replications (same seed for each row
	in the (physical) grid; this ensures least variance across
	computations for the same replication).
	This case leads to reproducible results.}
      \item{\code{\link{vector}("list", n.sim)}}{a \code{\link{list}} of
	length \code{n.sim} containing seeds (tyically numeric vectors)
	for each of the \code{n.sim} simulation replications
	(same seed for each row in the (physical) grid). The seeds are
	assigned to \code{\link{.Random.seed}} in
	\code{\link{globalenv}()} and can thus be used for other random
	number generators such as \code{"L'Ecuyer-CMRG"}, see
	\code{\link{set.seed}()}. This case leads to reproducible results.}
      \item{\code{\link{NA}}}{\code{\link{.Random.seed}} remains
	untouched. If it does not exist, so be it. No fifth component is
	concatenated to the result of the \code{doOne()} call in this
	case (in all other case, it is appended if
	\code{keepSeed=TRUE}). This method typically leads to
	non-reproducible results.}
      \item{\code{\link{character}} string}{a character string
	specifying a seeding method. Currently only \code{"seq"} in
	which case the seeds 1 to \code{n.sim} for the \code{n.sim}
	simulation replications are used. This case leads to
	reproducible results.}
    }}
  \item{keepSeed}{\code{\link{logical}} indicating if \code{\link{.Random.seed}}
    should be appended to each return value of
    \code{\link{doCallWE}()}.}
  \item{repFirst}{\code{\link{logical}}; if \code{\link{TRUE}} (the
    default), all \code{n.sim} replications are computed for a
    row in the (physical) grid first, before the next row is considered;
    if \code{\link{FALSE}}, first all rows of the (physical) grid are
    computed for a fixed replicate until the next replicate is considered.}
  \item{doOne}{\code{\link{function}} for computing one row in the
    (physical) grid; must return a numeric vector, matrix, or array.}
  \item{timer}{a \code{\link{function}} similar to
    \code{\link{system.time}()}, passed to \code{\link{doCallWE}()}.}
  \item{monitor}{\code{\link{logical}} indicating whether a monitor
    is displayed.}
  \item{\dots}{additional arguments passed to \code{doOne()}.}
}
\value{
  \describe{
    \item{printInfo}{A named \code{\link{list}} of functions string containing
      information about the sub-job which \code{subjob()} has just
      finished.}
    \item{subjob()}{A vector of length five if \code{seed} is not \code{NA},
      otherwise of length four.  The first four components contain the
      return value of \code{\link{doCallWE}()}.  The fifth component contains
      \code{\link{.Random.seed}} before the call of \code{\link{doCallWE}()}
      (for reproducibility).}
  }
}
\details{
  See the vignette or references in \code{\link{simsalapar-package}} for
  how to use these functions.

  The case where \code{seed} is a \code{\link{numeric}} vector of length
  \code{n.sim} also leads to the same results no matter which variables
  are of type ``grid'' or ``inner''; see \code{demo(robust.mean)} where
  this is tested. This is important to guarantee since one might want to
  change certain ``inner'' variables to ``grid'' variables due to
  load-balancing while computing the desired statistics based on the
  same seed (or generated data from this seed).}
\author{Marius Hofert and Martin Maechler.}
\seealso{
  \code{\link{doCallWE}()}; \code{\link{.Random.seed}} for
  information about random number generators and seeds.

  For examples of \emph{implicit} use of \code{subjob}, see
  \code{\link{doLapply}}.
}
%% \examples{
%% }
\keyword{utilities}
