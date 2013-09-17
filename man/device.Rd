\name{device}
\title{Cropping and Font Embedding PDF Device}
\alias{dev.off.pdf}
\description{
  \code{dev.off.pdf()} is meant for closing a pdf device. It also performs
  cropping and font embedding if chosen.
}
\usage{
dev.off.pdf(file="Rplots.pdf", crop=NULL, embedFonts="", ...)
}
\arguments{
  \item{file}{output file name including extension .pdf.}
  \item{crop}{crop command, can be one of:
    \describe{
      \item{\code{\link{NULL}}}{crop with a default command (suitable
	for Unix; for non-Unix, no cropping is done in this case).}
      \item{\code{\link{character}}}{a string containing a crop
	command.}
      \item{""}{do not crop.}
  }}
  \item{embedFonts}{font embedding command, can be one of:
    \describe{
      \item{\code{\link{NULL}}}{font embedding with a default command (suitable
	for Unix; for non-Unix, no font embedding is done in this case).}
      \item{\code{\link{character}}}{a string containing a font embedding
	command.}
      \item{""}{do not embed fonts.}
  }}
  \item{\dots}{additional arguments passed to \code{\link{dev.off}()}.}
}
\value{
  \code{\link{invisible}()}.
}
\author{Marius Hofert.}
\seealso{
  \pkg{sfsmisc}'s \code{\link{pdf.end}()}.
}
\examples{
## definition
dev.off.pdf
}
\keyword{utilities}
