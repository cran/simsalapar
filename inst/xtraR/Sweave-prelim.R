## Copyright (C) 2012 Marius Hofert and Martin Maechler
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


## Custom graphics device (for cropping .pdf):
pdfCrop <- function(name, width, height, ...) {
    f <- paste0(name, ".pdf")
    grDevices::pdf(f, width=width, height=height, onefile=FALSE)
    assign(".pdfCrop.name", f, envir=globalenv())
}
pdfCrop.off <- function() { # used automagically
    grDevices::dev.off() # closing the pdf device
    f <- get(".pdfCrop.name", envir=globalenv())
    system(paste("pdfcrop --pdftexcmd pdftex", f, f, "1>/dev/null 2>&1"),
           intern=FALSE) # crop the file (relies on PATH)
}

# options(width=70, useFancyQuotes=FALSE, prompt="R> ", continue="+  ") # JSS style, but ugly
options(width=70, useFancyQuotes=FALSE, prompt="> ", continue="  ")
## as we use pdfCrop(): unneeded:
## options(SweaveHooks=list(fig=function() par(mar=c(4, 4, 0.4, 0.7))))

## patchDVI:::patchSynctex setup
.TexRoot <- "parallel.tex"

## "Global"  system.time() saving etc:
.dir.exists <- function(x)
  is.character(x) && file.exists(x) && file.info(path.expand(x))$isdir

##' Create directory if needed
mkDir <- function(name) if(!.dir.exists(name)) dir.create(name, recursive=TRUE)

##' Create "unique" file name
mkFname <- function(stem, ext, sep="_") {
    stopifnot(is.character(stem))
    fn <- paste(stem, format(as.Date(Sys.time())), sep=sep)
    if(nzchar(ext)) paste(fn, ext, sep=".") else fn
}

##' Create "unique" file name inside note-specific directory
mkD.Fname <- function(dir, stem, ext="", first = Sys.info()[["nodename"]]) {
    stopifnot(is.character(dir), is.character(stem), is.character(first))
    mkDir(dir <- file.path(dir, first))
    file.path(dir, mkFname(stem, ext=ext))
}
