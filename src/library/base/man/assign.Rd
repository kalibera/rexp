% File src/library/base/man/assign.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{assign}
\alias{assign}
\alias{unassign}
\title{Assign a Value to a Name}
\description{
  Assign a value to a name in an environment.
}
\usage{
assign(x, value, pos = -1, envir = as.environment(pos),
       inherits = FALSE, immediate = TRUE)
unassign(x, pos = -1, envir = as.environment(pos),
       inherits = FALSE, warn.on.copy = FALSE)
}
\arguments{
  \item{x}{a variable name, given as a character string.  No coercion is
    done, and the first element of a character vector of length greater
    than one will be used, with a warning.}
  \item{value}{a value to be assigned to \code{x}.}
  \item{pos}{where to do the assignment.  By default, assigns into the
    current environment.  See \sQuote{Details} for other possibilities.}
  \item{envir}{the \code{\link{environment}} to use.  See \sQuote{Details}.}
  \item{inherits}{should the enclosing frames of the environment be
    inspected?}
  \item{immediate}{an ignored compatibility feature.}
  \item{warn.on.copy}{should \code{unassign} print a warning when returning
    a freshly created copy of the value?}
}
\details{
  There are no restrictions on the name given as \code{x}: it can be a
  non-syntactic name (see \code{\link{make.names}}).

  The \code{pos} argument can specify the  environment in which to assign or
  unassign the object in any of several ways: as \code{-1} (the default), as
  a positive integer (the position in the \code{\link{search}} list); as the
  character string name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}} to
  access the currently active function calls).  The \code{envir} argument is
  an alternative way to specify an environment, but is primarily for back
  compatibility.

  \code{assign} and \code{unassign} do not dispatch assignment methods, so
  they cannot be used to set elements of vectors, names, attributes, etc.

  Note that assignment to an attached list or data frame changes the
  attached copy and not the original object: see \code{\link{attach}}
  and \code{\link{with}}.

  \code{unassign} first looks up the variable \code{x} in the environment
  specified, following the same algorithm as \code{\link{get}}.  It also
  returns the value of the variable, but sets the variable to \code{NULL}.
  The returned value is guaranteed to not be bound to any variable; if
  \code{unassign} cannot rule out the existence of another binding, it
  returns a fresh (unbound) copy of the value.  \code{unassign} is useful
  for avoiding unnecessary object copying (see example below).  It allows
  in-place modification of R objects by native code without violating the
  pass-by-value semantics of R.  The function should not be used when the
  memory/performance savings are unimportant, as it hinders code
  readability.
}

\value{
  This function is invoked for its side effect, which is assigning
  \code{value} to the variable \code{x}.  If no \code{envir} is
  specified, then the assignment takes place in the currently active
  environment.

  If \code{inherits} is \code{TRUE}, enclosing environments of the supplied
  environment are searched until the variable \code{x} is encountered.
  The value is then assigned in the environment in which the variable is
  encountered (provided that the binding is not locked: see
  \code{\link{lockBinding}}: if it is, an error is signaled).  If the
  symbol is not encountered then assignment takes place in the user's
  workspace (the global environment).

  If \code{inherits} is \code{FALSE}, assignment takes place in the
  initial frame of \code{envir}, unless an existing binding is locked or
  there is no existing binding and the environment is locked (when an
  error is signaled).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}

\seealso{
  \code{\link{<-}},
  \code{\link{get}},
  \code{\link{exists}},
  \code{\link{environment}}.
}
\examples{
for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
    nam <- paste("r", i, sep = ".")
    assign(nam, 1:i)
}
ls(pattern = "^r..$")

##-- Global assignment within a function:
myf <- function(x) {
    innerf <- function(x) assign("Global.res", x^2, envir = .GlobalEnv)
    innerf(x+1)
}
myf(3)
Global.res # 16

a <- 1:4
assign("a[1]", 2)
a[1] == 2          # FALSE
get("a[1]") == 2   # TRUE

x <- c(-1, 0, 1)
x <- abs(x)             # the object allocated by "c()" is copied by abs
                        # in current version of R

x <- c(-1, 0, 1)
x <- abs(unassign("x")) # the object allocated by "c()" is re-used by abs

x <- c(-1, 0, 1)
z <- x                  # z is bound to the object allocated by "c()"
x <- abs(unassign("x")) # the object allocated by "c()" has to be copied by abs

x <- 1 + 1
unassign("x")           # 2
x                       # NULL
}
\keyword{data}
