\name{stableMarriage-package}
\alias{stableMarriage-package}
\docType{package}
\title{
  Implementation of the Stable Matching algorithm
}
\description{
  Implementation of the Stable Matching algorithm from "Stable Marriage and Its Relation to Other Combinatorial Problems" (Knuth. D and GoldStein. M, 1996), using C++ for the main function
}
\details{
  "Preference Tables" as described in the text are here implemented as named lists of vectors of strings. An example preference table is given by 
  \dontrun{
    pref1 = list(
      A = c(a,b,c,d,e),
      B = c(b,c,d,e,a),
      C = c(c,d,e,a,b),
      D = c(d,e,a,b,c),
      E = c(e,a,b,c,d)
      )
  }
  where the A,...,E and a,...,e variables are unique strings
}
\author{
Luke Fairley.

Maintainer: Luke Fairley <l.fairley@lancaster.ac.uk>
}
\references{
  "Stable Marriage and Its Relation to Other Combinatorial Problems" (Knuth. D and GoldStein. M, 1996)
}
\keyword{ package }
\seealso{
  Optional links to other man pages
}
\examples{
  \dontrun{
     a = "a"
b = "b"
c = "c"
d = "d"
e = "e"
A = "A"
B = "B"
C = "C"
D = "D"
E = "E"

pref1 = list(
  A = c(a,b,c,d,e),
  B = c(b,c,d,e,a),
  C = c(c,d,e,a,b),
  D = c(d,e,a,b,c),
  E = c(e,a,b,c,d)
)

pref2 = list(
  a = c(B,C,D,E,A),
  b = c(C,D,E,A,B),
  c = c(D,E,A,B,C),
  d = c(E,A,B,C,D),
  e = c(A,B,C,D,E)
)

findStableMatching(pref1, pref2)

##Output:
[[1]]
[1] "E" "C" "B" "D" "A"

[[2]]
[1] "e" "c" "b" "d" "a"
  }
}
