\name{findStableMatching}
\alias{findStableMatching}
\docType{package}
\title{
Finds a stable matching, formulated as a list of string vectors, given two preference tables
}
\description{
Does not check validity of preference tables by default, use argument checkTables = T to ensure that the given tables are compatible. Note that this impacts performance.
The raw underlying C++ function can be called as "stableMatching(pref1,pref2)"", but has no inbuilt capability for checking validity of the arguments, and cannot be interrupted striaght away.
}
\usage{
findStableMatching(pref1, pref2, checkTables = F)	
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
[1]]
[1] "E" "C" "B" "D" "A"

[[2]]
[1] "e" "c" "b" "d" "a"

}
}
