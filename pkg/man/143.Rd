\name{getTimeRange_method__DecompositionOperator}
\alias{getTimeRange_method__DecompositionOperator}
\title{ask for the boundaries of the underlying time interval}
\description{The method returns the time range of the given object 
It is ( probably mostly ) used internally to make sure that 
time dependent functions retrieved from data are not
used outside the interval where they are valid. }
\arguments{
  \item{object}{
}
}

\value{a vector of length two \code{ c(t_min,t_max) }
containing start and end time of the time interval 
for which the object has been defined.}

\author{Carlos A. Sierra, Markus Mueller}





