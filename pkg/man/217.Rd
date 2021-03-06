\name{TwopMMmodel}
\alias{TwopMMmodel}
\title{Implementation of a two-pool Michaelis-Menten model}
\description{This function implements a two-pool Michaelis-Meneten model with a microbial biomass and a substrate pool.}
\usage{TwopMMmodel(t, ks = 1.8e-05, kb = 0.007, Km = 900, r = 0.6, Af = 1, 
    ADD = 3.2, ival)}\arguments{
  \item{t}{vector of times (in days) to calculate a solution.}
  \item{ks}{a scalar representing SOM decomposition rate (m3 d-1 (gCB)-1)}
  \item{kb}{a scalar representing microbial decay rate (d-1)}
  \item{Km}{a scalar representing the Michaelis constant (g m-3)}
  \item{r}{a scalar representing the respired carbon fraction (unitless)}
  \item{Af}{a scalar representing the Activity factor; i.e. a temperature and moisture modifier (unitless)}
  \item{ADD}{a scalar representing the annual C input to the soil (g m-3 d-1)}
  \item{ival}{a vector of length 2 with the initial values of the SOM pool and the microbial biomass pool (g m-3)}
}
\details{This implementation is similar to the model described in Manzoni and Porporato (2007). }
\value{An object of class NlModel that can be further queried.}
\references{Manzoni, S, A. Porporato (2007). A theoretical analysis of nonlinearities and feedbacks in soil carbon and nitrogen cycles.
Soil Biology and Biochemistry 39: 1542-1556.}
\author{Carlos A. Sierra, Markus Mueller}




\examples{
  
  days=seq(0,1000,0.5)
  
  #Run the model with default parameter values
  MMmodel=TwopMMmodel(t=days,ival=c(100,10))
# fixme mm:
# the next line causes trouble on Rforge Windows patched build

#  Cpools=getC(MMmodel)
#  
#  #Time solution
#  matplot(days,Cpools,type="l",ylab="Concentrations",xlab="Days",lty=1,ylim=c(0,max(Cpools)*1.2))
#  legend("topleft",c("SOM-C", "Microbial biomass"),lty=1,col=c(1,2),bty="n")
#  
#  ks=0.000018
#  kb=0.007
#  r=0.6
#  ADD=3.2
#  
#  #Analytical solution of fixed points
#  #Cs_=kb/((1-r)*ks) wrong look at the sympy test print twopMModel.pdf
#  Km=900
#  Af=1
#  Cs=kb*Km/(Af*ks*(1-r)-kb)
#  abline(h=Cs,lty=2)
#  
#  Cb=(ADD*(1-r))/(r*kb)
#  abline(h=Cb,lty=2,col=2)
#  
#  #State-space diagram
#  plot(Cpools[,2],Cpools[,1],type="l",ylab="SOM-C",xlab="Microbial biomass")
#  
#  #Microbial biomass over time
#  plot(days,Cpools[,2],type="l",col=2,xlab="Days",ylab="Microbial biomass")
#  
#  #The default parameterization exhaust the microbial biomass.
#  #A different behavior is obtained by increasing ks and decreasing kb
#  MMmodel=TwopMMmodel(t=days,ival=c(972,304) ,Af=3,kb=0.0000001)
#  Cpools=getC(MMmodel)
#  
#  matplot(days,Cpools,type="l",ylab="Concentrations",xlab="Days",lty=1,ylim=c(0,max(Cpools)*1.2))
#  legend("topleft",c("SOM-C", "Microbial biomass"),lty=1,col=c(1,2),bty="n")
#  
#  plot(Cpools[,2],Cpools[,1],type="l",ylab="SOM-C",xlab="Microbial biomass")
#  
#  plot(days,Cpools[,2],type="l",col=2,xlab="Days",ylab="Microbial biomass")
  
}
