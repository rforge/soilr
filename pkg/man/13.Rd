\name{AWBmodel}
\alias{AWBmodel}
\title{Implementation of the microbial model AWB (Allison, Wallenstein, Bradford, 2010)}
\description{This function implements the microbial model AWB (Allison, Wallenstein, Bradford, 2010), a four-pool model with a microbial biomass, enzyme, SOC and DOC pools. It is a special case of the general nonlinear model.}
\usage{AWBmodel(t, V_M = 1e+08, V_m = 1e+08, r_B = 2e-04, r_E = 5e-06, 
    r_L = 0.001, a_BS = 0.5, epsilon_0 = 0.63, epsilon_s = -0.016, 
    Km_0 = 500, Km_u0 = 0.1, Km_s = 0.5, Km_us = 0.1, Ea = 47, 
    R = 0.008314, Temp1 = 20, Temp2 = 20, ival = c(B = 2.19159, 
        E = 0.0109579, S = 111.876, D = 0.00144928), I_S = 0.005, 
    I_D = 0.005)}\arguments{
  \item{t}{vector of times (in hours) to calculate a solution.}
  \item{V_M}{a scalar representing the maximum rate of uptake (mg DOC cm-3 h-1). Equivalent to V_maxuptake0 in original paper.}
  \item{V_m}{a scalar representing the maximum rate of decomposition of SOM (mg SOM cm-3 h-1). Equivalent to V_max0 in original paper.}
  \item{r_B}{a scalar representing the rate constant of microbial death (h-1). Equivalent to r_death in original publication.}
  \item{r_E}{a scalar representing the rate constant of enzyme production (h-1). Equivalent to r_EnzProd in original publication.}
  \item{r_L}{a scalar representing the rate constant of enzyme loss (h-1). Equivalent to r_EnzLoss in original publication.}
  \item{a_BS}{a scalar representing the fraction of the dead microbial biomass incorporated to SOC. MICtoSOC in original publication.}
  \item{epsilon_0}{a scalar representing the intercept of the CUE function (mg mg-1). CUE_0 in original paper.}
  \item{epsilon_s}{a scalar representing the slope of the CUE function (degree-1). CUE_slope in original paper.}
  \item{Km_0}{a scalar representing the intercept of the half-saturation constant of SOC as a function of temperature (mg cm-3).}
  \item{Km_u0}{a scalar representing the intercept of the half saturation constant of uptake as a function of temperature (mg cm-3).}
  \item{Km_s}{a scalar representing the slope of the half saturation constant of SOC as a function of temperature (mg cm-3 degree-1).}
  \item{Km_us}{a scalar representing the slope of the half saturation constant of uptake as a function of temperature (mg cm-3 degree-1).}
  \item{Ea}{a scalar representing the activation energy (kJ mol-1).}
  \item{R}{a scalar representing the gas constant (kJ mol-1 degree-1).}
  \item{Temp1}{a scalar representing the temperature in the output vector.}
  \item{Temp2}{a scalar representing the temperature in the transfer matrix.}
  \item{ival}{a vector of length 4 with the initial values for the pools (mg cm-3).}
  \item{I_S}{a scalar with the inputs to the SOC pool (mg cm-3 h-1).}
  \item{I_D}{a scalar with the inputs to the DOC pool (mg cm-3 h-1).}
}
\details{This implementation containts default parameters presented in Allison et al. (2010). }
\value{An object of class NlModel that can be further queried.}
\references{Allison, S.D., M.D. Wallenstein, M.A. Bradford. 2010. Soil-carbon response to warming dependent on microbial physiology.
Nature Geoscience 3: 336-340.}
\author{Carlos A. Sierra, Markus Mueller}




\examples{

hours=seq(0,800,0.1)

#Run the model with default parameter values
bcmodel=AWBmodel(t=hours)

# fixme mm:
# the next line causes trouble on Rforge Windows patched build

#Cpools=getC(bcmodel)
#
##Time solution
#matplot(hours,Cpools,type="l",ylab="Concentrations",xlab="Hours",lty=1,ylim=c(0,max(Cpools)*1.2))
#legend("topleft",c("B", "E", "S", "D"),lty=1,col=c(1:4),bty="n")
#
##State-space diagram
#plot(as.data.frame(Cpools))

}
