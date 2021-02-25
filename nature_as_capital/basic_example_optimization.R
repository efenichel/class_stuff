# basic Conrad Bioecon model example
#documentation for opt function is at https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html 
rm(list = ls())  #clears memory
if (!require("pacman")) install.packages("pacman") #pacman allows you to use the p_load function
#the p_load function checks is a library is installed, if not it installs it, then it attaches the 
#called library
p_load(ggplot2, plyr)

# ggplot2 https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf 
#############################################################
#this section is editable

#time periods
capT <- 10

#parameters
s.parms <- data.frame(
  price = 10, 
  cost = 1.0,
  growth = 0.5,
  carrying.capacity = 1.0,
  discount.rate = 0.1,
  stock0 = 0.2,
  capT <- capT
  )

#############################################################3
#discount factor
df <- 1/(1+s.parms$discount.rate)

#preallocate harvests and stocks
harvest <-matrix(0,capT,1)
stock <- matrix(0,capT+1,1)
stock[1]<-s.parms$stock0

####################################################
# Functions

#stock dynamics, note that this is actually a difference equation
xtp1 <- function(xt, harv, param){
  with(as.data.frame(c(param)),{ 
    return(xt + growth*xt*(1-xt/carrying.capacity)-harv)
  })
}

#test function with growth from stock "stock0" and no harvest
xtp1(s.parms$stock0,0,s.parms)

#present value of net benefits (pvnbt) function
pvnbt<- function(harvest, time, param){
  with(as.data.frame(c(param)),{ 
    return(df^time*(price*harvest-cost/2*harvest^2))
  }) 
}


#simulator returns stock, harvest, and pvnbt
my.sim <- function(stock, harvest, param){
  with(as.data.frame(c(param)),{ 
    for(j in 1:length(harvest)){
      stock[j+1]<-xtp1(stock[j],harvest[j],param)
    }
    h.out <- c(harvest, growth*stock[capT+1]*(1-stock[capT+1]/carrying.capacity))
    timep <- c(0,c(1:capT))
    pvn.out <- pvnbt(h.out,timep,param)
    pvn.out[length(pvn.out)] <-  pvn.out[length(pvn.out)]/discount.rate
    sim.out<-matrix(c(stock,h.out,pvn.out), nrow=length(stock))
    return(list(sum(sim.out[,3]),sim.out))
  }) 
}

#uses the simulation function to create a function that can be minimized
#R only can minimize
max.me <- function(harvest,param){
  with(as.data.frame(c(param)),{ 
    stock <- c(stock0,matrix(0,capT,1)) #provides a preallocated vector for stock
    temp <- my.sim(stock,harvest,param)
    return(-temp[[1]]) #negative sign is because we want to maximize, but the R
    #solver only minimizes
  })
}


######################################################################################
# test, optimize, and graph

#test simulation
sim.res <- my.sim(stock,harvest,s.parms)
sim.res[[2]][capT,1]
sim.res[[1]]

#this calls the "solver"
opt.out <- optim(par = harvest, fn = max.me, param = s.parms, method = 'L-BFGS-B', lower = 0)
cat("maximized value is", -opt.out$value)
cat("optimal harvest program is", opt.out$par)

#simulate the optimal path and save as plot sim, then rename the variable names
optsim <- my.sim(stock,opt.out$par,s.parms)
plot.sim <- as.data.frame(optsim[[2]][,1:2])
plot.sim <- rename(plot.sim, c("V1" = "stock", "V2" = "harvest"))

#create data for the nullcline or x\dot = 0 isocline
nullx <- c(0:100)/100
dxdt <- xtp1(nullx,0,s.parms) - nullx
nullcline <- data.frame(nullx, dxdt)

#make a phase plane using ggplot2
ggplot() + 
  geom_line(data = plot.sim, aes(x = stock, y = harvest), color = 'blue') + 
  geom_line(data = nullcline, aes (x = nullx, y = dxdt)) +
  theme(  #http://ggplot2.tidyverse.org/reference/theme.html
    axis.line = element_line(color = "black"), 
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )
  
