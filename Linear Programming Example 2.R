#Load packages
if(!require('lpSolveAPI')){install.packages('lpSolveAPI')}
if(!require('dplyr')){install.packages('dplyr')}

#Set up the LPP
#Create LP object. Always 0 as first argument and number of decision variables as second.
lp = make.lp(0,3)
lp
#set to maximize (min is default)
lp.control(lp, sense="max", all.int=TRUE,  int.vec= TRUE, int.count=TRUE)
#Objective function coefficients
set.objfn(lp,c(45,400,20))


#Constraints
add.constraint(lp, c(3, 25,1), "<=", 45)

set.bounds(lp, upper = 7, columns = 3)
set.type(lp, 1:3, type = c("integer"))
#Check LPP setup
lp

#Solve the LPP
solve(lp)
#Optimized value of objective function
get.objective(lp)
#Optimized values of decision variables
get.variables(lp)
#Values of each constraint line at optimum
get.constraints(lp)

