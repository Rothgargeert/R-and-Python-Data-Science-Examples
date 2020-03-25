
#Set up the LPP
if(!require('lpSolveAPI')){install.packages('lpSolveAPI')}
if(!require('dplyr')){install.packages('dplyr')}


#Create LP object. Always 0 as first argument and number of decision variables as second.
lp = make.lp(0,8)
lp
#(min is default)
lp.control(lp)
#Objective function coefficients
set.objfn(lp,c(200,250,225,190,215,245,235,220))


#Constraints
add.constraint(lp, Crews$A, ">=", 1)
add.constraint(lp, Crews$B, ">=", 1)
add.constraint(lp, Crews$C, ">=", 1)
add.constraint(lp, Crews$D, ">=", 1)
add.constraint(lp, Crews$E, ">=", 1)
add.constraint(lp, Crews$F, ">=", 1)
add.constraint(lp, Crews$G, ">=", 1)
add.constraint(lp, Crews$H, ">=", 1)
add.constraint(lp, Crews$I, ">=", 1)
add.constraint(lp, Crews$J, ">=", 1)
#Check LPP setup
lp

#Solve the LPP
solve(lp)
#Optimized value of objective function
get.objective(lp)
#Optimized values of decision variables
get.variables(lp)


