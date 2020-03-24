##set working directory
##yours will be different
setwd("~/OneDrive - Missouri State University/TEACHING/745 Grad Statistics/notes/fall 16")

longdata = c9_invisible_long

##calculate means and sds for descriptive statistics
##to use tapply you would need data in long format (either between or within)
##use melt to convert to long format if you need to
M = tapply(longdata$Mischief, longdata$Cloak, mean)
stdev = tapply(longdata$Mischief, longdata$Cloak, sd)
N = tapply(longdata$Mischief, longdata$Cloak, length)

M;stdev;N
#Cloak No Cloak 
#5.00     3.75 
#Cloak No Cloak 
#1.651446 1.912875 
#Cloak No Cloak 
#12       12

####independent t ####
#Y ~ X or DV ~ IV group
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, 
       paired = FALSE)
#Two Sample t-test

#data:  Mischief by Cloak
#t = 1.7135, df = 22, p-value = 0.1007
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.2629284  2.7629284
#sample estimates:
  #mean in group Cloak mean in group No Cloak 
#5.00                   3.75 

##effect size
library(MOTE)
effect = d.ind.t(m1 = M[1], m2 = M[2],
        sd1 = stdev[1], sd2 = stdev[2],
        n1 = N[1], n2 = N[2], a = .05)

effect$d

##power
library(pwr)
pwr.t.test(n = NULL, d = effect$d, sig.level = .05,
           power = .80, type = "two.sample", alternative = "two.sided")

####dependent t####
##note note note this code requires the participants be in order 
##note note note if you melt data that will be true as long as you don't eliminate NAs
##therefore, datascreen in wide format, melt at the point you are doing the analysis
##there are lots of reasons here, do not melt then datascreen, that violates independence
t.test(Mischief ~ Cloak, 
       data = longdata, 
       var.equal = TRUE, 
       paired = TRUE)

#Paired t-test

#3data:  Mischief by Cloak
#t = 3.8044, df = 11, p-value = 0.002921
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
 # 0.5268347 1.9731653
#sample estimates:
 # mean of the differences 
#1.25 

##dept averages
##you need M, sd of each level, remember N is the same people
M
stdev
N

effect2 = d.dep.t.avg(m1 = M[1], m2 = M[2],
                 sd1 = stdev[1], sd2 = stdev[2],
                 n = N[1], a = .05)

effect2$d
#N
#Cloak No Cloak 
#12       12 
#effect2 = d.dep.t.avg(m1 = M[1], m2 = M[2],
                    #    +                  sd1 = stdev[1], sd2 = stdev[2],
                     #   +                  n = N[1], a = .05)
# effect2$d
#Cloak 
#0.7013959 

##dept t differences
##create the mdiff and sddiff scores (melted data is a bit trickier)
table(longdata$Cloak)
cloak = subset(longdata, Cloak == "Cloak")
nocloak = subset(longdata, Cloak == "No Cloak")
diff = cloak$Mischief - nocloak$Mischief
mdiff = mean(diff, na.rm = T)
sddiff = sd(diff, na.rm = T)
N2 = length(diff)

mdiff; sddiff; N2
#[1] 1.25
#[1] 1.13818
#[1] 12
effect2.1 = d.dep.t.diff(mdiff = mdiff, sddiff = sddiff,
                         n = N2, a = .05)

effect2.1$d
#[1] 1.098244

library(pwr)
pwr.t.test(n = NULL, d = effect2$d, sig.level = .05,
           power = .80, type = "paired", alternative = "two.sided")
#Paired t test power calculation 

#n = 17.96948
#d = 0.7013959
#sig.level = 0.05
#power = 0.8
#alternative = two.sided

#NOTE: n is number of *pairs*

####chart####
library(ggplot2)

cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

bargraph = ggplot(longdata, aes(Cloak, Mischief))
bargraph +
  cleanup +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = .2, 
               position = "dodge") +
  xlab("Invisible Cloak Group") +
  ylab("Average Mischief Acts")
