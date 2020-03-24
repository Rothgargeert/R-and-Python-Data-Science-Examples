View(OCD)
library(MASS)
#Discriminant ANalysis of OCD data;
#newModel <- lda(Group ~ Predictor(s), data=dataFrame, prior=prior
#probabilities, na.action="na.omit")
OCDDFA <- lda(Group ~ Actions + Thoughts, data=OCD)
OCDDFA
#lda(Group ~ Actions + Thoughts, data = OCD)

#Prior probabilities of groups:
#  BT       CBT        NT 
#0.3333333 0.3333333 0.3333333 

#Group means:
#  Actions Thoughts
#BT      3.7     15.2
#CBT     4.9     13.4
#NT      5.0     15.0

#Coefficients of linear discriminants:
#               LD1        LD2
#Actions    0.6030047   0.4249451
#Thoughts   -0.3352478  0.3392631

#Proportion of trace:
#  LD1    LD2 
#0.8219 0.1781 

#obtaining the individual patient scores
#predict(OCDDFA)
#plotting the individual patient scores
plot(OCDDFA)
