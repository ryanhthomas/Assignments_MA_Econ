library(AER)
library(stargazer)
library(car)
library(sandwich)
data(HMDA)

#Model 1#
HMDA$deny <- ifelse(HMDA$deny=="yes",1,0)
HMDA$afam <- ifelse(HMDA$afam=="yes",1,0)
HMDA$mlvrat <- ifelse(HMDA$lvrat>=.8&HMDA$lvrat<=.95,1,0)
HMDA$hlvrat <- ifelse(HMDA$lvrat > .95,1,0)
HMDA$chist <- as.numeric(levels(HMDA$chist))[HMDA$chist]
HMDA$mhist <- as.numeric(levels(HMDA$mhist))[HMDA$mhist]
HMDA$phist <- ifelse(HMDA$phist=="yes",1,0)
HMDA$insurance <- ifelse(HMDA$insurance=="yes",1,0)
HMDA$selfemp <- ifelse(HMDA$selfemp=="yes",1,0)
m1 <- lm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp,data=HMDA)

#Model 2#
m2 <- glm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp,family=binomial(link=logit),data=HMDA)

#Model 3#
m3 <- glm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp,family=binomial(link=probit),data=HMDA)

#Model 4#
m4 <- glm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp+single+hschool+unemp,family=binomial(link=probit),data=HMDA)
#Exclusion test: applicant single; high school diploma; industry unemployment rate#
xm4F <- round(na.trim(linearHypothesis(m4,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m4,type="HC1"))$F),digits=2)
xm4p <- round(na.trim(linearHypothesis(m4,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m4,type="HC1"))$"Pr(>F)"),digits=2)

#Model 5#
m5 <- glm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp+single+hschool+unemp+condomin,family=binomial(link=probit),data=HMDA)
#Exclusion test: applicant single; high school diploma; industry unemployment rate#
xm5F <- round(na.trim(linearHypothesis(m5,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m5,type="HC1"))$F),digits=2)
xm5p <- round(na.trim(linearHypothesis(m5,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m5,type="HC1"))$"Pr(>F)"),digits=2)
xcm5F <- round(na.trim(linearHypothesis(m5,c("condomin=0"),test="F",vcov=vcovHC(m5,type="HC1"))$F),digits=2)
xcm5p <- round(na.trim(linearHypothesis(m5,c("condomin=0"),test="F",vcov=vcovHC(m5,type="HC1"))$"Pr(>F)"),digits=2)

#Model 6#
HMDA$afamnum <- ifelse(HMDA$afam=="yes",1,0)
HMDA$afampirat <- HMDA$afamnum * HMDA$pirat
HMDA$afamhirat <- HMDA$afamnum * HMDA$hirat
m6 <- glm(deny~afam+pirat+hirat+mlvrat+hlvrat+chist+mhist+phist+insurance+selfemp+single+hschool+unemp+afampirat+afamhirat,family=binomial(link=probit),data=HMDA)
#Exclusion test: applicant single; high school diploma; industry unemployment rate#
xm6F <- round(na.trim(linearHypothesis(m6,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m6,type="HC1"))$F),digits=2)
xm6p <- round(na.trim(linearHypothesis(m6,c("single=0","hschool=0","unemp=0"),test="F",vcov=vcovHC(m6,type="HC1"))$"Pr(>F)"),digits=2)
xrbm6F <- round(na.trim(linearHypothesis(m6,c("afampirat=0","afamhirat=0","afam=0"),test="F",vcov=vcovHC(m6,type="HC1"))$F),digits=2)
xrbm6p <- round(na.trim(linearHypothesis(m6,c("afampirat=0","afamhirat=0","afam=0"),test="F",vcov=vcovHC(m6,type="HC1"))$"Pr(>F)"),digits=2)
xrm6F <- round(na.trim(linearHypothesis(m6,c("afampirat=0","afamhirat=0"),test="F",vcov=vcovHC(m6,type="HC1"))$F),digits=2)
xrm6p <- round(na.trim(linearHypothesis(m6,c("afampirat=0","afamhirat=0"),test="F",vcov=vcovHC(m6,type="HC1"))$"Pr(>F)"),digits=2)

#Robust & clustered standard errors#
cov1 <- vcovHC(m1,type = "HC1")
se1 <- sqrt(diag(cov1))
cov2 <- vcovHC(m2,type = "HC1")
se2 <- sqrt(diag(cov2))
cov3 <- vcovHC(m3,type = "HC1")
se3 <- sqrt(diag(cov3))
cov4 <- vcovHC(m4,type = "HC1")
se4 <- sqrt(diag(cov4))
cov5 <- vcovHC(m5,type = "HC1")
se5 <- sqrt(diag(cov5))
cov6 <- vcovHC(m6,type = "HC1")
se6 <- sqrt(diag(cov6))

lse <- list(se1,se2,se3,se4,se5,se6)

#Difference in predicted probability of denial, white vs. black (percentage points)#

d1 <- coef(summary(m1))[2,1]

#Stargazer table output#
stargazer(m1,m2,m3,m4,m5,m6,se=lse,digits=3,dep.var.caption=" ",
          omit.table.layout=c("d"),omit.stat=c("n","adj.rsq","f","ser","rsq"),
          covariate.labels=c("black","P/I ratio","housing expense-to-income ratio","medium loan-to-value ratio (0.80 <= loan-value ratio <= 0.95)",
                             "high loan-to-value ratio (loan-valueratio > 0.95)","consumer credit score",
                             "mortgage credit score","public bad credit record","denied mortgage insurance","self-employed","single","high school diploma",
                             "unemployment rate","condominium","black x P/I ratio","black x housing expense=to-income ratio"),
          add.lines=list(c("additional credit rating indicator variables","no","no","no","no","yes","no"),
                         c(" "),
                         c(" "),
                         c("F-Statistics and p-values testing exclusion of groups of variables"),
                         c(" "),
                         c("applicant single; high school diploma; industry unemployment rate"," "," "," ",xm4F,xm5F,xm6F),
                         c(" "," "," "," ",xm4p,xm5p,xm6p),
                         c("additional credit rating indicator variables"," "," "," "," ",xcm5F," "),
                         c(" "," "," "," "," ",xcm5p," "),
                         c("race interactions and black"," "," "," "," "," ",xrbm6F),
                         c(" "," "," "," "," "," ",xrbm6p),
                         c("race interactions only"," "," "," "," "," ",xrm6F),
                         c(" "," "," "," "," "," ",xrm6p),
                         c("difference in predicted probability of denial, white vs. black (percentage points)"," "," "," "," "," "," ")),
          align=TRUE,rownames=FALSE,type="text",out="ProbitLogitTable.txt")

          #c("difference in predicted probability of denial, white vs. black (percentage points)", , , , , , )),#