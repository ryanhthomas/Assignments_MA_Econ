library(ISLR)
set.seed(2)
summary(USArrests)

#(a)#
hc.complete=hclust(dist(USArrests,method="euclidian"), method="complete")
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)

#(b)#
cut=cutree(hc.complete, 3)
cutdat=cbind(USArrests,cut)
rownames(cutdat[cut==1,])
rownames(cutdat[cut==2,])
rownames(cutdat[cut==3,])

#(c)#
xsc=scale(USArrests)
hc.scaled=hclust(dist(xsc,method="euclidian"), method="complete")
plot(hc.scaled,main="Complete Linkage (scaled)", xlab="", sub="", cex=.9)
cut2=cutree(hc.scaled, 3)
cutdat2=cbind(USArrests,cut2)
rownames(cutdat2[cut2==1,])
rownames(cutdat2[cut2==2,])
rownames(cutdat2[cut2==3,])

#(d)#

var(xsc)
var(USArrests)
summary(cutdat[cut==1,])
summary(cutdat[cut==2,])
summary(cutdat[cut==3,])
summary(cutdat2[cut2==1,])
summary(cutdat2[cut2==2,])
summary(cutdat2[cut2==3,])
