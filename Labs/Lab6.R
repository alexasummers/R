#8

library(Matrix)
# a --> use precomp() = show sdev

pr.out = prcomp(USArrests, scale = TRUE)
pr.out
names(pr.out)
#find variance
pr.var = pr.out$sdev^2
#Eg. 10.8
#PVE is mth principal component divided by total variance

pve = pr.var / sum(pr.var)
pve
plot(pve)
plot(pve, type = "l", col = 4, xlab = "Principal Component", ylab = "PVE")
sum(pr.var)
cumsum(pve)
plot(cumsum(pve), type="l", col = 4, xlab = "Principle component", ylab = "Cumsum PVE")

#b using the matrix of variable loadings --> rotation
loadings = pr.out$rotation
USArrests2 = scale(USArrests)
sumvar = sum(apply(as.matrix(USArrests2)^2, 2, sum))
# the results are the same as A
pve1 = apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum) / sumvar
pve1
plot(pve1, type="l", col = 4, xlab = "Princpal Component", ylab = "PVE")
plot(cumsum(pve1), type="l", col=4, xlab = "Principal Component", ylab="Cumsum PVE")


#9
#Perform hierarchical clustering on the states.

#a Use complete linkage, distance matrix of USArrests
set.seed(2)
hc.complete = hclust(dist(USArrests), method = "complete")
plot(hc.complete)

#b
cutree(hc.complete, 3)

#c Euclidian distance after scaling variables

sd.data = scale(USArrests)
hc.complete.sd = hclust(dist(sd.data), method = "complete")
plot(hc.complete.sd)

#d
cutree(hc.complete.sd, 3)

table(cutree(hc.complete, 3))
table(cutree(hc.complete.sd, 3))
tab = table(cutree(hc.complete, 3), cutree(hc.complete.sd, 3))
tab
same = (tab[1,1] + tab[2,2] + tab[3,3]) /sum(tab)
cat('It appears that ', same*100, '% of the observations are assigned to the same clusers')

#Scaling the variables affect the clusters obtained
