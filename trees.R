
library(maptools)
library(rpart)
library(tree)
library(party)
library(cluster)
library(partykit)

raw.orig <- read.csv(file = file.choose(), 
                     header = T, 
                     sep = "\t")
View(raw.orig)

raw = subset(raw.orig, 
             select = c("Metal","OTW","AirDecay","Koc"))
 
row.names(raw) = raw.orig$CASNumber
raw = na.omit(raw);
 
frmla = Metal ~ OTW + AirDecay + Koc
 
# Metal: Core Metal (CM); Metal (M); Non-Metal (NM); Core Non-Metal (CNM)
 
fit = rpart(frmla, method="class", data=raw)
 
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

plot(fit, uniform=TRUE, main="Classification Tree for Chemicals")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
 
# tabulate some of the data
table(subset(raw, Koc>=190.5)$Metal)



library(maptree)
library(cluster)
draw.tree( clip.rpart (rpart ( raw), best=7),
nodeinfo=TRUE, units="species",
cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)
 
plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)

