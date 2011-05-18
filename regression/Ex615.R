install.packages('car');
library(car);

B14.table <- read.csv("C:/Users/th/git/mva/regression/B14.table.csv");
analysis.data = B14.table[,c(1:4,6)]

# variance/covariance matrix
round(var(analysis.data),2)

#pearson correlation 
round(cor(analysis.data),2)

# Simple function to plot correlation between all variables 
# panel.pearson <- function(x, y, ...) {
#     horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
#     vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
#     text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
# }
# 
# pairs(iris[1:4], main = "Electronic Converter Data", pch = 21, bg = c("red","green3","blue")[unclass(iris$Species)], upper.panel=panel.pearson);

panel.hist <- function(x, ...)
{ 
  usr <- par("usr"); 
  on.exit(par(usr));
  par(usr = c(usr[1:2], 0, 1.5) );
  h <- hist(x, plot = FALSE);
  breaks <- h$breaks; nB <- length(breaks);
  y <- h$counts; 
  y <- y/max(y);
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...);
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); 
  on.exit(par(usr));
  par(usr = c(0, 1, 0, 1));
  r <- abs(cor(x, y));
  txt <- format(c(r, 0.123456789), digits=digits)[1];
  txt <- paste(prefix, txt, sep="");
  if(missing(cex.cor)) cex <- 0.6/strwidth(txt);
  text(0.5, 0.5, txt, cex = cex);
}

pairs(analysis.data, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist);
pairs(analysis.data, lower.panel={}, upper.panel=panel.cor, diag.panel=panel.hist);

# fit initial model 
initial.lm = lm(y ~ x1+x2+x3+x4, data=B14.table);
summary(initial.lm);

# compute H, DFFITS, DFBETAS
influence.measures(initial.lm)

remove2.subset = c(1,3:25);
summary(lm(y ~ x1+x2+x3+x4, data=B14.table, subset=remove2.subset));


summary(initial.lm);

remove24.subset = c(1,3,5:25);
summary(lm(y ~ x1+x2+x3+x4, data=B14.table, subset=remove24.subset));






