
plot(x1,y, type='n', xlab='Speed', ylab='Tool life');
i = 1;
for (c in c('A','B')) {
    subset = as.logical(x2==c);
    points(x1[subset], y[subset], pch=23, bg=colors[i],cex=2);
    i = i+1;
}
