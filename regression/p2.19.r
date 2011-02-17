
x = seq(1,10.5,20);

# generare 500 samples 
	for (i in c(1:500))  {
		e = 	rnorm(n, mean = 0, sd = 16);
		y = 50 + 10*x + e;
		
	}
