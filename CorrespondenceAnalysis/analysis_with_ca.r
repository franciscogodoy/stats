install.packages('ca');
library(ca)

# smoke dataset contains frequencies of 
# smoking habits (none, light, medium and heavy) 
# for staff groups (senior managers, junior managers, senior employees, junior employees and secretaries)
	data("smoke");
	mytable = smoke;

	fit <- ca(mytable);

	# basic results 
	print(fit) 

	# extended results 
	summary(fit);

	# symmetric map
	plot(fit); 

	# asymmetric map
	plot(fit, mass = TRUE, contrib = "absolute", map = "rowgreen", arrows = c(FALSE, TRUE));
