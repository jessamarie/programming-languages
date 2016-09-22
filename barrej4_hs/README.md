Jessica Barre

Features: three functions and one helper function to compute alpha, two functions to compute beta reduction, and two functions to compute eta reduction

main.hs:
The program runs a reducer function which consecutively calls alpha, beta and eta to reduce the expression. I wanted to keep them all separate, but I found it necessary to call eta once in beta, and beta once in eta, in order to produce accurate results. I did not change variables based on whether they would capture free variables, or not, but instead changed all the bound variables with new atoms instead.

This assignment currently passes all of the test-cases on the homework. I made test cases of my own also.

