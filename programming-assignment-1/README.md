Jessica Barre

Features: three functions and one helper function to compute alpha, two functions to compute beta reduction, and two functions to compute eta reduction

The program runs a reducer function which consecutively calls alpha, beta and eta to reduce the expression. I wanted to keep them all separate, but I found it necessary to call eta once in beta, and beta once in eta, in order to produce accurate results. 

This assignment currently passes all of the test-cases on the homework.

