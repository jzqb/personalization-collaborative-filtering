# personalization-collaborative-filtering
Personalization Project
Cindy Chen Wu (https://github.com/ccw2145), Maura Fitzgerald (https://github.com/maurafitzgerald2), Carolyn Silverman (https://github.com/carsilverman), Gaurav Singh (jzqb)

Part I -- Fundamentals	 (Due November 7th)			 							
Build two very simple collaborative filtering models. You may use published packages or methods - the goal of this exercise is to gain a practical intuition for these types of common models, and to develop methods to test and explore them. 			
						
-  Choose a dataset / recommendation domain. Possible datasets:			
https://gist.github.com/entaroadun/1653794							
https://gab41.lab41.org/the-nine-must-have-datasets-for-investigating-recommender-systems-ce9421bf981c			
http://www.recsyswiki.com/wiki/Category:Dataset		
iHR - stay tuned 					 					
				
- Treat this as a case study and think about it from a business perspective. What is your objective? What are you trying to optimizing and what are you willing to sacrifice?

-  For this section, develop with a small dataset (< 10000 users / < 100 items) 

-  Build two brute-force collaborative filtering algorithms:				 	
1. Neighborhood-based (item or user) (Gauro)
	-build both item-based and user-based models; use whichever performs better
	2. Model-based (Maura)
		-association rues (arules package)
		-unconstrained matrix factorization

- Develop evaluation methods: (Carolyn and Cindy)
1. Cross-validation setup
	-write one function and use it to validate both models
2. Accuracy on training and test data
3. Coverage on training and test data
		Include plots

-Systematically try a range of hyper parameters for your models (e.g. neighborhood size or number of latent dimensions). Record how your evaluation metrics change as a function of these parameters 
	-plot hyperparameters against various evaluation metrics
- What other design choices might you consider?

-How do the evaluation metrics change as a function of your model size? Systematically sample your data from a small size to a large size
1. Does the overall accuracy change?
2. How does run-time scale with data size

- Referencing your case study set up from above, how do these methods meet your hypothetical objectives? Would you feel comfortable putting these solutions into production at a real company? What would be the potential watch outs? 										
Deliverables & Guidelines
						
The main deliverable is a GitHub repository with the following:			
A README file outlining the repository contents		
A requirements file with all software/package requirements to run your code		
A top level directory for Part I						 			
• Include a notebook or markdown with your approach and basic results 
 A top level directory for Part II
		• Include a notebook or markdown with your approach and basic results
						
Imagine a future employer looking at this repository. They would evaluate your project based on technical correctness, but they would also look for coherence and creativity. An employer would look for thoroughness (how does the solution scale, were the hyper parameters tuned, will this solution discover novel recommendations, etc). As a Data Scientist you would also be expected to interface with other departments in the business. Are the major highlights in your work clear? Did you call out important caveats? 
				
