🏆 Bayesian Analysis of British Literary Prize Outcomes (1990–2022)

This project analyzes the probability of winning a literary prize based on author characteristics, using a Bayesian multilevel logistic model implemented via brms. The goal is to investigate whether attributes such as ethnicity, gender, genre, and educational attainment affect the likelihood of receiving literary recognition.

🔍 Method Overview

We model the binary outcome (winner vs. non-winner) using a non-linear multilevel formulation. Author education is treated as a monotonic effect with a Dirichlet prior to capture ordered uncertainty, while gender, ethnicity, and genre are included as varying effects.

📊 Key Visual Summaries

1. Distribution of Win Probabilities Taken by prior (-1, 1)

<img width="834" height="880" alt="2ac58e68-4b23-4400-aebc-d7044d047be8" src="https://github.com/user-attachments/assets/a0693098-2131-4976-abd9-669548449b0a" />


The histogram shows the distribution of estimated win probabilities across all posterior draws. Most draws suggest low individual probabilities, skewed toward 0.2, reflecting the overall rarity of prize-winning events.

2. Variance Components Across Grouping Variables

<img width="834" height="880" alt="642502f7-2724-430c-88a3-6f2604d67638" src="https://github.com/user-attachments/assets/ad8a1217-942e-4fdc-9d6f-d51d668e1cf6" />


Posterior estimates of standard deviations (σ) for the group-level effects. Genre exhibits the greatest heterogeneity in its influence on winning, while ethnicity and gender show smaller variance.


3. Posterior Density Plots of Random Effects

<img width="834" height="880" alt="c01e2bb5-c64a-4a3d-9cdb-ecae65a64ccf" src="https://github.com/user-attachments/assets/71aa359f-769d-43a1-8274-02c6140c0783" />


This visual emphasizes the relative uncertainty and spread of each random effect. All group-level parameters cluster toward zero, with long right tails indicating sparse high-impact levels.

4. Raw Ethnic Differences in Winning Rates

<img width="834" height="880" alt="35f76b8e-d1ac-49c4-a0dc-e503a89b5cde" src="https://github.com/user-attachments/assets/84644cbe-cd0e-420c-9fee-e70e1ad22aa9" />


Estimated win rates by ethnic group suggest disparities. White British authors have the highest observed probability, while African and Asian authors show lower rates, highlighting potential inequities in award distribution.



5. Dirichlet Prior Samples for Education Levels

<img width="834" height="880" alt="12e83e63-2f3d-4053-8270-faa279d886ea" src="https://github.com/user-attachments/assets/e068a5e6-ecff-43b8-8d1a-9a65e8a8d1e7" />


The Dirichlet(2,2,…) prior encodes uncertainty over the ordered educational levels used in the monotonic effect (mo(degree)). The densities show most weight on lower levels (e.g., unknown, certificate, diploma), consistent with conservative prior beliefs.


6. Posterior Correlation Matrix for Educational Effects

<img width="834" height="880" alt="86eb417d-a4d7-4b11-8049-7520411660a9" src="https://github.com/user-attachments/assets/0cacd0db-53d5-4b82-b3a6-6d2ff19b6395" />


Pairwise posterior correlations among δ parameters for each educational level. These parameters reflect the relative monotonic contributions of educational attainment to winning likelihood.


7. Cumulative Sum of Monotonic Effects (δ)

<img width="834" height="880" alt="2373b437-682a-4a4b-97fb-599b163f7df9" src="https://github.com/user-attachments/assets/ec6b327e-c9de-4d89-8006-5a91242e1af8" />


Cumulative δ effects show the ordered contribution of each educational level to the log-odds of winning. Contrary to expectations, advanced degrees do not exhibit significantly higher contributions.


🧾 Data & Code
	•	Dataset: british_literary_prizes-1990-2022.csv
	•	Modeling script: prize.R
	•	Package dependencies: brms, ggplot2, dplyr, tidyr, tidybayes, posterior, cmdstanr

📌 Conclusion

The results suggest genre is the most influential grouping variable, while ethnicity and gender also contribute moderately. Education shows no clear monotonic advantage for higher degrees in predicting win probability. Overall, this model provides a probabilistic framework to explore systemic patterns in literary prize outcomes.
