# Brain-Study-Publication-Analysis

#Part 1

Exploratory Analysis
Construct the probability distribution function of the year of first publication of faculty.

Construct the probability distribution function of the total citations of faculty.

Cluster the subject areas of faculty using the Louvain algorithm (k=5).

Generate a bar plot of the number of publications per subject area per continent pre-2014, post-2014, and their difference.

Generate a bar plot of the number of publications per CIP category per continent pre-2014, post-2014, and their difference.


#Part 2

Statistical Modeling
Construct the author networks for USA, Europe, and Australasia. In these networks, each node is a scholar and edges connecting scholars 
represent joint publications. Use Gephi for this construction. Scholar nodes, depending on their departmental CIP codes would be colored 
differently. Consider aggregating the numerous CIP codes in the dataset in three disciplinary clusters: Biological Sciences (green color), 
Medical Sciences (orange color), Engineering (magenta color). Compute the Page Rank for each scholar.

Construct three linear models, one for each geographic area - USA, Europe, and Australasia. The key predictor in each model would be 
scholar cross-disciplinarity. The response variable would be citation impact. Hence, the main goal of these models would be to investigate 
whether performing cross-disciplinary research in brain science correlates with impactful careers or not. If it does, then the brain 
science community is on a good path, because promise of success would attract more talent in cross-disciplinary research, which will 
eventually make the difference. If not, then new science policies would need to be instituted to change the situation.
