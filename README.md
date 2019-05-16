# Facilitation, not competition, in flowering timing in a diverse tropical hummingbird-visited plant community 

Authors: Ben Weinstein, Holger Beck, John Clark, Mathieu Perret, Catherine Graham

Abstract:
  Local co-occurrence of related species depends in part on the niche overlap among species competing for limited resources. Connecting natural history within local assemblages with hypothesized mechanisms of co-occurrence from biogeographic scales remains an ongoing challenge in community ecology. For example, competition for pollination services is often cited as key factor in promoting morphological evolution and diversification in tropical flowering plants. Using five years of information on hummingbird visitation and flowering phenology, we tested whether co-occurring hummingbird pollinated plant species in the family Gesneriaceae stagger their flowering period to reduce pollen competition, or flower at similar periods to maximize pollination rewards. Monthly flowering transects along a wide elevation gradient showed consistent temporal and spatial patterns in flowering among species. Species tend to co-flower with other species that are visited by similar hummingbird species. These results highlight facilitation, rather than competition, in tropical co-flowering plants. We find little evidence of phenological staggering among plants visited by similar hummingbird species as would be expected if hummingbird-mediated selection has strongly influenced the evolution of floral morphology and life history. We discuss several potential scenarios that could explain the potential facilitation among plants despite overlap in pollinator visitation. 
  
Repository Contents:

Data/: Data files for the analysis. Files underwent significant taxonomy cleaning and name standardization. Final files are in /cleaned. The transects.csv hold the flowering data for each elevation transect. The interactions_all.csv contains hummingbird visitation data to all plants at the site. See GenerateInteractions.Rmd for creating a Gesneriaceae only list.

GenerateInteractions.Rmd: Create the poisson interaction model for niche overlap among hummingbird species

Phenology.Rmd: Main analysis script for models of covariance.

model/ Folder of bayesian covariance models to be run in JAGS.

* model/threshold_baseline_site.R is the time indepedent model from the manuscript

* model/threshold_attraction.R is the visitor attraction model

* model/threshold_repulsion.R is the visitor repulsion model


