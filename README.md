# optiscape

The codes support the analysis included in the paper: Bonato M., Burian A., Equihua J.A., Bartowski B. Cord A. F., Strauch, M. (in preparation). Optimizing the spatial allocation of Agri-Environmental Practices to minimize trade-offs and achieve win-win solutions in multifunctional landscapes


## optmodels
In the folder "optmodels" there is the code used to model habitat quality, probability of connectivity and SWAT related indicators (model_all). This is used for the multi-objectives optimization of crop yield, water quality, habitat connectivity and habitat quality.

The code "model_all_fallow" is used for the scenarios with fallow lands as proposed measure to be implemented. For the scenarios with reduced fertilization the input data on fertilization need to be changed.  


## output_analysis
In the folder "output_analysis" there are the codes used to postprocess the optimization results (baseline and scenarios), calculating the hypervolume and retrieving the fitness values and the genome of the Best Solutions. Fitness values and genomes are used as input for all the following analyses.


## optresults
The "Trade_offs_baseline" code is used to plot the four-dimensional Pareto frontier (= trade-off curve) and to analyse the trade-off between all the considered objectives, as well as for 2-dimensional Pareto-frontiers. 
The "Trade_offs_HabCnt" code is used to model habitat connectivity separately for the different animal species considered. It is run using as input the Best Solution coming from the optimization (baseline scenario).
The "Trade_offs_scenarios" code is used to plot together the four-dimensional Pareto frontiers of the baseline and policy-based scenarios.
The "Trade_offs_barplot" code is used to plot the barplots of the range of trade-offs for the baseline and policy-based scenarios and for the each couple of objectives in the baseline scenario.

The "Cluster-analysis" code is used to individuate clusters of solutions along the Pareto-frontier (baseline scenario)
The "Freq_analysis_all" code is used to analyse the frequency of AEP implementation when considering all the Best Solutions together.
The "Freq_analysis_clusters" code is used to analyse the frequency of AEP implementation for the Best Solutions beloging to each cluster.
