# Clustering Tool in R shinny

Clustering was one of the big part of my day to day job, so this tool was build to make the clustering excerise simple. You just need to upload the file through excel or connect with database (credential should be given in server) and you could directly build clusters from the data. If the data has missing values and outlier, this tool has the capability to treat it.

This tool has total five tabs:-

* Tab 1 :- Data Upload - Connect with source (Excel or DB) and select the variables for which you want to build the cluster
* Tab 2 :- EDA - Exploratory Data Analysis. Missing value can be dealt in this tab. By default, we are doing 5-95 percentile of capping.    You can even do manual capping as per your need. 
* Tab 3 :- Variable Treatment - Correlation of different variable with each other, and the feature selected to do clustering. Before proceding to clustering we are normalizing the data in the backend
* Tab 4 :- Cluster Creation - Elbow curve and Dendogram. You need to specify the number of cluster you want to build. In the same tab you can the centroid of the cluster created
* Tab 5 :- Cluster Visualization - Different types of visualization of centroids. Visualization of cluster migration, which cluster is closer to one-another based on Eucliedian Distance.




