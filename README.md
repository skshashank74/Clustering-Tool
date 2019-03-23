# Clustering Tool in R shinny

Clustering was one of the major part of my day to day job, so this tool was build to make the clustering excerise simple. You just need to upload the file through excel or connect with database (credential should be given in server) and you could directly build clusters from the data. If the data has missing values and outlier, this tool has the capability to treat it.

This tool has total five tabs:-

* Tab 1 :- Data Upload - Connect with source (Excel or DB) and select the variables for which you want to build the clusters for.
* Tab 2 :- EDA - Exploratory Data Analysis. Missing value and Outlier Treatment can be done in this tab. By default, we are doing 5-95 percentile of capping.    You can even do manual capping as per your requirement.
* Tab 3 :- Variable Treatment - Correlation Martrix, and the feature selected to do the clustering. Before proceding to do clustering we are normalizing the data in backend
* Tab 4 :- Cluster Creation - Elbow curve and Dendogram. You need to specify the number of cluster you want to build. In the same tab you can see the centroid of the cluster created
* Tab 5 :- Cluster Visualization - Different types of visualization of centroids. Visualization of cluster migration, to estimate which cluster is closer to one-another based on Eucliedian Distance.




