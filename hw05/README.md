#Homework05 Github Repository 

You will be able to reproduce my research if you download the entire github repository as it is and follow the following steps. 

##Steps to Reproduce the Analysis 
1. Clone this [git repository](https://github.com/limchengyee/hw05) in RStudio 
2. Ensure that your working directory is where the .md and .Rmd files are located in your computer 
3. Ensure that you have ```foreign```, ```stringr```, and ```ggmap``` packages installed
4. Use the Rmd files to reproduce the output in the md files 

Now, I will further explain each component of the github repository below. 

##aid_infrastructure.md/.Rmd
The md/Rmd file consists the importing, tidying and analysis of the compiled dataset. 
The [md file](https://github.com/limchengyee/hw05/blob/master/aid_infrastructure.md) shows both the code and the results of the code. 
The [Rmd file](https://github.com/limchengyee/hw05/blob/master/aid_infrastructure.Rmd) only shows the code that can reproduce the same results, as in the report, by downloading .

##appendix.md/Rmd
The appendix consists of the plots of percentage change in telecommunications subscribers in all countries in the dataset. 
The [md file](https://github.com/limchengyee/hw05/blob/master/appendix.md) shows only the plots of the code. 
The [Rmd file](https://github.com/limchengyee/hw05/blob/master/appendix.Rmd) shows only the code to reproduce the plots in the appendix.md. 

##report.md/Rmd
The report consists of write-up and the analysis of the results of exploratory data analysis. 
The [md file](https://github.com/limchengyee/hw05/blob/master/report.md) consists of the insights of our exploratory data analysis of telecommunications and aid. 
The [Rmd file](https://github.com/limchengyee/hw05/blob/master/report.Rmd) consists of the writeup and the code to generate the plots in the report.

##Data Folder
In the [data folder](https://github.com/limchengyee/hw05/tree/master/data), there are three csv files. 
The two in the main data folder are the [Aid Effectiveness](http://data.worldbank.org/topic/aid-effectiveness) and [Infrastructure](http://data.worldbank.org/topic/infrastructure) compiled indicators from the World Bank. 
The last csv file is in a subfolder, [labels](https://github.com/limchengyee/hw05/tree/master/data/labels), and consists of labels of the countries in the dataset.

##Map Folder
The [map folder](https://github.com/limchengyee/hw05/tree/master/map) contains the relevant files for plotting a world map in RStudio. 

##Files Folder
There are three files folder and they consist for the images generated from the .md file. 
* [aid_infrastructure_files](https://github.com/limchengyee/hw05/tree/master/aid_infrastructure_files/figure-markdown_github)
* [appendix_files](https://github.com/limchengyee/hw05/tree/master/appendix_files/figure-markdown_github)
* [report_files](https://github.com/limchengyee/hw05/tree/master/report_files/figure-markdown_github) 

