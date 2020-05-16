# Stock Portfolio Risk Manager

This is an Shiny app built with R to democratise the more in-depth technical study of stocks. The main goal is to build an open-source Shiny app for quantitative stock analysis.

## Usage or installation

You can acess the app in the shinyapp.io platform with this [link](https://adriel-martins.shinyapps.io/Portfolio_Manager/?_ga=2.210384469.1377155949.1589635077-482263549.1556407394).

But, if you wish to run in your own computer, you must have R software installed and the dependent packages. Those packages are on the script 'global.R'. Then, you must clone the git repository:

```git
git clone https://github.com/Martins6/Stock_Portfolio_Manager
```
Next you must set the working directory in R to the folder you just downloaded.

```r
setwd('path_to_the_folder')
```
Finally, run the "app.R" script.

## In-App Flow and Input Details

The app follows the natural way of data exploration. First, descriptive, then modelling. There is also the About section in the app that explains all the statistics behind the app.

Please, notice that the stock inputs are following the symbols of the Yahoo Finance. You just have to enter on their site and check if the stock symbol is the one that you want. Normally, in the american stock market the ticket symbol is the same. However, for example, in the brazillian market it always differ. 

## Contributing

There is an empty folder called 'contributions'. This folder is meant for those that wish to contribute to some quantitative statistical tool to stock portfolio analysis but doesn't have the knowledge of Shiny. You can leave a reproducible example script in the folder and we will convert it to fit on the app.

Otherwise, feel free to tweak and improve however you want! Remember that the main goal is to build a open-source app for quantitative stock analysis.

Pull requests are very much welcome! For major changes, please open an issue first to discuss what you would like to change.

## License
[GPL - 3.0](https://choosealicense.com/licenses/gpl-3.0/)
