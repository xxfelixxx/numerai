#!/bin/bash

echo 'Updating all of the plots with the latest data from numerai_training_data.csv'
Rscript plot.R

echo 'Moving plots to html directory'
mv *.png html
cd html

echo 'Updating table.html'
perl create_html.pl

echo 'Updating summary.html'
perl create_summary_html.pl

echo 'Adding latest plots to git'
git add *.html *.png
git commit -m 'Updated plots with the latest numerai data.'
DIR=`pwd`
HTML="file://$DIR/index.html"
SUMM="file://$DIR/summary.html"
echo 
echo "Latest plots are visible at: $HTML"
