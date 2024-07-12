#!/bin/bash

echo "Run initial scrape of all posts"
python3 01_initial_scrape.py

echo "Perform sentiment analysis"
python3 02_sentiment_analysis.py

echo "Generate word cloud"
python3 03_wordcloud_generator.py

echo "Analysis with R"
Rscript 04_script.R