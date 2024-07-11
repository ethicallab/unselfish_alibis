from transformers import pipeline
import polars as pl
from util import multithread_map

## Import SiEBERT for sentiment analysis
sentiment_analysis = pipeline("sentiment-analysis",model="siebert/sentiment-roberta-large-english")

if __name__ == "__main__":
    ## Obtain posts that are broken down into sentences
    posts = pl.read_csv("./Data/Posts_Sentences.csv")

    ## Apply sentiment analysis function
    posts = posts.with_columns(
        pl.col("sentences") \
        .map_elements(sentiment_analysis) \
        .alias("sentiment"))
    
    ## Extract the Label and Score respectively
    posts = posts.with_columns(
        pl.col("sentiment").map_elements(lambda x: x[0]["label"]).alias("label"),
        pl.col("sentiment").map_elements(lambda x: x[0]["score"]).alias("score")
    ).drop("sentiment")

    ## Write output
    posts.write_csv("./Data/Posts_Sentences.csv")

    ## Read Comments
    comments = pl.read_csv("./Data/Comments_Sentences.csv")

    ## Multithread sentiment analysis
    comments = multithread_map(
        dataframe=comments,
        num_threads=8,
        map_function=sentiment_analysis,
        target_column="sentence"
    )
    
    ## Extract the Label and Score respectively
    comments = comments.with_columns(
        pl.col("sentiment").map_elements(lambda x: x[0]["label"]).alias("label"),
        pl.col("sentiment").map_elements(lambda x: x[0]["score"]).alias("score")
        ).drop("sentiment")
    
    ## Write File
    comments.write_csv("./Data/Comments_Sentences.csv")