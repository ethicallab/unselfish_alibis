import praw
import polars as pl
from datetime import datetime
import os
from util import split_into_sentences, get_body
from dotenv import load_dotenv

load_dotenv() 
    
# Initialize Reddit object
reddit = praw.Reddit(
    client_id = os.getenv("CLIENT_ID"),
    client_secret = os.getenv("CLIENT_SECRET"),
    user_agent = os.getenv("USER_AGENT")
)

tesla_subreddits = ["teslamodel3","teslamodely", "teslamotors", "teslalounge", "teslafsd"]

dataframes = []
terms = ["assertive", "assertive mode"]

for tesla_subreddit in tesla_subreddits:

    for term in terms:
    
        submissions = reddit.subreddit(tesla_subreddit).search(term, limit = None)

        submissions_list = [submission for submission in submissions]

        id = [submission.id for submission in submissions_list]
        title = [submission.title for submission in submissions_list]
        text = [submission.selftext for submission in submissions_list]
        url = [submission.url for submission in submissions_list]
        time = [submission.created_utc for submission in submissions_list]
        subreddit = [tesla_subreddit] * len(id)

        df = pl.DataFrame({ "id": id, 
                            "title": title, 
                            "text": text, 
                            "url": url, 
                            "time": time, 
                            "subreddit": subreddit })
        
        dataframes.append(df)

df = pl.concat(dataframes).unique()
df = df.with_columns(pl.col("time").map_elements(lambda x: datetime.utcfromtimestamp(int(x)).strftime('%Y-%m-%d %H:%M:%S')))

df.write_csv("Posts.csv")

posts = df.with_columns(pl.struct(["title", "text"]).map_elements(lambda x: x["title"] + x["text"]).alias("all_text"))

posts = posts.with_columns(pl.col("all_text").map_elements(split_into_sentences)).explode("all_text")
posts = posts.select(["id", "all_text"]).rename({"all_text":"sentences"})

posts.write_csv("Posts_Sentences.csv")

post_ids = df["id"]

post_links = []
comment_ids = []
comment_body = []

for id in post_ids:
    submission = reddit.submission(id)
    comments = submission.comments.list()
    if len(comments) != 0:
        comment_id = [comment.id for comment in comments]
        body = [get_body(comment) for comment in comments]
        comment_ids.append(comment_id)
        comment_body.append(body)
        post_links.append([id] * len(body))

comment_body = [x for xs in comment_body for x in xs]
post_links = [x for xs in post_links for x in xs]
comment_ids = [x for xs in comment_ids for x in xs]

df = pl.DataFrame(
    {"post_id": post_links,
     "comment_id": comment_ids, 
     "body": comment_body})

df = df.with_columns(pl.col("body").map_elements(split_into_sentences)) \
    .explode("body") \
    .rename({"body":"sentence"})

df.write_csv("Comments.csv")
