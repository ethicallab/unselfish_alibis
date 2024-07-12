from wordcloud import WordCloud
import matplotlib.pyplot as plt
import spacy
import re
import polars as pl
from util import remove_stop_words

nlp = spacy.load("en_core_web_sm")

if __name__ == "__main__":
    ## Read in the sentences of Posts
    posts = pl.read_csv("./Data/Posts_Sentences.csv")

    posts = posts.filter(pl.col('sentences').str.contains("assertive"))
    posts_text = " ".join(posts["sentences"].to_list())

    ## Read in sentences in Comments
    comments = pl.read_csv("./Data/Comments_Sentences.csv")

    comments = comments.filter(pl.col('sentence').str.contains("assertive"))
    comments_text = " ".join(comments["sentence"].to_list())

    ## Combine both of them
    text = posts_text + comments_text

    ## Standardize text and remove stop words from document
    doc = nlp(text)
    text = ' '.join([token.text for token in doc if not token.is_stop])
    text = text.lower()

    ## Tokenize keywords
    text = re.sub("average assertive", "assertive", text)
    text = re.sub("assertively", "assertive", text)
    text = re.sub("assertiveness", "assertive", text)
    text = re.sub("aggressively", "aggressive", text)
    text = re.sub("aggressiveness", "aggressive", text)

    # Wordcloud settings
    wordcloud = WordCloud(width=2000, height=1000, random_state=14, max_words=75, background_color="white", prefer_horizontal=.99, max_font_size=250, min_font_size=50)

    # Process words and generate wordcloud
    wc_dict = wordcloud.process_text(text)
    del wc_dict['assertive']
    wordcloud = wordcloud.generate_from_frequencies(wc_dict)

    # Plot figure and save
    plt.figure(figsize=(30, 16))
    plt.imshow(wordcloud)
    plt.axis("off")
    plt.savefig("./Graphics/wordcloud.png", bbox_inches='tight')

    ## Convert dictionary into dataframe
    words = list(wc_dict.keys())
    freq = list(wc_dict.values())
    df = pl.DataFrame({"word": words, "freq": freq}).sort("freq", descending=True)

    df.write_csv("./Data/KeywordCounts.csv")