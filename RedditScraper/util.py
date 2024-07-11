# -*- coding: utf-8 -*-
import re
import polars as pl 
from multiprocessing.pool import ThreadPool 
import spacy

nlp = spacy.load("en_core_web_sm")

alphabets= "([A-Za-z])"
prefixes = "(Mr|St|Mrs|Ms|Dr)[.]"
suffixes = "(Inc|Ltd|Jr|Sr|Co)"
starters = "(Mr|Mrs|Ms|Dr|Prof|Capt|Cpt|Lt|He\s|She\s|It\s|They\s|Their\s|Our\s|We\s|But\s|However\s|That\s|This\s|Wherever)"
acronyms = "([A-Z][.][A-Z][.](?:[A-Z][.])?)"
websites = "[.](com|net|org|io|gov|edu|me)"
digits = "([0-9])"
multiple_dots = r'\.{2,}'

def split_into_sentences(text: str) -> list[str]:
    """
    Split the text into sentences.

    If the text contains substrings "<prd>" or "<stop>", they would lead 
    to incorrect splitting because they are used as markers for splitting.

    :param text: text to be split into sentences
    :type text: str

    :return: list of sentences
    :rtype: list[str]
    """
    text = " " + text + "  "
    text = text.replace("\n"," ")
    text = re.sub(prefixes,"\\1<prd>",text)
    text = re.sub(websites,"<prd>\\1",text)
    text = re.sub(digits + "[.]" + digits,"\\1<prd>\\2",text)
    text = re.sub(multiple_dots, lambda match: "<prd>" * len(match.group(0)) + "<stop>", text)
    if "Ph.D" in text: text = text.replace("Ph.D.","Ph<prd>D<prd>")
    text = re.sub("\s" + alphabets + "[.] "," \\1<prd> ",text)
    text = re.sub(acronyms+" "+starters,"\\1<stop> \\2",text)
    text = re.sub(alphabets + "[.]" + alphabets + "[.]" + alphabets + "[.]","\\1<prd>\\2<prd>\\3<prd>",text)
    text = re.sub(alphabets + "[.]" + alphabets + "[.]","\\1<prd>\\2<prd>",text)
    text = re.sub(" "+suffixes+"[.] "+starters," \\1<stop> \\2",text)
    text = re.sub(" "+suffixes+"[.]"," \\1<prd>",text)
    text = re.sub(" " + alphabets + "[.]"," \\1<prd>",text)
    if "”" in text: text = text.replace(".”","”.")
    if "\"" in text: text = text.replace(".\"","\".")
    if "!" in text: text = text.replace("!\"","\"!")
    if "?" in text: text = text.replace("?\"","\"?")
    text = text.replace(".",".<stop>")
    text = text.replace("?","?<stop>")
    text = text.replace("!","!<stop>")
    text = text.replace("<prd>",".")
    sentences = text.split("<stop>")
    sentences = [s.strip() for s in sentences]
    if sentences and not sentences[-1]: sentences = sentences[:-1]
    return sentences

def get_body(comment):
    """
    Get body of the reddit comments, and exclude the ones that throw exceptions
    """
    try:
        body = comment.body
        return body
    except:
        return ""

def multithread_map(dataframe: pl.DataFrame, num_threads: int, map_function, target_column: str) -> pl.DataFrame:
    """
    Takes in polar Dataframe and applies a function on a target column
    based on the number of threads

    Due to race conditions and implementation, the returned dataframe
    will NOT be in the same order
    """

    ## Set up partition of dataset
    dataframe = dataframe.with_row_count().with_columns(
            pl.col("row_nr").map_elements(lambda x: x % num_threads))

    ## Partition based on the number of threads
    dfs = dataframe.partition_by("row_nr")

    ## Internal function to apply the following function
    def map_partition(which_partition):
        return dfs[which_partition].with_columns(
            pl.col(target_column) \
              .map_elements(map_function) \
              .alias("sentiment")
            )

    ## Run thread pool
    results = []
    pool = ThreadPool(num_threads)
    for i in range(0, num_threads):
        results.append(pool.apply_async(map_partition, (i,)))

    ## Close and collect threads
    pool.close()
    pool.join()

    ## Obtain results and concatenate all the mappde values
    results = [result.get() for result in results]
    df = pl.concat(results).drop("row_nr")
    
    return df

def remove_stop_words(sentence): 
  # Parse the sentence using spaCy 
  doc = nlp(sentence) 
  
  # Use a list comprehension to remove stop words 
  filtered_tokens = [token for token in doc if not token.is_stop] 
  
  # Join the filtered tokens back into a sentence 
  return ' '.join([token.text for token in filtered_tokens])



