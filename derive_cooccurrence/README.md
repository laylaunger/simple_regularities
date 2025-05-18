This script uses the preprocessed CHILDES corpora to calculate co-occurrence regularities between pairs of words.

Co-occurrence regularities are measured using t-scores and variants of MI, as described here: 

Evert, Stefan (2008). Corpora and collocations. In A. Lüdeling and M. Kytö (eds.), Corpus Linguistics. An International Handbook, chapter 58, pages 1212-1248. Mouton de Gruyter, Berlin, New York.

Co-occurrence regularities are calculated within sliding windows of words, following these steps:

(1) Load preprocessed corpora

(2) Define a helper function to extract co-occurrence scores

(2) Define a function that measures word pair co-occurrence frequencies within a specified
    window size, and converts these frequencies to t-scores/variants of MI
    
(3) Use function to measure co-occurrence regularities within specified window sizes (here, 3, 7, and 11 words)
    
(4) Combine regularities in each window size into a single object and save as an .rds file
