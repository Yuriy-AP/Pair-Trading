# Pair-Trading
The One-Sided Pair Trading aims to test a version of Pair trading strategy, where short selling is not allowed. 

Traditional pair trading provides to: 
(1) identify pair(s) of stocks 
(2) go short on undervalued stock and go long on overvalued stock  - when paired stocks are relatively far from each other 
(3) close the position when the paired stocks come back to each other

This project aims to: 
(1) Test pair trading when short selling is not allowed
(2) Optimize the strategy parameters, such as (a) finding 'optimal' trigger distances between the paired stocks as buy/sell signals, (b) test using k>=2 pairs simultaneously (isntead of 1 pair), (c) finding 'optimal' length of 'testing' (when pairs are found) and 'trading' (trading period before re-balancing) periods. 
(3) Performance is assessed versus benchmark - S&P500 index. 
Performance assessment indicators: (a) Cumulative return, (b) Annualized return, (c) Sharpe ratio, (d) Jensen's alpha, (e) portfolio Beta.   


See available results in the PDF file
Code is available in R. 
