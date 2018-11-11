import sys
import os
import pandas
import json

m = os.environ['COMPUTERNAME']
if m=='PHANTOM':
    path = 'E:/Documents/GitHub/GetOldTweets-python'
else:
    path = 'C:/Users/Bruno-NB/Documents/GitHub/GetOldTweets-python'
sys.path.insert(0,path)
if sys.version_info[0] < 3:
    import got
else:
    import got3 as got
# Importando bibliotecas

id (str)
permalink (str)
username (str)
text (str)
date (date)
retweets (int)
favorites (int)
mentions (str)
hashtags (str)
geo (str)

tweetCriteria = got.manager.TweetCriteria().setQuerySearch('bolsonaro').setSince("2018-05-01").setUntil("2018-06-01").setMaxTweets(1)
tweet = got.manager.TweetManager.getTweets(tweetCriteria)
print([tweet[t] for t in range(len(tweet))])
print(tweet[0].id)





def printTweet(t):
    print("Id: %s" % t.id)
	print("Username: %s" % t.username)
	print("Retweets: %d" % t.retweets)
	print("Text: %s" % t.text)
	print("Mentions: %s" % t.mentions)
	print("Hashtags: %s\n" % t.hashtags)
	
	# Example 1 - Get tweets by username
	tweetCriteria = got.manager.TweetCriteria().setUsername('barackobama').setMaxTweets(1)
	tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]

	printTweet("### Example 1 - Get tweets by username [barackobama]", tweet)

	# Example 2 - Get tweets by query search
	tweetCriteria = got.manager.TweetCriteria().setQuerySearch('europe refugees').setSince("2015-05-01").setUntil("2015-09-30").setMaxTweets(1)
	tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]

	printTweet("### Example 2 - Get tweets by query search [europe refugees]", tweet)

	# Example 3 - Get tweets by username and bound dates
	tweetCriteria = got.manager.TweetCriteria().setUsername("barackobama").setSince("2015-09-10").setUntil("2015-09-12").setMaxTweets(1)
	tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]

	printTweet("### Example 3 - Get tweets by username and bound dates [barackobama, '2015-09-10', '2015-09-12']", tweet)



    tweetCriteria = got.manager.TweetCriteria().setQuerySearch('bolsonaro').setSince("2018-05-01").setUntil("2018-06-01").setMaxTweets(1)
    tweet = got.manager.TweetManager.getTweets(tweetCriteria)[0]
    printTweet("Test", tweet)


