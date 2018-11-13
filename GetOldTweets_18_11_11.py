import sys
import os
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

tweetCriteria = got.manager.TweetCriteria().setQuerySearch('bolsonaro').setSince("2018-05-01").setUntil("2018-06-01").setMaxTweets(2)
t = got.manager.TweetManager.getTweets(tweetCriteria)

jsonData = []
for i in range(len(t)):
    jsonData.append({'created_at':"t[i].date", 'id':int(t[i].id), 'id_str':t[i].id, 'text':t[i].text, 
                       'source':t[i].permalink, 'user':{'name':t[i].username},
                       'geo':t[i].geo, 'retweet_count':t[i].retweets, 'favorite_count':t[i].favorites,
                       'entities':{'hashtags':t[i].hashtags, 'user_mentions':t[i].mentions}
                      })

file = open('st_py.json', 'w')
#jsonData = {'text':'sucess'}
json.dump(jsonData, file)
file.close()


with open('st_model.json','r') as f:
	#data = json.load(f.read())
	print('one: '+type(f))
	print('TWO: '+str(f))
	data = json.load(f)
	print(data[0]["id"])


'''
    data = json.loads(f)
    for i in range(len(data)):
        print(data[i])
        #json.dumps(data[i], indent=4)
'''
'''
id (str) -
permalink (str) -
username (str) -
text (str) -
date (date) -
retweets (int) -
favorites (int) -
mentions (str)
hashtags (str) -
geo (str) -

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


'''