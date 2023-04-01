import praw
import pandas as pd

posts = []
reddit = praw.Reddit(client_id='x', client_secret='y', user_agent='z')

all = reddit.subreddit("progresspics") 
for b in all.search("waist", limit=300): 
      posts.append([b.title, b.author])
posts = pd.DataFrame(posts,columns=['title', 'username'])

posts['gender'] = posts['title'].str[:1]
posts['age'] = posts['title'].str[2:4]
posts['height'] = posts['title'].str[5:9]

posts.to_csv('database.csv', encoding='utf-8-sig')
