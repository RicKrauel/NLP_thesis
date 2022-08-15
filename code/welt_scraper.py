import requests
import pandas as pd
from bs4 import BeautifulSoup
from multiprocessing import Pool, Manager
import tqdm
import re
import time as t
from datetime import date, timedelta

def scrape(errors, plus_urls, url):
    trycnt = 3  # max try cnt
    while trycnt > 0:
        try:
            response = requests.get(url)
            content = BeautifulSoup(response.text, 'html.parser')

            # select articles
            search_results = content.find('div', class_='articles text')
            results = search_results.find_all('div', {"class": "article"})

            # create tmp variables
            links_all = []
            titles_all = []
            dates_all = []
            texts_all = []
            tags_all = []
            comments_all = []

            # loop through found articles
            for result in tqdm.tqdm(results):
                try:
                    # check section and kicker if they are corona relevant
                    section = result.find('h5', class_='kicker').text
                    article = result.find('a')
                    if section is None:
                        continue
                    if article is None:
                        continue
                    article_title = article.text
                    article_head = (section + ' ' + article_title).lower()
                    if any(srchstr in article_head for srchstr in ('corona', 'coronavirus', 'impf', 'covid', 'lockdown')):
                        # find link and title of article
                        link = article['href']
                        if 'plus' in link:
                            plus_urls.append(link)
                            continue
                        links_all.append(link)
                        titles_all.append(article_title)
                        article_time = result.find('div', class_='time').text
                        article_date = url[-15:-5]
                        dates_all.append(article_date + ' ' + article_time)

                        # load individual article
                        response = requests.get(link)
                        soup_link = BeautifulSoup(response.text, 'html.parser')

                        # find article paragraphs
                        article_body = soup_link.find('div',
                                                      class_='c-article-text c-content-container __margin-bottom--is-0')
                        article_paragraphs = article_body.find_all('p')

                        if article_paragraphs is None:
                            texts_all.append([])
                        else:
                            text = []
                            for para in article_paragraphs:
                                text.append(para.text)
                            texts_all.append(text)

                        # find tags
                        tags_article = []
                        article_tags_list = soup_link.find('div', class_='c-themepages')
                        if article_tags_list is None:
                            tags_all.append([])
                        else:
                            article_tags = article_tags_list.find_all('a', class_='c-themepages__link')
                            for tags in article_tags:
                                tags_article.append(tags.text)
                            tags_all.append(tags_article)

                        # find comments link
                        if not re.findall(r'article(.+?)/', link):
                            comments_link = re.findall(r'plus(.+?)/', link)
                        else:
                            comments_link = re.findall(r'article(.+?)/', link)
                        # load first comment page
                        comments_page = requests.get(
                            "https://api-co.la.welt.de/api/comments?document-id=" + comments_link[
                                0] + '&sort=NEWEST&limit=100').json()
                        # find and append top-level comments of first comment page
                        comments_article = []
                        child_counter = 0
                        if comments_page['totalCount'] > 0:
                            for comment in comments_page['comments']:
                                if child_counter > 0:
                                    child_counter -= 1
                                    continue
                                else:
                                    if comment['childCount'] > 0:
                                        child_counter += 1
                                        comments_article.append(str(comment['contents']))
                                    else:
                                        comments_article.append(str(comment['contents']))
                                last_comment = comment
                            # get timestamp of last comment
                            last_comment_timestamp = last_comment['created']
                            comments_count = 100
                            # check if there are more comment pages
                            while comments_count < comments_page['totalCount']:
                                comments_page = requests.get(
                                    "https://api-co.la.welt.de/api/comments?document-id=" + comments_link[
                                        0] + '&created-cursor=' + last_comment_timestamp + '&sort=NEWEST&limit=100').json()
                                child_counter = 0
                                for comment in comments_page['comments']:
                                    if child_counter > 0:
                                        child_counter -= 1
                                        continue
                                    else:
                                        if comment['childCount'] > 0:
                                            child_counter += 1
                                            comments_article.append(str(comment['contents']))
                                        else:
                                            comments_article.append(str(comment['contents']))
                                comments_count += 100

                            comments_all.append(comments_article)
                        else:
                            comments_all.append([])
                    else:
                        continue

                    df_titles = pd.DataFrame(titles_all, columns=['title'])
                    df_links = pd.DataFrame(links_all, columns=['link'])
                    df_dates = pd.DataFrame(dates_all, columns=['date'])
                    text_columns = []
                    for i in range(len(max(texts_all, key=len, default=0))):
                        text_columns.append('Text'+str(i))
                    df_texts = pd.DataFrame(texts_all, columns=text_columns)
                    df_texts['combined_text'] = df_texts[text_columns].fillna('').agg(' '.join, axis=1)
                    df_texts.drop(text_columns, axis=1, inplace=True)
                    tags_columns = []
                    for i in range(len(max(tags_all, key=len, default=0))):
                        tags_columns.append('Tag'+str(i))
                    df_tags = pd.DataFrame(tags_all, columns=tags_columns)
                    comment_columns = []
                    for i in range(len(max(comments_all, key=len, default=0))):
                        comment_columns.append('Comment'+str(i))
                    df_comments = pd.DataFrame(comments_all, columns=comment_columns)

                    return pd.concat([df_titles,df_links,df_dates,df_texts,df_comments], axis=1)

                except Exception as e:
                    errors.append([url, article['href'], e])
                    continue

        except Exception as e:
            if trycnt <= 0:
                errors.append([url, e])
                pass
            else:
                trycnt -= 1
                t.sleep(0.5)

def daterange(start_date, end_date):
    for n in range(int((end_date - start_date).days)):
        yield start_date + timedelta(n)

if __name__ == '__main__':
    urls = []
    start_date = date(2020, 3, 23)
    end_date = date(2020, 3, 26)
    for single_date in daterange(start_date, end_date):
        urls.append('https://www.welt.de/schlagzeilen/nachrichten-vom-'+str(single_date.strftime("%d-%m-%Y"))+'.html')
    #create shared variables to store the scraped data
    manager = Manager()
    errors = manager.list()
    plus_urls = manager.list()

    with Pool(processes=5) as pool:
        results = pool.starmap(scrape, [[errors, plus_urls, url] for url in urls])

    #create output
    errors_real = list(errors)
    plus_urls_real = list(plus_urls)
    df_errors = pd.DataFrame(errors_real)
    df_plus = pd.DataFrame(plus_urls_real)
    df_errors.to_csv('errors.csv')
    df_plus.to_csv('plus.csv')

    output_final = pd.concat(results, ignore_index=True)
    output_final.to_csv('output_combined.csv')
