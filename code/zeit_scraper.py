import requests
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from multiprocessing import Pool, Manager
import tqdm
import re


def scrape(errors, url):
    try:
        response = requests.get(url)
        content = BeautifulSoup(response.text, 'html.parser')

        #select articles
        search_results = content.find('div', class_ = 'cp-area cp-area--paginated')
        results = search_results.find_all('article', class_='zon-teaser-standard has-bookmark-icon')

        #create tmp variables
        links_all = []
        titles_all = []
        dates_all = []
        texts_all = []
        tags_all = []
        comments_all = []

        #loop through found articles
        for result in tqdm.tqdm(results):
            article = result.find('a', class_='zon-teaser-standard__faux-link')
            time = result.find('time', class_='zon-teaser-standard__datetime')
            #find link and title of article
            link = article['href']
            links_all.append(link)
            title = article.text
            titles_all.append(title)
            date = time['datetime']
            dates_all.append(date)
            
            #load individual article
            response = requests.get(link)
            soup_link = BeautifulSoup(response.text, 'html.parser')

            #check if article needs to be displayed on one page
            komplettansicht = soup_link.find('a', class_='article-toc__fullview')
            if komplettansicht != None:
                response = requests.get(link+'/komplettansicht')
                soup_link = BeautifulSoup(response.text, 'html.parser')

            #find text paragraphs
            article_paragraphs = soup_link.find_all('p', class_='paragraph article__item')

            text = []
            for para in article_paragraphs:
                text.append(para.text)
            texts_all.append(text)

            #find tags
            tags_article = []
            article_tags_list = soup_link.find('ul', class_='article-tags__list')
            article_tags = article_tags_list.find_all('li')
            for tags in article_tags:
                tag = tags.find('a').text
                tags_article.append(tag)
            tags_all.append(tags_article)

            #find comments of first page
            comments = soup_link.find_all('article', class_=re.compile(r'comment js-comment-toplevel'))
            comments_article = []
            for comment in comments:
                comments_article.append(comment.find('div', class_='comment__body').text)
            
            #check if there are more comment pages
            more_comments = soup_link.find('a', class_='pager__button pager__button--next')
            
            #find comments of next pages
            while more_comments != None:
                response = requests.get(more_comments['href'])
                soup_link = BeautifulSoup(response.text, 'html.parser')
                comments = soup_link.find_all('article', class_=re.compile(r'comment js-comment-toplevel'))
                for comment in comments:
                    comments_article.append(comment.find('div', class_='comment__body').text)
                more_comments = soup_link.find('a', class_='pager__button pager__button--next')

            comments_all.append(comments_article)
        
        df_titles = pd.DataFrame(titles_all, columns=['title'])
        df_links = pd.DataFrame(links_all, columns=['link'])
        df_dates = pd.DataFrame(dates_all, columns=['date'])
        text_columns = []
        for i in range(len(max(texts_all,key=len))):
            text_columns.append('Text'+str(i))
        df_texts = pd.DataFrame(texts_all, columns=text_columns)
        df_texts['combined_text'] = df_texts[text_columns].fillna('').agg(' '.join, axis=1)
        df_texts.drop(text_columns, axis=1, inplace=True)
        tags_columns = []
        for i in range(len(max(tags_all,key=len))):
            tags_columns.append('Tag'+str(i))
        df_tags = pd.DataFrame(tags_all, columns=tags_columns)
        comment_columns = []
        for i in range(len(max(comments_all,key=len))):
            comment_columns.append('Comment'+str(i))
        df_comments = pd.DataFrame(comments_all, columns=comment_columns)

        return pd.concat([df_titles,df_links,df_dates,df_texts,df_tags,df_comments],axis=1)

    except:
        errors.append(url)
        pass

if __name__ == '__main__':
    #start webdriver
    driver = webdriver.Chrome()
    driver.get('https://www.zeit.de/thema/coronavirus?p=18')
    driver.implicitly_wait(5)
    wait = WebDriverWait(driver, 20)

    #login
    driver.find_element(by=By.XPATH, value='//*[contains(concat( " ", @class, " " ), concat( " ", "js-forward-link", " " )) and contains(concat( " ", @class, " " ), concat( " ", "js-login", " " ))]').click()
    driver.implicitly_wait(5)
    email = driver.find_element(by=By.ID, value='login_email')
    pw = driver.find_element(by=By.ID, value='login_pass')
    login_button = driver.find_element(by=By.XPATH, value='//*[contains(concat( " ", @class, " " ), concat( " ", "log", " " ))]')

    email.send_keys('richy.krauely@gmail.com')
    pw.send_keys('crawler123')
    login_button.click()
    driver.implicitly_wait(5)

    #click consent button
    wait.until(EC.frame_to_be_available_and_switch_to_it((By.ID,"sp_message_iframe_574935")))
    wait.until(EC.element_to_be_clickable((By.XPATH, '//*[@title="EINVERSTANDEN"]')))
    consent = driver.find_element(by=By.XPATH, value='//*[@title="EINVERSTANDEN"]')
    consent.click()

    driver.switch_to.default_content()
    wait.until(EC.presence_of_element_located((By.XPATH, '//*[contains(concat( " ", @class, " " ), concat( " ", "header-image__media-container", " " ))]')))

    #look at webpage content
    content = BeautifulSoup(driver.page_source,'lxml')

    #find number of result pages
    pages = content.find_all('li', class_='pager__page')
    last_page = None
    for last_page in pages:
        pass
    last_page = last_page.text

    urls = []
    for i in range(28,int(last_page)):
        urls.append('https://www.zeit.de/thema/coronavirus?p='+str(i))
    #int(last_page)
    #urls = ['https://www.zeit.de/thema/coronavirus?p=18', 'https://www.zeit.de/thema/coronavirus?p=19']

    #create shared variables to store the scraped data
    manager = Manager()
    #ns = manager.Namespace()
    #df_output_combined = pd.DataFrame()
    #df = multiprocessing.list([df_output_combined])
    errors = manager.list()

    with Pool(processes=30) as pool:
        results = pool.starmap(scrape, [[errors, url] for url in urls])

    #create output
    errors_real = list(errors)
    df_errors = pd.DataFrame(errors_real)
    df_errors.to_csv('errors.csv')

    output_final = pd.concat(results, ignore_index=True)
    output_final.to_csv('output_combined.csv')
    
    driver.quit()