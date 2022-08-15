import requests
import pandas as pd
from bs4 import BeautifulSoup
from multiprocessing import Pool, Manager
import tqdm
import time as t

def scrape(errors, req_session, url):
    trycnt = 3  # max try cnt
    while trycnt > 0:
        try:
            response = req_session.get(url)
            content = BeautifulSoup(response.text, 'html.parser')

            #select articles
            search_results = content.find('ul', class_='hcf-teaser-list')
            results = search_results.find_all('li', {"class": "hcf-teaser"})

            #create tmp variables
            links_all = []
            titles_all = []
            dates_all = []
            texts_all = []
            tags_all = []
            comments_all = []

            #loop through found articles
            for result in tqdm.tqdm(results):
                article = result.find('a', {"target": "_self"})
                if article is None:
                    continue
                #find link and title of article
                link = article['href']
                links_all.append(link)
                titles_all.append(article['title'])
                time = result.find('span', class_='hcf-date hcf-separate')
                dates_all.append(time.text)

                #load individual article
                if link.startswith('https'):
                    #response = req_session.get(link)
                    #soup_link = BeautifulSoup(response.text, 'html.parser')
                    # find article paragraphs
                    #article_body = soup_link.find('div', class_='A')
                    #article_paragraphs = article_body.find_all('p')
                    texts_all.append([])
                    comments_all.append([])
                    continue
                else:
                    response = req_session.get('https://www.tagesspiegel.de/'+link)
                    soup_link = BeautifulSoup(response.text, 'html.parser')

                # find article paragraphs
                article_body = soup_link.find('div', class_='ts-article-body')
                article_paragraphs = article_body.find_all('p')

                text = []
                for para in article_paragraphs:
                    text.append(para.text)
                texts_all.append(text)

                #find tags
                #tags_article = []
                #article_tags_list = soup_link.find('ul', class_='article-tags__list')
                #article_tags = article_tags_list.find_all('li')
                #for tags in article_tags:
                #    tag = tags.find('a').text
                #    tags_article.append(tag)
                #tags_all.append(tags_article)

                #find comments link
                comments_link = soup_link.find('section', id='kommentare')
                #load first comment page
                comments_page = requests.get(
                    "https://www.tagesspiegel.de"+comments_link['data-param']+'&preview=false&pageNumber=1&view=singlePageChronologicalLatestAtFirst') \
                    .json()
                comment_content = BeautifulSoup(comments_page['html'], 'html.parser')
                comments = comment_content.find_all('article', class_='ts-comment ts-is-accepted')
                comments_article = []
                for comment in comments:
                    comments_article.append(comment.find('div', class_='ts-comment-body').text)

                #check if there are more comment pages
                more_pages= comments_page['data']['totalPages']

                #find comments of next pages
                if more_pages > 1:
                    for j in range(1,more_pages):
                        comments_page = requests.get(
                            "https://www.tagesspiegel.de"+comments_link['data-param']+'&preview=false&pageNumber='+str(j+1)+'&view=singlePageChronologicalLatestAtFirst') \
                            .json()
                        comment_content = BeautifulSoup(comments_page['html'], 'html.parser')
                        comments = comment_content.find_all('article', class_='ts-comment ts-is-accepted')
                        for comment in comments:
                            comments_article.append(comment.find('div', class_='ts-comment-body').text)

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
            #tags_columns = []
            #for i in range(len(max(tags_all,key=len))):
            #    tags_columns.append('Tag'+str(i))
            #df_tags = pd.DataFrame(tags_all, columns=tags_columns)
            comment_columns = []
            for i in range(len(max(comments_all,key=len))):
                comment_columns.append('Comment'+str(i))
            df_comments = pd.DataFrame(comments_all, columns=comment_columns)

            return pd.concat([df_titles,df_links,df_dates,df_texts,df_comments],axis=1)

        except Exception as e:
            if trycnt <= 0:
                errors.append([url, e])
                pass
            else:
                trycnt -= 1
                t.sleep(0.5)

if __name__ == '__main__':
    # # start webdriver
    # driver = webdriver.Chrome()
    # driver.get('https://plus.tagesspiegel.de/wissen/wie-in-berlin-der-erste-pcr-test-zum-corona-nachweis-entstand-fahrtenleser-des-virus-346744.html')
    # driver.implicitly_wait(5)
    # wait = WebDriverWait(driver, 20)
    #
    # # click consent button
    # wait.until(EC.frame_to_be_available_and_switch_to_it((By.ID, "sp_message_iframe_606206")))
    # wait.until(EC.element_to_be_clickable((By.XPATH, '/html/body/div/div[2]/div[2]/div/div[3]/button[2]')))
    # consent = driver.find_element(By.XPATH, '/html/body/div/div[2]/div[2]/div/div[3]/button[2]')
    # consent.click()
    #
    # driver.switch_to.default_content()
    #
    # # login
    # driver.find_element(By.XPATH, '/html/body/div[1]/div/div[2]/div/header/div/div/div[2]/div/div/a').click()
    # driver.implicitly_wait(5)
    # #wait.until(EC.frame_to_be_available_and_switch_to_it((By.ID, "modal-iframe")))
    # email = driver.find_element(By.ID, 'forms/formLogin_email')
    # pw = driver.find_element(By.ID, 'forms/formLogin_password')
    # login_button = driver.find_element(By.XPATH, '/html/body/div[1]/main/section/div/div/div[3]/div[1]/div/form/div[3]/div[1]/div/button')
    # email.send_keys('richy.krauely@gmail.com')
    # pw.send_keys('wevdoz-bodge1-pejsuP')
    # login_button.click()
    # driver.implicitly_wait(5)
    # #driver.switch_to.default_content()
    # wait.until(EC.presence_of_element_located((By.XPATH, '/html/body/div[1]/main/section[1]/h1')))
    #
    # # get login cookies from selenium session and set for request session
    # cookies = driver.get_cookies()
    #
    req_session = requests.Session()
    # for cookie in cookies:
    #     req_session.cookies.set(cookie['name'], cookie['value'])

    start_url = 'https://www.tagesspiegel.de/suchergebnis/artikel/?p9049616=1&sw=Coronavirus&search-fromday=1&search-frommonth=1&search-fromyear=2020&search-today=31&search-tomonth=12&search-toyear=2021'
    response = requests.get(start_url)
    content = BeautifulSoup(response.text, 'html.parser')
    #find number of result pages
    final_page = content.find('a', class_='hcf-page-link hcf-fast-forward hcf-like-button')
    final_page = int(''.join(filter(str.isdigit, final_page['title'])))

    urls = []
    for i in range(638):
        urls.append('https://www.tagesspiegel.de/suchergebnis/artikel/?p9049616='+str(i+1)+'&sw=Coronavirus&search-fromday=1&search-frommonth=1&search-fromyear=2020&search-today=31&search-tomonth=12&search-toyear=2021')
    #create shared variables to store the scraped data
    manager = Manager()
    errors = manager.list()

    with Pool(processes=20) as pool:
        results = pool.starmap(scrape, [[errors, req_session, url] for url in urls])

    #create output
    errors_real = list(errors)
    df_errors = pd.DataFrame(errors_real)
    df_errors.to_csv('errors.csv')

    output_final = pd.concat(results, ignore_index=True)
    output_final.to_csv('output_combined.csv')

    #driver.quit()