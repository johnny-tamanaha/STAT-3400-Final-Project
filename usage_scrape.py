'''Scrape advanced statstics from basketball-reference.com'''
import numpy as np 
import pandas as pd 
from urllib.request import urlopen 
from bs4 import BeautifulSoup, Comment 

def scrape_astats(years):
    '''Scrape advanced stats for all players in given years.
Arguments:
    1. years (list-like object) -> years to be scraped
Scraped Data:
    1. astats.csv -> concatenated advanced statistics tables
        Tm -> team
        G -> games played
        MP -> minutes played
        PER -> player efficiency rating
        USG% -> usage percentage
        Season -> year'''
    astats = pd.DataFrame(columns=['Tm',
                                   'G',
                                   'MP',
                                   'PER',
                                   'USG%',
                                   'Season'])
    for year in years:
        url = r'https://www.basketball-reference.com/leagues/NBA_{}_advanced.html'.format(str(year))
        try: 
            html = urlopen(url)
        except:
            print('Year ->', year, '| Load -> Error (404 Not Found)')
            continue
        soup = BeautifulSoup(html, features='html5lib')
        aTable = soup.find('table', {'id':'advanced_stats'})
        try:
            df = pd.read_html(str(aTable))[0]
        except:
            print('Year ->', year, '| Load -> Error (Advanced Missing)')
            continue
        df = df[['Tm', 'G', 'MP', 'PER', 'USG%']]
        df['Season'] = str(year)
        astats = pd.concat([astats, df])
        print('Year ->', year, '| Load -> Success')
    astats.to_csv('astats.csv', index=False)

def scrape_tstats(years):
    '''Scrape all team records in given years
Arguments:
    1. years (list-like object) -> years to be scraped
Scraped Data:
    1. tstats.csv -> concatenated team statistics tables
        Team -> team
        Overall -> record
        Season -> year'''
    tstats = pd.DataFrame(columns=['Team', 'Overall', 'Season'])
    for year in years:
        url = r'https://www.basketball-reference.com/leagues/NBA_{}_standings.html'.format(str(year))
        try:
            html = urlopen(url)
        except:
            print('Year ->', year, '| Load -> Error (404 Not Found)')
            continue
        soup = BeautifulSoup(html, features='html5lib')
        comments = soup.find_all(string=lambda text: isinstance(text, Comment))
        tables = '' # expanded_standings table is embedded in comments
        for c in comments:
            if '<table' in c:
                tables += c
        soup = BeautifulSoup(tables, 'html5lib')
        tTable = soup.find('table', {'id':'expanded_standings'})
        try:
            df = pd.read_html(str(tTable))[0]
        except:
            print('Year ->', year, '| Load -> Error (Advanced Missing)')
            continue
        df.columns = df.columns.droplevel()
        df = df[['Team', 'Overall']]
        df['Season'] = str(year)
        tstats = pd.concat([tstats, df])
        print('Year ->', year, '| Load -> Success')
    tstats.to_csv('tstats.csv', index=False)

if __name__ == '__main__':
    years = range(2000, 2020)
    scrape_astats(years)
    scrape_tstats(years)