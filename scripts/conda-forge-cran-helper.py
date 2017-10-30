#!/usr/bin/env conda-execute

'''
Update CRAN R package on conda-forge.
'''

# conda execute
# env:
#  - beautifulsoup4
#  - python >=3
#  - requests
# channels:
#  - conda-forge
# run_with: python

'''
Usage:
python conda-forge-cran-helper.py [-h]
[--dry-run]
[--target-feedstocks [TARGET_FEEDSTOCKS [TARGET_FEEDSTOCKS ...]]]

or

conda execute conda-forge-cran-helper.py [-h]
[--dry-run]
[--target-feedstocks [TARGET_FEEDSTOCKS [TARGET_FEEDSTOCKS ...]]]
'''

import argparse
import bs4
import re
import requests
import sys

def get_feedstocks():
    '''
    Return list of conda-forge feedstocks.
    '''
    url = 'https://conda-forge.org/feedstocks/'
    r = requests.get(url)
    assert r.ok, 'Able to download list of feedstocks from conda-forge/conda-forge.github.io'
    bs = bs4.BeautifulSoup(r.text, 'html5lib')
    feedstocks = [x.text.strip() for x in bs.find_all('a') \
                  if 'https://github.com/conda-forge/' in x.get('href')]
    return feedstocks

def get_r_feedstocks():
    '''
    Return list of conda-forge feedstocks of R packages.
    '''
    feedstocks = get_feedstocks()
    r_feedstocks = [x for x in feedstocks if re.match('^r-', x)]
    return r_feedstocks
    
def cran_helper(dry_run=False,
                target_feedstocks=None):
    '''
    Update CRAN R packages on conda-forge.
    '''
    feedstocks = get_r_feedstocks()
#    import ipdb; ipdb.set_trace()
    sys.stdout.write('There are {} R packages available from conda-forge.\n'.format(len(feedstocks)))

def main():
    '''
    Parse command-line arguments and run cran_helper()
    '''
    parser = argparse.ArgumentParser()
    parser.add_argument('--dry-run',
                        action = 'store_true',
                        dest = 'dry_run',
                        help = 'Do not make any changes on GitHub')
    parser.add_argument('--target-feedstocks',
                        default = None,
                        dest = 'target_feedstocks',
                        nargs = '*',
                        help = 'List of feedstocks to update')
    args = parser.parse_args()

    cran_helper(args.dry_run,
                args.target_feedstocks)

if __name__ == '__main__':
    main()
