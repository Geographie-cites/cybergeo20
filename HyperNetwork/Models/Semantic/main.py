# -*- coding: utf-8 -*-
import time

import relevant,utils,stats,cybergeo


def run():
    ## extract keywords
    #run_kw_extraction()

    ## stats
    #stats.export_ref_info()

    ## relevance estimation
    relevant.relevant_full_corpus(50000)


    #cybergeo.extract_cybergeo_keywords()
    #cybergeo.extract_relevant_cybergeo(2000)
    #cybergeo.extract_relevant_cybergeo_fulltext(20)

def main():

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
