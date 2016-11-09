# -*- coding: utf-8 -*-
import time

import bootstrap,utils,stats,cybergeo


def run():
    #run_kw_extraction()
    #bootstrap.test_bootstrap()
    #utils.mysql2sqlite('../../Data/dumps/20160126_cybergeo.sqlite3')
    #bootstrap.test_bootstrap()
    #stats.export_ref_info()
    #bootstrap.init_bootstrap('relevant_full_'+str(20000))
    #bootstrap.run_bootstrap('test_kw1000_csize5000_b20',1000,5000,20,1)
    bootstrap.relevant_full_corpus(50000)
    #bootstrap.run_bootstrap('bootstrap/run_kw1000_csize5000_b20',1000,5000,20,10)
    #cybergeo.extract_cybergeo_keywords()
    #cybergeo.extract_relevant_cybergeo(2000)
    #cybergeo.extract_relevant_cybergeo_fulltext(20)

def main():

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
