# -*- coding: utf-8 -*-
import time

import bootstrap,utils,stats,cybergeo


def run():
    #run_kw_extraction()
    #bootstrap.test_bootstrap()
    #utils.mysql2sqlite('../../Data/dumps/20160126_cybergeo.sqlite3')
    #bootstrap.test_bootstrap()
    #stats.export_ref_info()
    #bootstrap.init_bootstrap('bootstrap/run_kw1000_csize10000_b20')
    bootstrap.run_bootstrap('bootstrap/run_kw1000_csize5000_b20',1000,5000,20,10)
    #cybergeo.extract_cybergeo_keywords()
    #cybergeo.extract_relevant_cybergeo(2000)

def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
