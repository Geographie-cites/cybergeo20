# -*- coding: utf-8 -*-
import time

import bootstrap,utils,stats,cybergeo


def run():
    #run_kw_extraction()
    #bootstrap.test_bootstrap()
    #utils.mysql2sqlite('../../Data/dumps/20160126_cybergeo.sqlite3')
    #bootstrap.test_bootstrap()
    #stats.export_ref_info()
    #bootstrap.init_bootstrap('bootstrap/test')
    #bootstrap.run_bootstrap('bootstrap/test',20,200,20)
    cybergeo.extract_relevant_cybergeo(2000)

def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
