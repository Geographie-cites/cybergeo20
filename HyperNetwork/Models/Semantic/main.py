# -*- coding: utf-8 -*-
import time

import bootstrap,utils


def run():
    #run_kw_extraction()
    #bootstrap.test_bootstrap()
    #utils.mysql2sqlite('../../Data/dumps/20160126_cybergeo.sqlite3')
    bootstrap.test_bootstrap()


def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()