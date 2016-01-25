# -*- coding: utf-8 -*-
import time

import bootstrap


def run():
    #run_kw_extraction()
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
