# -*- coding: utf-8 -*-
import time

def imports():
    # import files - beware of order
    execfile('utils.py')
    execfile('kwFunctions.py')
    #execfile('kwExtraction.py')
    execfile('bootstrap.py')


def run():
    imports()

    #run_kw_extraction()
    test_bootstrap()






def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
