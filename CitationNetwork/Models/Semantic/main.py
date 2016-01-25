# -*- coding: utf-8 -*-


def run():
    #run_kw_extraction()
    test_bootstrap()






def main():

    # import utils
    # execfile('../Utils/utils.py')

    #sys.setdefaultencoding('utf8')
    # deprecated in python 2.6

    # import files - beware of order
    execfile('utils.py')
    execfile('kwFunctions.py')
    #execfile('kwExtraction.py')
    execfile('bootstrap.py')

    start = time.time()

    run()

    print('Ellapsed Time : '+str(time.time() - start))


main()
