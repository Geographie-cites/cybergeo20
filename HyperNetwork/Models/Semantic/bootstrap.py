# -*- coding: utf-8 -*-

# bootstrap for relevant terms extraction

import numpy,os,pymongo,math
import kwFunctions,utils,butils



def relevant_full_corpus(kwLimit):
    #corpus = utils.get_data('SELECT id FROM refdesc WHERE abstract_keywords IS NOT NULL;','../../Data/dumps/20160224_cybergeo.sqlite3')
    corpus = utils.get_ids('cybergeo','keywords')
    occurence_dicos = utils.import_kw_dico('cybergeo','keywords')
    mongo = pymongo.MongoClient('localhost',27017)
    database = mongo['relevant']
    relevant = 'relevant_full_'+str(kwLimit)
    network = 'network_full_'+str(kwLimit)+'_eth10'
    database[relevant].delete_many({"cumtermhood":{"$gt":0}})
    database[relevant].create_index('keyword')
    [keywords,dico,frequencies,edge_list] = kwFunctions.extract_relevant_keywords(corpus,kwLimit,occurence_dicos)
    print('insert relevant...')
    for kw in keywords.keys():
        butils.update_kw_tm(kw,keywords[kw],frequencies[kw],math.log(keywords[kw])*math.log(len(corpus)/frequencies[kw]),database,relevant)
    print('insert edges...')
    database[network].delete_many({"weight":{"$gt":0}})
    database[network].insert_many(edge_list)





##
#   assumed to be run in //
#     - run by packet for intermediate filtering -
def run_bootstrap(res_folder,kwLimit,subCorpusSize,bootstrapSize,nruns) :
    corpus = utils.get_data('SELECT id FROM refdesc WHERE abstract_keywords IS NOT NULL;','../../Data/dumps/20160224_cybergeo.sqlite3')
    occurence_dicos = utils.import_kw_dico('../../Data/dumps/20160224_cybergeo.sqlite3')
    mongo = pymongo.MongoClient()
    #database = res_folder+'/bootstrap.sqlite3'
    database = mongo[res_folder] # mongodb database
    #while True :
    for i in range(nruns):
        print("run "+str(i))
	[relevantkw,relevant_dico,allkw] = bootstrap_subcorpuses(corpus,occurence_dicos,kwLimit,subCorpusSize,bootstrapSize)
        # update bases iteratively (ok for concurrency ?)
        n=len(relevantkw)/100;k=0
	for kw in relevantkw.keys():
            if k % n == 0 : print('kwinsertion : '+str(k/n)+'%')
	    butils.update_kw_tm(kw,relevantkw[kw],database)
	    k = k + 1
	# rq : we do not need the dico -> - use full dico after -
	#n=len(relevant_dico)/100;k=0
        #for i in relevant_dico.keys():
	#    if k % n == 0 : print('dicoinsertion : '+str(k/n)+'%')
        #    butils.update_kw_dico(i,relevant_dico[i],database)
	#    k = k + 1
	butils.update_count(bootstrapSize,database)


def bootstrap_subcorpuses(corpus,occurence_dicos,kwLimit,subCorpusSize,bootstrapSize):
    N = len(corpus)

    print('Bootstrapping on corpus of size '+str(N))

    # generate bSize extractions
    #   -> random subset of 1:N of size subCorpusSize
    extractions = [map(lambda x : x - 1,numpy.random.choice(N,subCorpusSize,replace=False)) for b in range(bootstrapSize)]

    # numpy.random.choice(N, size, replace=False)

    mean_termhoods = dict() # mean termhoods progressively updated
    ref_kw_dico = dict() # ref -> kw dico : cumulated on repetitions. if a kw is relevant a few time, counted as 0 in mean.

    allkw = []

    for eind in range(len(extractions)) :
        print("bootstrap : run "+str(eind))
        extraction = extractions[eind]
        subcorpus = [corpus[i] for i in extraction]
        [keywords,ref_kw_local_dico] = kwFunctions.extract_relevant_keywords(subcorpus,kwLimit,occurence_dicos)

	allkw.append(keywords)

        # add termhoods
        for kw in keywords.keys() :
            if kw not in mean_termhoods : mean_termhoods[kw] = 0
            mean_termhoods[kw] = mean_termhoods[kw] + keywords[kw]

        # update p->kw dico
        for ref in ref_kw_local_dico.keys() :
            if ref not in ref_kw_dico : ref_kw_dico[ref] = set()
            for kw in ref_kw_local_dico[ref] :
		       ref_kw_dico[ref].add(kw)

    # sort on termhoods (no need to normalize) adn returns
    res = kwFunctions.extract_from_termhood(mean_termhoods,ref_kw_dico,kwLimit)
    #print(res)
    #print(allkw)
    res.append(allkw)
    return(res)
