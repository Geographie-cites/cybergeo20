# -*- coding: utf-8 -*-

# bootstrap for relevant terms extraction

import numpy

def test_bootstrap() :
    corpus = get_data('SELECT id FROM refdesc WHERE abstract_keywords IS NOT NULL;')
    bootstrap_subcorpuses(corpus,100,100,100)



def bootstrap_subcorpuses(corpus,kwLimit,subCorpusSize,bootstrapSize):
    N = len(corpus)

    print('Bootstrapping on corpus of size '+str(N))

    occurence_dicos = import_kw_dico()

    # generate bSize extractions
    #   -> random subset of 1:N of size subCorpusSize
    extractions = [numpy.random.random_integers(0,(N-1),subCorpusSize) for b in range(bootstrapSize)]

    mean_termhoods = dict() # mean termhoods progressively updated
    ref_kw_dico = dict() # ref -> kw dico : cumulated on repetitions. if a kw is relevant a few time, counted as 0 in mean.

    for eind in range(len(extractions)) :
        print("bootstrap : run "+str(eind))
	    extraction = extractions[eind]
        subcorpus = [corpus[i] for i in extraction]
        [keywords,ref_kw_local_dico] = extract_relevant_keywords(subcorpus,kwLimit,occurence_dicos)

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
    return(extract_from_termhood(mean_termhoods,ref_kw_dico,kwLimit))
