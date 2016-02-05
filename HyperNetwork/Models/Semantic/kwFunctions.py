# -*- coding: utf-8 -*-
import nltk,math,operator
import utils

# kw extraction functions common to kw Extraction and Bootstrap
#  -> for separation TreeTagger/other (necessites server installation)

# extract relevant keywords, using unithood and termhood
#  @returns [tselected,p_tsel_dico] : dico kw -> termhood ; dico patent -> kws
def extract_relevant_keywords(corpus,kwLimit,occurence_dicos):
    print('Extracting relevant keywords...')

    [ref_kw_dico,kw_ref_dico] = utils.extract_sub_dicos(corpus,occurence_dicos)

    #print(kw_ref_dico)

    # compute unithoods
    print('Compute unithoods...')
    unithoods = dict()
    for k in kw_ref_dico.keys():
        l = len(k.split(' '))
        unithoods[k]=math.log(l+1)*len(kw_ref_dico[k])

    # sort and keep K*N keywords ; K = 4 for now ?
    selected_kws = dict() # dictionary : kw -> index in matrix
    sorted_unithoods = sorted(unithoods.items(), key=operator.itemgetter(1),reverse=True)
    for i in range(4*kwLimit):
        selected_kws[sorted_unithoods[i][0]] = i

    # computing cooccurrences
    print('Computing cooccurrences...')
    # compute termhoods :: coocurrence matrix -> in \Theta(16 N^2) - N must thus stay 'small'
    coocs = []
    for i in range(len(selected_kws.keys())):
        coocs.append(([0]*len(selected_kws.keys())))
    # fill the cooc matrix
    # for each patent : kws are coocurring if selected.
    # Beware to filter BEFORE launching O(n^2) procedure
    #
    #  Quick implementation using dict ? -> ¡ already optimized !

    for ref in ref_kw_dico.keys() :
        sel = []
        for k in ref_kw_dico[ref] :
            if k in selected_kws : sel.append(k)
        for i in range(len(sel)-1):
            for j in range(i+1,len(sel)):
                ii = selected_kws[sel[i]] ; jj= selected_kws[sel[j]] ;
                coocs[ii][jj] = coocs[ii][jj] + 1
                coocs[jj][ii] = coocs[jj][ii] + 1

    #print('coocs : ')
    #print(coocs)

    # compute termhoods
    colSums = [sum(row) for row in coocs]

    #print('colsums : ')
    #print(colSums)

    termhoods = [0]*len(coocs)
    for i in range(len(coocs)):
        s = 0;
        for j in range(len(coocs)):
            if j != i : s = s + ((coocs[i][j]-(colSums[i]*colSums[j]))*(coocs[i][j]-(colSums[i]*colSums[j])))/(colSums[i]*colSums[j])
        termhoods[i]=s

    #print(termhoods)
    # sort and filter on termhoods
    sorting_termhoods = dict()
    for k in selected_kws.keys():
        sorting_termhoods[k]=termhoods[selected_kws[k]]

    return(extract_from_termhood(sorting_termhoods,ref_kw_dico,kwLimit))


def extract_from_termhood(termhoods,ref_kw_dico,kwLimit):
    sorted_termhoods = sorted(termhoods.items(), key=operator.itemgetter(1),reverse=True)

    tselected = dict()
    for i in range(kwLimit):
        tselected[sorted_termhoods[i][0]] = sorted_termhoods[i][1]

    # reconstruct the ref -> tselected dico, finally necessary to build kw nw
    ref_tsel_dico = dict()
    for ref in ref_kw_dico.keys() :
        sel = []
        for k in ref_kw_dico[ref] :
            if k in tselected and k not in sel : sel.append(k)
        ref_tsel_dico[ref] = sel

    return([tselected,ref_tsel_dico])
