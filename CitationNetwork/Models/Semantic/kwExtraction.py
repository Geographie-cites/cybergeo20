# -*- coding: utf-8 -*-
import nltk,time,locale,sys
from treetagger import TreeTagger





def run_kw_extraction() :
    #start = time.time()

    # dirty
    while True :
        # get data from mysql

        data = get_data('SELECT id,abstract FROM refdesc WHERE abstract_keywords IS NULL;')

        for ref in data:
            language = get_language(ref[1])
	        keywords = extract_keywords(ref[1],ref[0],language)
            kwtext = ""
	        for multistem in keywords:
		        kwtext=kwtext+reduce(lambda s1,s2 : s1+' '+s2,multistem)+";"
	            print(kwtext)
	        cursor.execute("INSERT INTO refdesc (id,language,abstract_keywords) VALUES (\'"+ref[0].encode('utf8')+"\',\'"+language.encode('utf8')+"\',\'"+kwtext+"\') ON DUPLICATE KEY UPDATE language = VALUES(language),abstract_keywords=VALUES(abstract_keywords);")
            conn.commit()

        conn.close()


def potential_multi_term(tagged,language):
    res = True
    for tag in tagged :
	if len(tag)>=2 :
            if language=='english':
	        res = res and (tag[1]=="NN" or tag[1]=="NNP" or tag[1] == "VBG" or tag[1] =="NNS" or tag[1] =="JJ" or tag[1] =="JJR")
            else:
                if language=='french' :
	            res = res and (tag[1]=='NOM' or tag[1]=='ADJ') and len(tag[0]) >= 3 #and tag[2]!="<unknown>"
                else :
	            res = False
	else :
	    res=False
    return res


STOPWORDS_DICT = dict()
for lang in nltk.corpus.stopwords.fileids():
    STOPWORDS_DICT[lang] =  set(nltk.corpus.stopwords.words(lang))

def get_language(text):
    words = set(nltk.wordpunct_tokenize(text.lower()))
    return max(((lang, len(words & stopwords)) for lang, stopwords in STOPWORDS_DICT.items()), key = lambda x: x[1])[0]


def extract_keywords(raw_text,id,language):

    print("Extracting keywords for "+id)

    stemmer = nltk.PorterStemmer()

    # Construct text

    # Tokens

    if language == 'english':
        tokens = nltk.word_tokenize(raw_text)
        # filter undesirable words and format
        words = [w.replace('\'','') for w in tokens if len(w)>=3]
        text = nltk.Text(words)

        tagged_text = nltk.pos_tag(text)

    else:
       tt = TreeTagger(encoding='utf-8',language='french')
       tagged_text =tt.tag(raw_text.replace('\'',' ').replace(u'\u2019',' ').replace(u'\xab',' ').replace(u'\xbb',' '))

    print(tagged_text)

    # detect language using stop words, adapt filtering/stemming technique in function

    # multi-term
    multiterms = []
    for i in range(len(tagged_text)) :
  #      # max length 4 for multi-terms
        for l in range(1,5) :
            if i+l < len(tagged_text) :
                tags = [tagged_text[k] for k in range(i,i+l)]
                #if language == 'english':
                #        print(tags)
		#	print(potential_multi_term(tags,language))
		if potential_multi_term(tags,language) :
                    multistem = []
		    if language == 'english':
			#print(tags)
			#for k in range(i,i+l):
		        #    print(tagged_text[k][0])
			#    print(stemmer.stem(tagged_text[k][0]))
			#    print(stemmer.stem(tagged_text[k][0]).encode('ascii','ignore'))

			multistem = [str.lower(stemmer.stem(tagged_text[k][0]).encode('utf8','ignore')) for k in range(i,i+l)]
                    else :#in case of french or other language, terms are already stemmed by TreeTagger
			multistem=[]
			for k in range(i,i+l):
			    if tagged_text[k][2]!="<unknown>":
			        stem = tagged_text[k][2]
			    else :
			        stem = tagged_text[k][0]
			    multistem.append(str.lower(stem.encode('utf8','ignore')))
		    #multistem.sort(key=str.lower)
                    multiterms.append(multistem)

    return multiterms



# extract relevant keywords, using unithood and termhood
#  @returns [tselected,p_tsel_dico] : dico kw -> termhood ; dico patent -> kws
def extract_relevant_keywords(corpus,kwLimit,occurence_dicos):
    print('Extracting relevant keywords...')

    [ref_kw_dico,kw_ref_dico] = extract_sub_dicos(corpus,occurence_dicos)

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

    # compute termhoods
    colSums = [sum(row) for row in coocs]

    termhoods = [0]*len(coocs)
    for i in range(len(coocs)):
        s = 0;
        for j in range(len(coocs)):
            if j != i : s = s + (coocs[i][j]-colSums[i]*colSums[j])^2/(colSums[i]*colSums[j])
        termhoods[i]=s

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
