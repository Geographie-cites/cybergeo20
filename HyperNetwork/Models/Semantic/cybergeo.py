import utils,kwExtraction
# kw extraction for cybergeo corpus alone



def extract_cybergeo_keywords():
    kwExtraction.run_kw_extraction('SELECT refdesc.id,abstract FROM refdesc INNER JOIN cybergeo ON cybergeo.id=refdesc.id WHERE abstract_keywords IS NULL;')


def extract_relevant_cybergeo (kwLimit) :
    corpus = utils.get_data('SELECT refdesc.id FROM refdesc INNER JOIN cybergeo ON cybergeo.id=refdesc.id WHERE abstract_keywords IS NOT NULL;','../../Data/dumps/20160126_cybergeo.sqlite3')
    occurence_dicos = utils.import_kw_dico('../../Data/dumps/20160125_cybergeo.sqlite3')
    [relevantkw,relevant_dico] = kwFunctions.extract_relevant_keywords(corpus,kwLimit,occurence_dicos)
    utils.export_dico_csv(relevant_dico,'res/cybergeo/relevantDico_kwLimit'+str(kwLimit),False)
    utils.export_list(relevantkw,'res/cybergeo/kw_'+str(kwLimit),False)
