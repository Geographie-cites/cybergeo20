## User guide

### Description
Outil interactif d'analyse des mots-clefs du *Répertoire des géographes*.

### License

GNU GPLv3

### Citation

Les données sont issues de ...

Bla bla bla

------
### Méthodologie

#### Attributs des noeuds et des liens

Les noeuds du réseau sont dotés de deux attributs: le nombre de géographes qui citent le mot et le degré du mot, c'est-à-dire l'ensemble des liens reliant ce mot aux autres mots (pas de distinction entre degré entrant/sortant puisque le graphe est non dirigé). 

Les liens du réseau sont dotés de deux attributs. Le premier est le poids observé: pour deux mots A et B, le poids observé est le nombre de géographes qui co-citent ces deux mots. Le second est un attribut de résidu relatif qui est calculé comme le rapport entre le poids observé du lien et le poids espéré ou théorique. Ce poids espéré est conçu comme la probabilité d'occurrence de deux tirages successifs d'un noeud d'origine puis d'un noeud de destination. La probabilité de tirer un noeud d'origine puis un noeud de destination est l'intersection de deux probabilités dépendantes. La probabilité de tirer un noeud *i* d'origine est égale à <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>i</mi>
    </msub>
    <mi>w</mi>
  </mfrac>
</math> où <math xmlns="http://www.w3.org/1998/Math/MathML">
  <msub>
    <mi>w</mi>
    <mi>i</mi>
  </msub>
</math> est le poids du noeud *i* (degré pondéré) et *w* la somme des poids (sur la moitié de la matrice de poids). Puis la probabilité de tirer un noeud *j* de destination est égale à <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>j</mi>
    </msub>
    <mrow>
      <mi>w</mi>
      <mo>−</mo>
      <msub>
        <mi>w</mi>
        <mi>i</mi>
      </msub>
    </mrow>
  </mfrac>
</math> car les deux évènements ne sont pas indépendants. 

La probabilité d'existence d'un lien de *i* vers *j* s'écrit donc :

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <msub>
    <mi>P</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mo>&#x2212;<!-- − --></mo>
      <mo>&gt;</mo>
      <mi>j</mi>
    </mrow>
  </msub>
  <mo>=</mo>
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>i</mi>
    </msub>
    <mi>w</mi>
  </mfrac>
  <mo>&#x00D7;<!-- × --></mo>
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>j</mi>
    </msub>
    <mrow>
      <mi>w</mi>
      <mo>&#x2212;<!-- − --></mo>
      <msub>
        <mi>w</mi>
        <mi>i</mi>
      </msub>
    </mrow>
  </mfrac>
</math>

La probabilité d'existence d'un lien de *j* vers *i*, qui n'est pas forcément égale à celle de *i* vers *j* s'écrit :

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <msub>
    <mi>P</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>j</mi>
      <mo>&#x2212;<!-- − --></mo>
      <mo>&gt;</mo>
      <mi>i</mi>
    </mrow>
  </msub>
  <mo>=</mo>
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>j</mi>
    </msub>
    <mi>w</mi>
  </mfrac>
  <mo>&#x00D7;<!-- × --></mo>
  <mfrac>
    <msub>
      <mi>w</mi>
      <mi>i</mi>
    </msub>
    <mrow>
      <mi>w</mi>
      <mo>&#x2212;<!-- − --></mo>
      <msub>
        <mi>w</mi>
        <mi>j</mi>
      </msub>
    </mrow>
  </mfrac>
</math>

La probabilité d'existence d'un lien entre les deux noeuds, tous sens confondus (i.e. non dirigé) est l'union des deux :

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <msub>
    <mi>P</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mo>&lt;</mo>
      <mo>&#x2212;<!-- − --></mo>
      <mo>&gt;</mo>
      <mi>j</mi>
    </mrow>
  </msub>
  <mo>=</mo>
  <msub>
    <mi>P</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mo>&#x2212;<!-- − --></mo>
      <mo>&gt;</mo>
      <mi>j</mi>
    </mrow>
  </msub>
  <mo>+</mo>
  <msub>
    <mi>P</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>j</mi>
      <mo>&#x2212;<!-- − --></mo>
      <mo>&gt;</mo>
      <mi>i</mi>
    </mrow>
  </msub>
</math>

Finalement, le poids espéré est :

<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <msup>
    <mi>w</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>e</mi>
    </mrow>
  </msup>
  <mo>=</mo>
  <mi>w</mi>
  <mo stretchy="false">(</mo>
  <mfrac>
    <msub>
      <mi>P</mi>
      <mrow class="MJX-TeXAtom-ORD">
        <mi>i</mi>
        <mo>&lt;</mo>
        <mo>&#x2212;<!-- − --></mo>
        <mo>&gt;</mo>
        <mi>j</mi>
      </mrow>
    </msub>
    <mn>2</mn>
  </mfrac>
  <mo stretchy="false">)</mo>
</math>

#### Algorithme de détection de communautés

La plupart des algorithmes utilisés en théorie des graphes s'appuient sur une mesure de modularité. Pour un graphe dans lequel on distingue plusieurs communautés ou clusters, la modularité est forte quand les liens intracommunauté sont forts et les liens intercommunauté sont faibles. Cette mesure (Q) est définie comme la différence entre la proportion observée de liens intracommunauté et la proportion que l'on observerait dans un graphe aléatoire conservant la distribution de degrés du graphe originel.


<math xmlns="http://www.w3.org/1998/Math/MathML" display="block">
  <mi>Q</mi>
  <mo>=</mo>
  <mfrac>
    <mn>1</mn>
    <mrow>
      <mn>2</mn>
      <mi>m</mi>
    </mrow>
  </mfrac>
  <munder>
    <mo>&#x2211;<!-- ∑ --></mo>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mo>,</mo>
      <mi>j</mi>
    </mrow>
  </munder>
  <mrow class="MJX-TeXAtom-ORD">
    <mo maxsize="1.623em" minsize="1.623em">[</mo>
  </mrow>
  <msub>
    <mi>A</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mi>j</mi>
    </mrow>
  </msub>
  <mo>&#x2212;<!-- − --></mo>
  <mfrac>
    <mrow>
      <msub>
        <mi>w</mi>
        <mi>i</mi>
      </msub>
      <msub>
        <mi>w</mi>
        <mi>j</mi>
      </msub>
    </mrow>
    <mrow>
      <mn>2</mn>
      <mi>m</mi>
    </mrow>
  </mfrac>
  <mrow class="MJX-TeXAtom-ORD">
    <mo maxsize="1.623em" minsize="1.623em">]</mo>
  </mrow>
  <mi>&#x03B4;<!-- δ --></mi>
  <mo stretchy="false">(</mo>
  <msub>
    <mi>c</mi>
    <mi>i</mi>
  </msub>
  <mo>,</mo>
  <msub>
    <mi>c</mi>
    <mi>j</mi>
  </msub>
  <mo stretchy="false">)</mo>
</math>

Où *m* est la somme de la matrice de poids, où <math xmlns="http://www.w3.org/1998/Math/MathML">
  <msub>
    <mi>A</mi>
    <mrow class="MJX-TeXAtom-ORD">
      <mi>i</mi>
      <mi>j</mi>
    </mrow>
  </msub>
</math> est le poids du lien entre *i* et *j* et où la fonction <math xmlns="http://www.w3.org/1998/Math/MathML">
  <mi>&#x03B4;</mi>
</math> est une fonction qui renvoie 1 si *i* et *j* font partie de la même communauté et 0 sinon.

La modularité peut être utilisée comme mesure de la qualité d'une partition réalisée *a priori* mais aussi comme une fonction à maximiser par un algorithme pour produire une partition cohérente. C'est sur ce principe que fonctionne l'algorithme utilisé ici, développé par V. Blondel *et al.* (2008), dit "méthode de Louvain". Il produit une partition du graphe qui correspond à un optimum de modularité.

------

#### Onglet bla bla

Bla bla
