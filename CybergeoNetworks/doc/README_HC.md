## User guide

### Description
Exploratory Analysis of Cybergeo Keywords

------
### Methodology

#### Vertices and nodes attributes

The vertices are described by two variables: **frequency** and **degree**. The **frequency** is the number of articles citing the keyword. The **degree** is the total degree of the nodes in the network, that is the number of edges linking thiw keyword to the others (there is no distinction between in- and out- degree as the network is undirected). Both variables are distinct but correlated.
  
The edges are described by two variables: **observed weight** and **relative residual**. For two given keywords the **observed weight** is the number of articles citing both keywords. The **relative residual** is the ratio between the **observed weight** and the **expected weigth** of the edge. For a given edge the expected weight is the probability that this edge exists considering the degree of the nodes. It is computed as the union of two dependant probabilities.

The probability of drawing a vertex *i* equals <math xmlns="http://www.w3.org/1998/Math/MathML">
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
</math> is the degree of vertex *i* (weighted degree) and *w* the half sum of weights. 

Then the probability of drawing a vertex *j* distinct from *i* equals <math xmlns="http://www.w3.org/1998/Math/MathML">
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
</math>. 

The probability of existence of an edge between *i* and *j* is:

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

The probability of existence of an edge between *j* and *i*:

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

The probability of existence of an undirected edge is the union of both probabilities:

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

Eventually the expected weight is:

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

The community detection is computed with the Louvain algorithm which finds an optimum of modularity. See Blondel *et al.* 2008.


