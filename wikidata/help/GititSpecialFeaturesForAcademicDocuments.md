Why a Wiki?
-------------------------------------


> Ward Cunningham called wikis an example of “the simplest thing that could possibly work.”[1] Cunningham’s Zen-like aesthetic was (and is) aimed at software developers, but educators should take notice as well. The virtue of absolute architectural simplicity pays off in the real-world dynamics of an emergent “community of inquiry.” A wiki, stripped of features, bells, whistles, and other enticements, quickly becomes a working collaborative writing environment, knowledge base, discussion forum, media repository, and the evolving documentary record of a community of peers. Unlike a blog, a wiki focuses on the group instead of the individual, while the sheer simplicity of wiki editing keeps administrative tasks mostly on the pedagogical and textual level rather than the technical. Wiki is software that gets out of the way to allow more interesting dynamics to take center stage, and it is this shift that most interests us—as it does other educators.
    
[source](http://quod.lib.umich.edu/d/dcbooks/5871848.0001.001/1:4/--wiki-writing-collaborative-learning-in-the-college-classroom?g=dculture;rgn=div1;view=fulltext;xc=1#4.4) 

    Wiki Writing: Collaborative Learning in the College Classroom
    Edited by Robert E. Cummings and Matt Barton
    Skip other details (including permanent urls, DOI, citation information)
    Series: digitalculturebooks
    DOI: http://dx.doi.org/10.3998/dcbooks.5871848.0001.001
    
Useful Features
---------------

- slide show demo: [slideshowdemo](demoslideshow/slideshowdemo)

    

Symbol Markup
-------------
H~2~O  
10^100^  
$x = \frac{{ - b \pm \sqrt {b^2 - 4ac} }}{{2a}}$  
A simple footnote.^[Or is it so simple?]	


Latex
-----

Here are some math equations.  Some inline math: $\sqrt{1-x^2}$ or $ax^2+bx+c$, 
and some displayed math:
$$\int {1\over x}\,dx = \ln(x)+C$$
and
$$\sum_{i=1}^n i = {n(n+1)\over 2}.$$

[jsMath sample](js/jsMath/test/sample.html)

[jsMath all examples](http://www.math.union.edu/~dpvc/jsMath/examples/)

[jsMath advanced extensions](http://www.math.union.edu/~dpvc/jsMath/examples/extensions.html)


Latex Examples from the TeXbook (Chapter 16)
--------------------------------------------

TeXbook p. 130

$$\textstyle
\sqrt{x+2}\quad \underline 4\quad \overline{x+y}\quad
x^{\underline n}\quad x^{\overline{m+n}}
\quad \sqrt{x^2+\sqrt{\alpha}}
$$

$$\textstyle
\root 3 \of 2 \quad
\root n \of {x^n+y^n}\quad
\root n+1 \of k
$$

TeXbook p. 131, Exercise 16.7

$$\textstyle
10^{10}\quad 2^{n+1}\quad (n+1)^2\quad \sqrt{1-x^2}\quad
\overline{w+\bar z}\quad p^{e_1}_1\quad a_{b_{c_{d_e}}}\quad
\root 3 \of {h''_n(\alpha x)}
$$

TeXbook p. 133

$$
x\times y\cdot z \quad
x\circ y\bullet z \quad
x\cup y\cap z \quad
x\sqcup y \sqcap z \quad
x\vee y\wedge z \quad
x\pm y\mp z
$$

$$
K_n^+, K_n^- \quad
z^*_{ij} \quad
g^\circ \mapsto g^\bullet \quad
f^*(x)\cap f_*(y)
$$

$$
x = y > z \quad
x:= y \quad
x \le y \ne z \quad
x\sim y\simeq z \quad
x\equiv y \not\equiv z \quad
x\subset y\subseteq z
$$

TeXbook p. 135

$$
\hat a \quad \check a\quad \tilde a \quad \acute a \quad \grave a \quad
\dot a \quad \ddot a \quad \breve a \quad \bar a \quad \vec a
$$

TeXbook p. 136

$$
\widehat x, \widetilde x \quad
\widehat{xy}, \widetilde{xy} \quad
\widehat{xyz}, \widetilde{xyz}
$$

Page metadata
-------------

Pages may optionally begin with a metadata block.  Here is an example:

    ---
    format: latex+lhs
    categories: haskell math
    toc: no
    title: Haskell and
      Category Theory
    ...

    \section{Why Category Theory?}

The metadata block consists of a list of key-value pairs, each on a
separate line. If needed, the value can be continued on one or more
additional line, which must begin with a space. (This is illustrated by
the "title" example above.) The metadata block must begin with a line
`---` and end with a line `...` optionally followed by one or more blank
lines. (The metadata block is a valid YAML document, though not all YAML
documents will be valid metadata blocks.)

Currently the following keys are supported:

format
:   Overrides the default page type as specified in the configuration file.
    Possible values are `markdown`, `rst`, `latex`, `html`, `markdown+lhs`,
    `rst+lhs`, `latex+lhs`.  (Capitalization is ignored, so you can also
    use `LaTeX`, `HTML`, etc.)  The `+lhs` variants indicate that the page
    is to be interpreted as literate Haskell.  If this field is missing,
    the default page type will be used.

categories
:   A space or comma separated list of categories to which the page belongs.

toc
:   Overrides default setting for table-of-contents in the configuration file.
    Values can be `yes`, `no`, `true`, or `false` (capitalization is ignored).

title
:   By default the displayed page title is the page name.  This metadata element
    overrides that default.

Highlighted source code
-----------------------


plain source code by using
[delimited code blocks][]:

~~~ {.haskell .numberLines}
qsort []     = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
               qsort (filter (>= x) xs) 
~~~

SOURCE CODE:

    ~~~ {.haskell .numberLines}
    qsort []     = []
    qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++
                   qsort (filter (>= x) xs) 
    ~~~

Syntax highlighting is supported for the following languages:
    actionscript, ada, apache, asn1, asp, awk, bash, bibtex, boo, c, changelog,
    clojure, cmake, coffee, coldfusion, commonlisp, cpp, cs, css, curry, d,
    diff, djangotemplate, doxygen, doxygenlua, dtd, eiffel, email, erlang,
    fortran, fsharp, gnuassembler, go, haskell, haxe, html, ini, java, javadoc,
    javascript, json, jsp, julia, latex, lex, literatecurry, literatehaskell,
    lua, makefile, mandoc, markdown, matlab, maxima, metafont, mips, modelines,
    modula2, modula3, monobasic, nasm, noweb, objectivec, objectivecpp, ocaml,
    octave, pascal, perl, php, pike, postscript, prolog, python, r,
    relaxngcompact, restructuredtext, rhtml, roff, ruby, rust, scala, scheme,
    sci, sed, sgml, sql, sqlmysql, sqlpostgresql, tcl, texinfo, verilog, vhdl,
    xml, xorg, xslt, xul, yacc, yaml

# Dot Plugin Demo

[DotDemo](DotDemo)

The following chart is generated from a textual
description using the Dot plugin.

~~~ {.dot}
digraph finite_state_machine {
	rankdir=LR;
	size="8,5"
	node [shape = doublecircle]; LR_0 LR_3 LR_4 LR_8;
	node [shape = circle];
	LR_0 -> LR_2 [ label = "SS(B)" ];
	LR_0 -> LR_1 [ label = "SS(S)" ];
	LR_1 -> LR_3 [ label = "S($end)" ];
	LR_2 -> LR_6 [ label = "SS(b)" ];
	LR_2 -> LR_5 [ label = "SS(a)" ];
	LR_2 -> LR_4 [ label = "S(A)" ];
	LR_5 -> LR_7 [ label = "S(b)" ];
	LR_5 -> LR_5 [ label = "S(a)" ];
	LR_6 -> LR_6 [ label = "S(b)" ];
	LR_6 -> LR_5 [ label = "S(a)" ];
	LR_7 -> LR_8 [ label = "S(b)" ];
	LR_7 -> LR_5 [ label = "S(a)" ];
	LR_8 -> LR_6 [ label = "S(b)" ];
	LR_8 -> LR_5a [ label = "S(a)" ];
}
~~~

SOURCE CODE

        ~~~ {.dot}
        digraph finite_state_machine {
    	    rankdir=LR;
        	size="8,5"
        	node [shape = doublecircle]; LR_0 LR_3 LR_4 LR_8;
        	node [shape = circle];
        	LR_0 -> LR_2 [ label = "SS(B)" ];
        	LR_0 -> LR_1 [ label = "SS(S)" ];
        	LR_1 -> LR_3 [ label = "S($end)" ];
        	LR_2 -> LR_6 [ label = "SS(b)" ];
        	LR_2 -> LR_5 [ label = "SS(a)" ];
        	LR_2 -> LR_4 [ label = "S(A)" ];
        	LR_5 -> LR_7 [ label = "S(b)" ];
        	LR_5 -> LR_5 [ label = "S(a)" ];
        	LR_6 -> LR_6 [ label = "S(b)" ];
        	LR_6 -> LR_5 [ label = "S(a)" ];
        	LR_7 -> LR_8 [ label = "S(b)" ];
        	LR_7 -> LR_5 [ label = "S(a)" ];
        	LR_8 -> LR_6 [ label = "S(b)" ];
        	LR_8 -> LR_5a [ label = "S(a)" ];
        }
        ~~~

# Latex Plugin Demo

~~~ { .latex }
\begin{tikzpicture}[sibling distance=48pt]
  \Tree [ .{$a \wedge b$}
          [ .{$\neg a \vee c$} 
            [ .{$\neg c \vee \neg b$}
              [ .\node(n1){$\neg c1$}; {$\neg a$} \node(n2){$c$}; ]
              [ .\node(m1){$\neg b$}; [ .{$a$} \node(m2){$b$}; ] ]
            ]
          ]
        ]
  \draw[->] (n2)..controls +(north east:1) and +(east:1)..(n1);
  \draw[->] (m2)..controls +(east:1) and +(east:1)..(m1);
\end{tikzpicture}
~~~

SOURCE CODE

    ~~~ { .latex }
    \begin{tikzpicture}[sibling distance=48pt]
      \Tree [ .{$a \wedge b$}
              [ .{$\neg a \vee c$} 
                [ .{$\neg c \vee \neg b$}
                  [ .\node(n1){$\neg c1$}; {$\neg a$} \node(n2){$c$}; ]
                  [ .\node(m1){$\neg b$}; [ .{$a$} \node(m2){$b$}; ] ]
                ]
              ]
            ]
      \draw[->] (n2)..controls +(north east:1) and +(east:1)..(n1);
      \draw[->] (m2)..controls +(east:1) and +(east:1)..(m1);
    \end{tikzpicture}
    ~~~

#ImgLatex Plugin

~~~ {.imgtex}
\nabla \times \bm{V}
=
\frac{1}{h_1 h_2 h_3}
  \begin{vmatrix}
    h_1 e_1 & h_2 e_2 & h_3 e_3 \\
    \frac{\partial}{\partial q_{1}} &
    \frac{\partial}{\partial q_{2}} &
    \frac{\partial}{\partial q_{3}} \\
    h_1 V_1 & h_2 V_2 & h_3 V_3
  \end{vmatrix}
~~~

SOURCECODE

    ~~~ {.imgtex}
    \nabla \times \bm{V}
    =
    \frac{1}{h_1 h_2 h_3}
      \begin{vmatrix}
        h_1 e_1 & h_2 e_2 & h_3 e_3 \\
        \frac{\partial}{\partial q_{1}} &
        \frac{\partial}{\partial q_{2}} &
        \frac{\partial}{\partial q_{3}} \\
        h_1 V_1 & h_2 V_2 & h_3 V_3
      \end{vmatrix}
    ~~~


# Advanced Functionality

Ditta
-----
[omega-prime](http://blog.omega-prime.co.uk/?p=70) hacked together a quick plugin for the most excellent gitit wiki. He added support for ditaa (DIagrams Through Ascii Art). Basically, in the markdown source of your Gitit wiki you can now write something like the following:


    ~~~ {.ditaa}
    +--------+   +-------+    +-------+
    |        | --+ ditaa +--> |       |
    |  Text  |   +-------+    |diagram|
    |Document|   |!magic!|    |       |
    |     {d}|   |       |    |       |
    +---+----+   +-------+    +-------+
        :                         ^
        |       Lots of work      |
        +-------------------------+
    ~~~

The plugin will then call out to the ditaa command line tool (written in Java, boo!) to render that to a beautiful image:

~~~ {.ditaa}
+--------+   +-------+    +-------+
|        | --+ ditaa +--> |       |
|  Text  |   +-------+    |diagram|
|Document|   |!magic!|    |       |
|     {d}|   |       |    |       |
+---+----+   +-------+    +-------+
    :                         ^
    |       Lots of work      |
    +-------------------------+
~~~

A ditaa image on a Gitit page

To get this set up for yourself, try the following from the root of your Gitit wiki:

    git clone git://github.com/batterseapower/gitit-plugins.git batterseapower-plugins
    wget http://downloads.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip?use_mirror=kent -O ditaa0_9.zip
    unzip ditaa0_9.zip

Now edit your Gitit configuration file so the plugins list includes the plugin:

    plugins: batterseapower-plugins/Ditaa.hs

That's it - restart Gitit and you should be ready to go!

Citations
---------

The following two links explain how this works in this wiki:

[Including citations using pandoc](http://gitit.net/Pandoc%20Citations)

[Including citations using bibtex](http://blog.wuzzeb.org/posts/2012-06-26-bibtex-and-gitit.html)

[bibtex plugin test](bibtex_test)

Online Latex Editor
-------------------

Being able to use the wiki as an online Latex Editor is one of its main purposes. I think it enables dealing with collaborative authoring in a really nice way.

The following two links demonstrate the capability to have a PDF view and an edit view with multiple users able to interact with it:

[Test bibtex]()

[Here is a real sample document](http://sedimentlab05.sas.upenn.edu/GRCversion2)

Interestingly, it turns out there are websites that will give you this service for a fee, [sharelatex](https://www.sharelatex.com/user/subscription/plans?variant=freetrial&utm_expid=51652689-2.6rewKEHXTRyvdfOhKc1iLw.1&utm_referrer=https%3A%2F%2Fwww.sharelatex.com%2F) and [writelatex](https://www.writelatex.com/plans).

In fact, AMAZING, sharelatex has recently made their code completely open source, which means we can run a version of the server ourselves.
Fantastic.

See [here](https://github.com/sharelatex/sharelatex). It automatically syncs with Dropbox and everything.  Sweeeeet.

[ShareLatex Installation Log]()
