Converting a HOAS term GADT into a de Bruijn term GADT
======================================================

Two approaches to representing variables in a typed term language implemented as a [GADT](http://en.wikipedia.org/wiki/GADT) are (1) to use higher-order abstract syntax (HOAS) or to use de Bruijn indices.. Both approaches are convenient for manipulating terms as they require no explicit alpha conversion and avoid name capture during substitution. Nevertheless, there are trade offs between the two. In particular, the HOAS representation doesn't support comparing variable names and the de Bruijn representation is inconvenient for humans to read and write. For a more detailed discussion, see for example Conor McBride and James McKinna's [I am not a number: I am a free variable](http://www.strictlypositive.org/notanum.ps.gz), where they discuss a mixed representation using de Bruijn indices for bound variables and variables of the meta-language for free variables.

The tension between the HOAS and de Bruijn representation also has relevance for the design and implementation of embedded domain specific languages (EDSLs) — aka internal languages. When an internal includes higher-order functions, it is usually most convenient for the user to use the function abstraction mechanisms of the host language. However, to execute or compile the internal language, de Bruijn notation is often more convenient; at least, if optimisations are performed.

An obvious way to relief the tension between the representations is to use HOAS in the surface representation of the internal language and to convert that to the de Bruijn representation before optimisation and execution. However, the conversion is not entirely straight forward for a strongly typed internal language with typed intermediate representations and type-preserving transformations between those representations. The difficulties were already mentioned by Louis-Julien Guillemette and Stefan Monnier in their Haskell'07 paper [A Type-Preserving Closure Conversion in Haskell](http://www.iro.umontreal.ca/~monnier/tcm.pdf) (which is concerned with type-preserving compilation in general and doesn't specifically address internal languages). However, their description of the conversion (in Section 5 of the paper) is sketchy and intermingled with concerns specific to their application.

The following Haskell code demonstrates the type-preserving conversion from a HOAS to a de Bruijn representation for a simple term language (essentially the lambda calculus with constants of arbitrary type). The two important aspects of the method are two:

The HOAS term language requires an extra variant (called Tag in the code) that reifies variables during the conversion — otherwise, a HOAS representation doesn't explicitly represent variables.
We require an explicit type representation (of the types of bound variables) during the conversion. We use Data.Typeable for that purpose.
The implementation consists of four Haskell modules:

* `HOAS.hs`: Typed terms in higher-order abstract syntax
* `DeBruijn.hs`: Typed terms using de Bruijn notation
* `Convert.hs`: Type-preserving conversion from HOAS to de Bruijn
* `Main.hs`: Some example conversions

(This code has been tested with GHC 6.11.)

__Update [9Jul09]__ Robert Atkey, Sam Lindley & Jeremy Yallop have a forthcoming paper entitled [Unembedding Domain-Specific Languages](http://homepages.inf.ed.ac.uk/ratkey/unembedding/) in the upcoming Haskell Symposium 2009 that addresses the exact same translation, but on the basis of a somewhat different representation of HOAS terms.
