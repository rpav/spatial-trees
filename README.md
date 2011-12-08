This code was written by Christophe Rhodes, and the following
description was taken from
[cliki.net](http://www.cliki.net/spatial-trees).

spatial-trees
=============

spatial-trees is a set of dynamic index data structures for
spatially-extended data. The flavors provided are, as of the 0.1
release (on 2004-12-03):

* R-trees, as in R-TREES: A DYNAMIC INDEX STRUCTURE FOR SPATIAL
  SEARCHING, Antonin Guttman, Proc. ACM SIGMOD Int. Conf. on
  Management of Data, 1984.
  
* Greene-trees, as in An Implementation and Performance Analysis of
  Spatial Data Access Methods, Diane Greene, Proc. 5th IEEE
  Int. Conf. on Data Engineering, 1989.
  
* R*-trees, as in The R*-tree: An Efficient and Robust Access Method
  for Points and Rectangles, Beckmann, Kriegel, Schneider and Seeger,
  Proc. ACM Int. Conf. on Management of Data, 1990
  
* X-trees, as in The X-tree: An Index Structure for High-Dimensional
  Data, Berchtold, Keim and Kriegel, Proc. 22th Int. Conf. on Very
  Large Databases, 1996

Future work planned includes performance enhancements, incorporation
of more index structures, and some work on supporting more optimal
indexing when the entire set of data is known at index creation time;
for more details, see the TODO file in the binary distribution.

The code is licensed BSD-style, and is intended to be similar in
spirit to Nathan Froyd's TREES Library.
