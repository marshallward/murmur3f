"TreMur", a layout-independent Murmur hash(?)
=============================================

Rough idea for a layout-independent Murmur hash of a 2D array.

1. Divide the domain into 2x2 cells:


   +--+--+--+--+
   |10|11|14|15|
   +--+--+--+--+
   | 8| 9|12|13|
   +--+--+--+--+
   | 2| 3| 6| 7|
   +--+--+--+--+
   | 0| 1| 4| 5|
   +--+--+--+--+

2. Distribute values as a tree:

   .. image:: img/murmurtree.svg

3.  Fill in as much of tree as possible on each domain (CPU), then copy that
    part to a root node (rank 0), and finish the tree summation.


Communication will either be equal to a global bit gather (one number per node)
or something worse, but generally not as bad as an all-gather of a global
field.  (What is worst case?  Haven't gotten that far yet...)


1. Assign node index to (i,j) points

   .. math::

      b_{ij} = \sum_{k=0}^{N_b} b_{ij}^{(k)}


      b_{ij}^{(k)} = \ &
         2^{2k} \left\lfloor \frac{i}{2^k} \right\rfloor \text{mod} \ 2 \\
         & + \ 2^k N^{(k)}_i \left\lfloor \frac{j}{2^k} \right\rfloor \text{mod} \ 2 \\
         & + \ \left(2^k N^{(k)}_j - 2^{2k+1} \right)
            \left\lfloor \frac{i}{2^k} \right\rfloor \text{mod} \ 2
            \left\lfloor \frac{j}{2^k} \right\rfloor \text{mod} \ 2

      N_i^{(k)} =
         \min\left(
            2^{k+1} \left(
               \left\lfloor \frac{i}{2^{k+1}} \right\rfloor + 1
            \right), N_i
         \right)
         - 2^{k+1} \left\lfloor \frac{i}{2^{k+1}} \right\rfloor


   Need pictures to explain these, but it's basically just filling boxes.

   The :math:`N^{(k)}_{i}` expression can probably be written in a simpler way.
   If far from a boundary it's just :math:`2^{k+1}`.  If not, it's
   :math:`N_i` minus some 2^n pieces...
