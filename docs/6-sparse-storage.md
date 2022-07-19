# 6 - Optimised data storage

In addition to using faster codes such as Fortran, we can also achieve some significant speed increases by being careful with our use of memory. One important optimisation in our code will be how we store the data in our matrix $A$. The matrix $A$ for a 3-node and 5-node system can be seen below, where $a$, $b$ and $c$ are our lower, main and upper diagonals respectively. We can already begin to see that we are storing a large portion of $0$ 's in our system, a problem which will only increase as we increase our node numbers by orders of magnitude.

$$
\left(\begin{array}{ccc}
b & c & 0\\
a & b & c\\
0 & a & b\\
\end{array}\right)
\rightarrow
\left(\begin{array}{ccccc}
b & c & 0 & 0 & 0\\
a & b & c & 0 & 0\\
0 & a & b & c & 0\\
0 & 0 & a & b & c\\
0 & 0 & 0 & a & b\\
\end{array}\right)
$$

These matrices are known as 'sparse', as much of the data that they contain is of value $0$. We can therefore drastically reduce the amount of memory needed to store this matrix by only saving the values and the positions that they occupy. A number of efficient storage systems exist for this problem, but we have chosen to use Compressed Row Storage (CRS) in this case as it makes no assumptions about the structure of the matrix and can therefore be applied to a large range of problems. A good explanation of this methodology can be found at <http://netlib.org/linalg/html_templates/node91.html>.

<!-- TODO: I would add more about compressed storages, links to Wiki -->