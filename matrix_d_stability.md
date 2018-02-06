D-stability of interaction matrices
================
Hao Ye
2018 February 06

## Definitions

Suppose we have an *interaction* matrix, \(A\) that is defined as:
\[A_{ij} = \left.\frac{\partial\left(\frac{1}{N_i}\frac{dN_i}{dt}\right)}{\partial N_j}\right|_{N^*}\]

evaluated at some equilibrium point, \(N^*\).

Then, the corresponding *community* matrix, \(C\) is:
\[C_{ij} = \left.\frac{\partial\left(\frac{dN_i}{dt}\right)}{\partial N_j}\right|_{N^*} = {N_i}^* A_{ij}\]

Furthermore, a matrix, \(M\), is *stable* (asymptotic stability,
Lyapunov stability) if the real part of each eigenvalue is negative:
\[\forall \lambda_M : \operatorname{Re}{\lambda} < 0\]

and a matrix, \(M\), is *D-stable* if \(DM\) is stable for any positive
diagonal \(D\) (all entries on the main diagonal are positive, and the
rest are 0). Note that if \(M\) is D-stable, it is also stable (set
\(D\) to the identity matrix.)

## Example

Not all stable matrices are D-stable. From Logofet (2005), we borrow the
example of a matrix with a particular sign pattern:

``` r
set.seed(45)
A <- matrix(c(abs(rnorm(1)), -abs(rnorm(1)), 
              abs(rnorm(1)), -abs(rnorm(1))), byrow = TRUE, nrow = 2)
cat(A)
```

    ## 0.3407997 0.3795377 -0.7033403 -0.7460474

Is `A` stable?

``` r
lambda <- eigen(A)$values
cat(lambda)
```

    ## -0.371043 -0.03420479

``` r
all(Re(lambda) < 0)
```

    ## [1] TRUE

What if we multiply `A` by a positive diagonal matrix?

``` r
N <- diag(runif(2) * c(100, 10) + c(100, 10))
C <- A %*% N
```

Is `C` stable?

``` r
lambda <- eigen(C)$values
cat(lambda)
```

    ## 32.4036 0.4664075

``` r
all(Re(lambda) < 0)
```

    ## [1] FALSE
