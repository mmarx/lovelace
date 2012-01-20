generic
package Matrices.Gaussian is
  Non_Square_Matrix : exception;

  procedure LU_Decomposition (A : in Matrix;
                              L, U : out Matrix);

  function Forward_Substitution (L : in Matrix;
                                 B : in Vector) return Vector;
  function Backward_Substitution (U : in Matrix;
                                  Y : in Vector) return Vector;
end Matrices.Gaussian;
