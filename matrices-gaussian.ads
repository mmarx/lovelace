generic
package Matrices.Gaussian is
  Non_Square_Matrix : exception;

  procedure LU_Decomposition (A : Matrix_Access;
                              L, U : Matrix_Access);

  function Forward_Substitution (L : Matrix_Access;
                                 B : Vector_Access) return Vector_Access;
  function Backward_Substitution (U : Matrix_Access;
                                  Y : Vector_Access) return Vector_Access;

  function LU_Solve (A : Matrix_Access;
                     B : Vector_Access) return Vector_Access;
end Matrices.Gaussian;
