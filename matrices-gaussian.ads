generic
package Matrices.Gaussian is
  Non_Square_Matrix : exception;

  procedure LU_Decomposition (A : in Matrix_Access;
                              L, U : in Matrix_Access);

  function Forward_Substitution (L : in Matrix_Access;
                                 B : in Vector_Access) return Vector_Access;
  function Backward_Substitution (U : in Matrix_Access;
                                  Y : in Vector_Access) return Vector_Access;

  function LU_Solve (A : in Matrix_Access;
                     B : in Vector_Access) return Vector_Access;

  procedure LU_Solve_Destructive (A : in Matrix_Access;
                                  B : in Vector_Access);

private
  procedure LU_Decomposition_Destructive (A : in Matrix_Access);
end Matrices.Gaussian;
