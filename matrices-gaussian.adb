package body Matrices.Gaussian is
  procedure LU_Decomposition_Destructive (A : in Matrix_Access) is
    F : constant Integer := A'First (1);
    N : constant Integer := A'Last (1);
  begin
    for I in F .. N loop
      for J in I .. N loop
        for K in F .. I - 1 loop
          A.all (I, J) := A.all (I, J) - A.all (I, K) * A.all (K, J);
        end loop;
      end loop;

      for J in I + 1 .. N loop
        for K in F .. I - 1 loop
          A.all (J, I) := A.all (J, I) - A.all (J, K) * A.all (K, I);
        end loop;

        A.all (J, I) := A.all (J, I) / A.all (I, I);
      end loop;
    end loop;
  end LU_Decomposition_Destructive;

  procedure LU_Decomposition (A : in Matrix_Access;
                              L, U : in Matrix_Access) is

    B : Matrix_Access := new Matrix (A'Range (1),
                                     A'Range (2));

    F : constant Integer := A'First (1);
    N : constant Integer := A'Last (1);

  begin
    if not Is_Square (A.all) then
      raise Non_Square_Matrix;
    end if;

    B.all := A.all;

    LU_Decomposition_Destructive (B);

    for I in F .. N loop
      for J in F .. N loop
        if I > J then
          L.all (J, I) := Zero;
          L.all (I, J) := B.all (I, J);
          U.all (I, J) := Zero;
          U.all (J, I) := B.all (J, I);
        end if;
      end loop;

      L.all (I, I) := One;
      U.all (I, I) := B.all (I, I);
    end loop;

    Free (B);
  exception
    when others =>
      Free (B);

      raise;
  end LU_Decomposition;

  function Forward_Substitution (L : in Matrix_Access;
                                 B : in Vector_Access) return Vector_Access is
    Y : Vector_Access := new Vector (B'Range);
    S : Scalar := Zero;
  begin
    if not Is_Square (L.all) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (L.all, L'First (1)), B.all);

    for I in Y'Range loop
      S := Zero;

      for K in Y'First .. I - 1 loop
        S := S + L.all (I, K) * Y.all (K);
      end loop;

      Y.all (I) := (B.all (I) - S) / L.all (I, I);
    end loop;

    return Y;
  exception
    when others =>
      Free (Y);

      raise;
  end Forward_Substitution;

  function Backward_Substitution (U : in Matrix_Access;
                                  Y : in Vector_Access) return Vector_Access is
    X : Vector_Access := new Vector (Y'Range);
    S : Scalar := Zero;
  begin
    if not Is_Square (U.all) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (U.all, U'First (1)), Y.all);

    for I in reverse X'Range loop
      S := Zero;

      for K in I + 1 .. X'Last loop
        S := S + U.all (I, K) * X.all (K);
      end loop;

      X.all (I) := (Y.all (I) - S) / U.all (I, I);
    end loop;

    return X;
  exception
    when others =>
      Free (X);

      raise;
  end Backward_Substitution;

  function LU_Solve (A : in Matrix_Access;
                     B : in Vector_Access) return Vector_Access is
    L : Matrix_Access := new Matrix (A'Range (1),
                                     A'Range (2));
    U : Matrix_Access := new Matrix (A'Range (1),
                                     A'Range (2));
    X : Vector_Access := new Vector (B'Range);
    Y : Vector_Access := new Vector (B'Range);
  begin
    declare
    begin
      LU_Decomposition (A, L, U);
      Y := Forward_Substitution (L, B);
      X := Backward_Substitution (U, Y);
    exception
      when others =>
        Free (L);
        Free (U);
        Free (X);
        Free (Y);

        raise;
    end;

    Free (L);
    Free (U);
    Free (Y);

    return X;
  end LU_Solve;

  procedure LU_Solve_Destructive (A : in Matrix_Access;
                                  B : in Vector_Access) is
    S : Scalar := Zero;
  begin
    if not Is_Square (A.all) then
      raise Non_Square_Matrix;
    end if;

    LU_Decomposition_Destructive (A);

    -- forward substitution
    for I in B'Range loop
      S := Zero;

      for K in B'First .. I - 1 loop
        S := S + A.all (I, K) * B.all (K);
      end loop;

      B.all (I) := (B.all (I) - S);
    end loop;

    -- backward substitution
    for I in reverse B'Range loop
      S := Zero;

      for K in I + 1 .. B'Last loop
        S := S + A.all (I, K) * B.all (K);
      end loop;

      B.all (I) := (B.all (I) - S) / A.all (I, I);
    end loop;
  end LU_Solve_Destructive;
end Matrices.Gaussian;
