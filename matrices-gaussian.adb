package body Matrices.Gaussian is
  procedure LU_Decomposition (A : Matrix_Access;
                              L, U : Matrix_Access) is

    B : Matrix_Access := new Matrix (A'Range (1),
                                     A'Range (2));

    F : constant Integer := A'First (1);
    N : constant Integer := A'Last (1);

  begin
    if not Is_Square (A.all) then
      raise Non_Square_Matrix;
    end if;

    B.all := A.all;

    for I in F .. N loop
      for J in I .. N loop
        for K in F .. I - 1 loop
          B.all (I, J) := B.all (I, J) - B.all (I, K) * B.all (K, J);
        end loop;
      end loop;

      for J in I + 1 .. N loop
        for K in F .. I - 1 loop
          B.all (J, I) := B.all (J, I) - B.all (J, K) * B.all (K, I);
        end loop;

        B.all (J, I) := B.all (J, I) / B.all (I, I);
      end loop;
    end loop;

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
  exception
    when others =>
      Free (B);

      raise;
  end LU_Decomposition;

  function Forward_Substitution (L : Matrix_Access;
                                 B : Vector_Access) return Vector_Access is
    Y : Vector_Access := new Vector (B'Range);
    S : Scalar := Zero;
  begin
    if not Is_Square (L.all) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (L.all, L'First (1)), B.all);

    for I in Y'Range loop
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

  function Backward_Substitution (U : Matrix_Access;
                                  Y : Vector_Access) return Vector_Access is
    X : Vector_Access := new Vector (Y'Range);
    S : Scalar := Zero;
  begin
    if not Is_Square (U.all) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (U.all, U'First (1)), Y.all);

    for I in reverse X'Range loop
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

  function LU_Solve (A : Matrix_Access;
                     B : Vector_Access) return Vector_Access is
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
    end;

    Free (L);
    Free (U);
    Free (Y);

    return X;
  end LU_Solve;
end Matrices.Gaussian;
