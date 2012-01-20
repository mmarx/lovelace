package body Matrices.Gaussian is
  procedure LU_Decomposition (A : in Matrix;
                              L, U : out Matrix) is

    B : Matrix := A;

    F : constant Integer := A'First (1);
    N : constant Integer := A'Last (1);

  begin
    if not Is_Square (A) then
      raise Non_Square_Matrix;
    end if;

    for I in F .. N loop
      for J in I .. N loop
        for K in F .. I - 1 loop
          B (I, J) := B (I, J) - B (I, K) * B (K, J);
        end loop;
      end loop;

      for J in I + 1 .. N loop
        for K in F .. I - 1 loop
          B (J, I) := B (J, I) - B (J, K) * B (K, I);
        end loop;

        B (J, I) := B (J, I) / B (I, I);
      end loop;
    end loop;

    for I in F .. N loop
      for J in F .. N loop
        if I > J then
          L (J, I) := Zero;
          L (I, J) := B (I, J);
          U (I, J) := Zero;
          U (J, I) := B (J, I);
        end if;
      end loop;

      L (I, I) := One;
      U (I, I) := B (I, I);
    end loop;
  end LU_Decomposition;

  function Forward_Substitution (L : in Matrix;
                                 B : in Vector) return Vector is
    Y : Vector (B'Range) := (others => Zero);
    S : Scalar := Zero;
  begin
    if not Is_Square (L) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (L, L'First (1)), B);

    for I in Y'Range loop
      for K in Y'First .. I - 1 loop
        S := S + L (I, K) * Y (K);
      end loop;

      Y (I) := (B (I) - S) / L (I, I);
    end loop;

    return Y;
  end Forward_Substitution;

  function Backward_Substitution (U : in Matrix;
                                  Y : in Vector) return Vector is
    X : Vector (Y'Range) := (others => Zero);
    S : Scalar := Zero;
  begin
    if not Is_Square (U) then
      raise Non_Square_Matrix;
    end if;

    Equal_Range (Row (U, U'First (1)), Y);

    for I in reverse X'Range loop
      for K in I + 1 .. X'Last loop
        S := S + U (I, K) * X (K);
      end loop;

      X (I) := (Y (I) - S) / U (I, I);
    end loop;

    return X;
  end Backward_Substitution;

end Matrices.Gaussian;
