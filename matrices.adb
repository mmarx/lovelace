package body Matrices is
  use Ada.Text_IO;

  function "+" (Left, Right : in Vector) return Vector is
    Result : Vector := Left;
  begin
    Equal_Range (Left, Right);

    for I in Right'Range loop
      Result (I) := Result (I) + Right (I);
    end loop;

    return Result;
  end "+";

  function "-" (Right : in Vector) return Vector is
    Result : Vector := Right;
  begin
    for I in Result'Range loop
      Result (I) := -Result (I);
    end loop;

    return Result;
  end "-";

  function "-" (Left, Right : in Vector) return Vector is
  begin
    Equal_Range (Left, Right);

    return Left + (-Right);
  end "-";

  function "*" (Left, Right : in Vector) return Vector is
    Result : Vector := Left;
  begin
    Equal_Range (Left, Right);

    for I in Result'Range loop
      Result (I) := Result (I) * Right (I);
    end loop;

    return Result;
  end "*";

  function "*" (Left, Right : in Vector) return Scalar is
  begin
    Equal_Range (Left, Right);

    return Sum (Left * Right);
  end "*";

  function "*" (Left : in Scalar;
                Right : in Vector) return Vector is
    Result : Vector := Right;
  begin
    for I in Result'Range loop
      Result (I) := Left * Result (I);
    end loop;

    return Result;
  end "*";

  function "*" (Left : in Vector;
                Right : in Scalar) return Vector is
  begin
    return Right * Left;
  end "*";

  function "/" (Left : in Vector;
                Right : in Scalar) return Vector is
    Result : Vector := Left;
  begin
    for I in Result'Range loop
      Result (I) := Result (I) / Right;
    end loop;

    return Result;
  end "/";

  function "abs" (Right : in Vector) return Vector is
    Result : Vector := Right;
  begin
    for I in Result'Range loop
      Result (I) := abs Result (I);
    end loop;

    return Result;
  end "abs";

  function Sum (Right : in Vector) return Scalar is
    Result : Scalar := Right (Right'First);
  begin
    for I in Right'First + 1 .. Right'Last loop
      Result := Result + Right (I);
    end loop;

    return Result;
  end Sum;

  function "+" (Left, Right : in Matrix) return Matrix is
    Result : Matrix := Left;
  begin
    Equal_Ranges (Left, Right);

    for I in Result'Range (1) loop
      for J in Result'Range (2) loop
        Result (I, J) := Result (I, J) + Right (I, J);
      end loop;
    end loop;

    return Result;
  end "+";

  function "-" (Right : in Matrix) return Matrix is
    Result : Matrix := Right;
  begin
    for I in Result'Range (1) loop
      for J in Result'Range (2) loop
        Result (I, J) := -Result (I, J);
      end loop;
    end loop;

    return Result;
  end "-";

  function "-" (Left, Right : in Matrix) return Matrix is
  begin
    Equal_Ranges (Left, Right);

    return Left + (-Right);
  end "-";

  function "*" (Left : in Vector;
                Right : in Matrix) return Vector is
    Result : Vector (Right'Range (2));
  begin
    Equal_Range (Left, Row (Right, Right'First (1)));

    for J in Result'Range loop
      Result (J) := Left * Column (Right, J);
    end loop;

    return Result;
  end "*";

  function "*" (Left : in Matrix;
                Right : in Vector) return Vector is
    Result : Vector (Left'Range (1));
  begin
    Equal_Range (Column (Left, Left'First (2)), Right);

    for I in Result'Range loop
      Result (I) := Row (Left, I) * Right;
    end loop;

    return Result;
  end "*";

  function "*" (Left, Right : in Matrix) return Matrix is
    Result : Matrix (Left'Range (1),
                     Right'Range (2));
  begin
    Equal_Range (Column (Left, Left'First (2)),
                 Row (Right, Right'First (1)));

    for I in Result'Range (1) loop
      for J in Result'Range (2) loop
        Result (I, J) := Row (Left, I) * Column (Right, J);
      end loop;
    end loop;

    return Result;
  end "*";

  function Row (A : in Matrix;
                M : in Integer) return Vector is
    Result : Vector (A'Range (2));
  begin
    for J in Result'Range loop
      Result (J) := A (M, J);
    end loop;

    return Result;
  end Row;

  function Column (A : in Matrix;
                   N : in Integer) return Vector is
    Result : Vector (A'Range (1));
  begin
    for I in Result'Range loop
      Result (I) := A (I, N);
    end loop;

    return Result;
  end Column;

  function Transpose (A : in Matrix) return Matrix is
    Result : Matrix (A'Range (2),
                     A'Range (1));
  begin
    for I in Result'Range (1) loop
      for J in Result'Range (2) loop
        Result (I, J) := A (J, I);
      end loop;
    end loop;

    return Result;
  end Transpose;

  function Is_Diagonal (A : in Matrix) return Boolean is
  begin
    for I in A'Range (1) loop
      for J in A'Range (2) loop
        if I /= J
          and then A (I, J) /= Zero
        then
          return False;
        end if;
      end loop;
    end loop;

    return True;
  end Is_Diagonal;

  function Is_Triangular (A : in Matrix) return Boolean is
  begin
    return Is_Lower_Triangular (A)
      or else Is_Lower_Triangular (Transpose (A));
  end Is_Triangular;

  function Is_Square (A : in Matrix) return Boolean is
  begin
    return A'Length (1) = A'Length (2)
      and then A'First (1) = A'First (2);
  end Is_Square;

  function Is_Lower_Triangular (A : in Matrix) return Boolean is
  begin
    for I in A'Range (1) loop
      for J in A'Range (2) loop
        if I > J
          and then A (I, J) /= Zero
        then
          return False;
        end if;
      end loop;
    end loop;

    return True;
  end Is_Lower_Triangular;

  procedure Put (Item : in Vector) is
  begin
    Put (File => Standard_Output,
         Item => Item);
  end Put;

  procedure Put (File : in File_Type;
                 Item : in Vector) is
  begin
    Put (File => File,
         Item => "[ ");

    for I in Item'Range loop
      Put (File => File,
           Item => Item (I));
      if I /= Item'Last then
        Put (File => File,
             Item => ", ");
      end if;
    end loop;

    Put (File => File,
         Item => " ]");
  end Put;

  procedure Put (Item : in Matrix) is
  begin
    Put (File => Standard_Output,
         Item => Item);
  end Put;

  procedure Put (File : in File_Type;
                 Item : in Matrix) is
  begin
    Put (File => File,
         Item => "[");
    for I in Item'Range (1) loop
      if I /= Item'First (1) then
        Put (File => File,
             Item => " ");
      end if;

      Put (File => File,
           Item => Row (Item, I));

      if I /= Item'Last (1) then
        Put (File => File,
             Item => ",");
        New_Line (File => File);
      end if;
    end loop;
    Put (File => File,
         Item => "]");
  end Put;

  procedure Get (Item : out Vector) is
  begin
    Get (File => Standard_Input,
         Item => Item);
  end Get;

  procedure Get (File : in File_Type;
                Item : out Vector) is
  begin
    for I in Item'Range loop
      Get (File => File,
          Item => Item (I));
    end loop;
  end Get;

  procedure Get (Item : out Matrix) is
  begin
    Get (File => Standard_Input,
         Item => Item);
  end Get;

  procedure Get (File : in File_Type;
                 Item : out Matrix) is
  begin
    for I in Item'Range (1) loop
      for J in Item'Range (2) loop
        Get (File => File,
             Item => Item (I, J));
      end loop;
    end loop;
  end Get;

  procedure Equal_Range (Left, Right : in Vector) is
  begin
    if Left'Length /= Right'Length
      or else Left'First /= Right'First
    then
      raise Dimension_Mismatch;
    end if;
  end Equal_Range;

  procedure Equal_Ranges (Left, Right : in Matrix) is
  begin
    if Left'Length (1) /= Right'Length (1)
      or else Left'Length (2) /= Right'Length (2)
      or else Left'First (1) /= Right'First (1)
      or else Left'First (2) /= Right'First (2)
    then
      raise Dimension_Mismatch;
    end if;
  end Equal_Ranges;
end Matrices;
