with Ada.Text_IO;

generic
  type Scalar is private;

  Zero : in Scalar;
  One : in Scalar;

  pragma Unreferenced (One);

  with function "+" (Left, Right : Scalar) return Scalar is <>;
  with function "-" (Right : Scalar) return Scalar is <>;
  with function "-" (Left, Right : Scalar) return Scalar is <>;
  pragma Unreferenced ("-");

  with function "*" (Left, Right : Scalar) return Scalar is <>;
  with function "/" (Left, Right : Scalar) return Scalar is <>;
  with function "abs" (Right : Scalar) return Scalar is <>;

  with procedure Put (File : in Ada.Text_IO.File_Type;
                      Item : in Scalar) is <>;
  pragma Unreferenced (Put);

  with procedure Put (Item : in Scalar) is <>;

  with procedure Get (File : in Ada.Text_IO.File_Type;
                      Item : out Scalar;
                      Width : in Ada.Text_IO.Field := 0) is <>;
  pragma Unreferenced (Get);

  with procedure Get (Item : out Scalar;
                      Width : in Ada.Text_IO.Field := 0) is <>;
  pragma Unreferenced (Get);
package Matrices is
  Dimension_Mismatch : exception;
  Singular_Matrix : exception;

  type Vector is array (Integer range <>) of Scalar;
  type Matrix is array (Integer range <>,
                        Integer range <>) of Scalar;

  function "+" (Left, Right : in Vector) return Vector;
  function "-" (Right : in Vector) return Vector;
  function "-" (Left, Right : in Vector) return Vector;
  function "*" (Left, Right : in Vector) return Scalar;
  function "*" (Left, Right : in Vector) return Vector;
  function "*" (Left : in Scalar;
                Right : in Vector) return Vector;
  function "*" (Left : in Vector;
                Right : in Scalar) return Vector;
  function "/" (Left : in Vector;
                Right : in Scalar) return Vector;
  function "abs" (Right : in Vector) return Vector;

  function Sum (Right : in Vector) return Scalar;

  function "+" (Left, Right : in Matrix) return Matrix;
  function "-" (Right : in Matrix) return Matrix;
  function "-" (Left, Right : in Matrix) return Matrix;
  function "*" (Left : in Vector;
                Right : in Matrix) return Vector;
  function "*" (Left : in Matrix;
                Right : in Vector) return Vector;
  function "*" (Left, Right : in Matrix) return Matrix;

  function Row (A : in Matrix;
                M : in Integer) return Vector;
  function Column (A : in Matrix;
                   N : in Integer) return Vector;

  function Transpose (A : in Matrix) return Matrix;

  function Is_Diagonal (A : in Matrix) return Boolean;
  function Is_Triangular (A : in Matrix) return Boolean;
  function Is_Square (A : in Matrix) return Boolean;
  function Is_Lower_Triangular (A : in Matrix) return Boolean;

  procedure Put (V : in Vector);
  procedure Put (A : in Matrix);

private
  procedure Equal_Range (Left, Right : in Vector);
  procedure Equal_Ranges (Left, Right : in Matrix);
end Matrices;
