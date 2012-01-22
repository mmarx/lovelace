with Ada.Text_IO;

with Matrices;
with Matrices.Gaussian;

with Timings;

procedure Lovelace is
  use Ada.Text_IO;
  package I is new Float_IO (Float);

  use I;

  procedure Put (File : File_Type;
                 Item : in Float) is
  begin
    I.Put (File, Item);
  end Put;

  procedure Put (Item : in Float) is
  begin
    I.Put (Item);
  end Put;

  package M is new Matrices (Scalar => Float,
                             Zero => 0.0,
                             One => 1.0);
  package G is new M.Gaussian;

  use M;
  use G;
  use I;

  A : constant Matrix (1 .. 2, 1 .. 2) := ((1.0, 1.0),
                                           (1.0, 0.0));
  B : constant Vector (1 .. 2) := (23.0, 42.0);

  L, U : Matrix (1 .. 2, 1 .. 2);
  X, Y : Vector (1 .. 2);
begin
  Put_Line ("hello, world!");

  LU_Decomposition (A, L, U);
  Y := Forward_Substitution (L, B);
  X := Backward_Substitution (U, Y);

  Put (X);
  New_Line;

  Y := A * X;

  New_Line;

  Put (Y);
  New_Line;

  New_Line;

  Put (L);
  New_Line;

  New_Line;

  Put (U);
  New_Line;

  Timings.Put (Timings.Resource_Usage (Timings.Self));
end Lovelace;
