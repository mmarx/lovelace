with Ada.Text_IO;

with Matrices;
with Matrices.Gaussian;

with Timings;
with Timings.Benchmarks;

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

  A : constant Matrix (1 .. 2, 1 .. 2) := ((1.0, 1.0),
                                           (1.0, 0.0));
  B : constant Vector (1 .. 2) := (23.0, 42.0);

  type Parameter_Type (N : Integer) is record
    A : Matrix (1 .. N, 1 .. N);
    B : Vector (1 .. N);
  end record;

  package BM is new Timings.Benchmarks (Parameter_Type);

  procedure Benchmark_LU (Parameters : in Parameter_Type) is
    X : Vector (Parameters.B'Range);
  begin
    X := LU_Solve (Parameters.A, Parameters.B);
    pragma Unreferenced (X);
  end Benchmark_LU;

begin
  BM.Benchmark (What => Benchmark_LU'Access,
                Name => "2x2 LU decomposition",
                Parameters => (N => 2,
                               A => A,
                               B => B));
end Lovelace;
