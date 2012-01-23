with Ada.Text_IO;

with Matrices;
with Matrices.Gaussian;

with Timings;
with Timings.Benchmarks;

procedure Lovelace is
  use Ada.Text_IO;
  package F_IO is new Float_IO (Float);

  use F_IO;

  procedure Put (File : File_Type;
                 Item : in Float) is
  begin
    F_IO.Put (File, Item);
  end Put;

  procedure Put (Item : in Float) is
  begin
    F_IO.Put (Item);
  end Put;

  package M is new Matrices (Scalar => Float,
                             Zero => 0.0,
                             One => 1.0);
  package G is new M.Gaussian;

  use M;

  --  A : aliased constant Matrix := (1 => (1 => 1.0, 2 => 1.0),
  --                                  2 => (1 => 1.0, 2 => 0.0));
  --  B : aliased constant Vector := (1 => 23.0, 2 => 42.0);

  type Parameter_Type (N : Integer) is record
    A : Matrix_Access;
    B : Vector_Access;
  end record;

  package BM is new Timings.Benchmarks (Parameter_Type);

  procedure Benchmark_LU (Parameters : in Parameter_Type) is
    X : Vector_Access := G.LU_Solve (Parameters.A, Parameters.B);

  begin
    Free (X);
  end Benchmark_LU;

  function Matrix_From_File (Name : String;
                             Rows, Columns : Positive)
                            return Matrix_Access is
    Result : constant Matrix_Access := new Matrix (1 .. Rows,
                                                   1 .. Columns);
    File : File_Type;
  begin
    Open (File => File,
          Mode => In_File,
          Name => Name);
    Get (File => File,
         Item => Result.all);
    Close (File => File);

    return Result;
  end Matrix_From_File;

  A1024 : Matrix_Access := Matrix_From_File (Name => "/home/mmarx/danielbench/data/single-float-1024",
                                             Rows => 1024,
                                             Columns => 1024);
  Z1024 : Vector_Access := new Vector (1 .. 1024); -- => 0.0);
begin
  Z1024.all := (others => 0.0);

  --  BM.Benchmark (What => Benchmark_LU'Access,
  --                Name => "2x2 LU decomposition",
  --                Parameters => (N => 2,
  --                               A => A'Access,
  --                               B => B'Access));

  BM.Benchmark (What => Benchmark_LU'Access,
                Name => "1024x1024 single LU decomposition",
                Parameters => (N => 1024,
                               A => A1024,
                               B => Z1024));

  Free (A1024);
  Free (Z1024);
end Lovelace;
