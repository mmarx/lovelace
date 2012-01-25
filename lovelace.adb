with Ada.Text_IO;

with Matrices;
with Matrices.Gaussian;

with Timings;
with Timings.Benchmarks;

procedure Lovelace is
  type My_Float is new Float;

  use Ada.Text_IO;
  package F_IO is new Float_IO (My_Float);

  use F_IO;

  procedure Put (File : File_Type;
                 Item : in My_Float) is
  begin
    F_IO.Put (File, Item);
  end Put;

  procedure Put (Item : in My_Float) is
  begin
    F_IO.Put (Item);
  end Put;

  package M is new Matrices (Scalar => My_Float,
                             Zero => 0.0,
                             One => 1.0);
  package G is new M.Gaussian;

  use M;

  type LU_Parameter_Type is record
    A : Matrix_Access;
    B : Vector_Access;
  end record;

  type Matmul_Parameter_Type is record
    A : Matrix_Access;
    B : Matrix_Access;
  end record;

  package LU_Benchmark is new Timings.Benchmarks (LU_Parameter_Type);
  package Matmul_Benchmark is new Timings.Benchmarks (Matmul_Parameter_Type);

  procedure Benchmark_LU (Parameters : in LU_Parameter_Type) is
    X : Vector_Access := G.LU_Solve (Parameters.A, Parameters.B);

  begin
    Free (X);
  end Benchmark_LU;

  procedure Benchmark_Matmul (Parameters : in Matmul_Parameter_Type) is
    X : Matrix_Access := new Matrix (Parameters.A.all'Range (2),
                                     Parameters.B.all'Range (1));
  begin
    X.all := Parameters.A.all * Parameters.B.all;

    Free (X);
  end Benchmark_Matmul;

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

  Matmul_Benchmark.Benchmark (What => Benchmark_Matmul'Access,
                              Name => "1024x1024 matmul, digits" &
                                Integer'Image (My_Float'Digits),
                              Parameters => (A => A1024,
                                             B => A1024));

  LU_Benchmark.Benchmark (What => Benchmark_LU'Access,
                          Name => "1024x1024 LU decomposition, digits" &
                            Integer'Image (My_Float'Digits),
                          Parameters => (A => A1024,
                                         B => Z1024));

  Free (Z1024);
  Free (A1024);
end Lovelace;
