with Ada.Text_IO;

package body Timings.Benchmarks is
  procedure Benchmark (What : in Benchmarkable;
                       Name : in String;
                       Parameters : in Parameter_Type;
                       Who : in Usage_Who := Self) is
    Initial, Final : Usage_Type;

    use Ada.Text_IO;
  begin
    Put_Line (File => Standard_Error,
              Item => "-!- Starting benchmark `" & Name & "'.");

    Initial := Resource_Usage (Who);

    What (Parameters);

    Final := Resource_Usage (Who);

    Put_Line (File => Standard_Error,
              Item => "-!- Resource usage for `" & Name & "':");

    Put (File => Standard_Error,
         Item => Final - Initial);

    Put_Line (File => Standard_Error,
              Item => "-!- Done with benchmark `" & Name & "'.");
  end Benchmark;
end Timings.Benchmarks;
