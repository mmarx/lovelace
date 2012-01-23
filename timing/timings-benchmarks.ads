generic
  type Parameter_Type (<>) is limited private;
package Timings.Benchmarks is
  type Benchmarkable is access procedure (Parameters : in Parameter_Type);

  procedure Benchmark (What : in Benchmarkable;
                       Name : in String;
                       Parameters : in Parameter_Type;
                       Who : in Usage_Who := Self);
end Timings.Benchmarks;
