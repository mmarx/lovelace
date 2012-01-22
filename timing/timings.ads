with Ada.Text_IO;
with Interfaces.C;

package Timings is
  type Usage_Who is (Self, Children, Thread);

  type Seconds_Type is new Interfaces.C.long;
  type Micro_Seconds_Type is new Interfaces.C.unsigned;
  type IO_Operations_Type is new Interfaces.C.long;
  type Context_Switches_Type is new Interfaces.C.long;

  type Time_Type is record
    Seconds : Seconds_Type;
    Micro_Seconds : Micro_Seconds_Type;
  end record;

  type Usage_Type is record
    User_Time : Time_Type;
    System_Time : Time_Type;
    In_IO : IO_Operations_Type;
    Out_IO : IO_Operations_Type;
    Voluntary_Context_Switches : Context_Switches_Type;
    Involuntary_Context_Switches : Context_Switches_Type;
  end record;

  function Resource_Usage (Who : in Usage_Who) return Usage_Type;

  procedure Put (Item : in Time_Type);
  procedure Put (File : in Ada.Text_IO.File_Type;
                 Item : in Time_Type);

  procedure Put (Item : in Usage_Type);
  procedure Put (File : in Ada.Text_IO.File_Type;
                 Item : in Usage_Type);
end Timings;
