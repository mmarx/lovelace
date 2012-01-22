with sys_resource_h;
with x86_64_linux_gnu_bits_time_h;
with x86_64_linux_gnu_bits_resource_h;

package body Timings is
  use Ada.Text_IO;

  package I_IO is new Ada.Text_IO.Integer_IO (Integer);

  function Resource_Usage (Who : in Usage_Who) return Usage_Type is
    package R renames sys_resource_h;
    package BT renames x86_64_linux_gnu_bits_time_h;
    package BR renames x86_64_linux_gnu_bits_resource_h;

    Usage : Usage_Type;

    The_Result : Interfaces.C.int;
    The_Who : BR.uu_rusage_who;
    The_Usage : aliased BR.rusage;

    use type Interfaces.C.int;

    function Time_Type_From_Timeval (Tv : in BT.timeval) return Time_Type is
      T : constant Time_Type := (Seconds =>
                                   Seconds_Type (Tv.tv_sec),
                                 Micro_Seconds =>
                                   Micro_Seconds_Type (Tv.tv_usec));
    begin
      return T;
    end Time_Type_From_Timeval;
  begin
    case Who is
      when Self =>
        The_Who := BR.RUSAGE_SELF;
      when Children =>
        The_Who := BR.RUSAGE_CHILDREN;
      when Thread =>
        The_Who := BR.RUSAGE_THREAD;
    end case;

    The_Result := R.getrusage (R.uu_rusage_who_t (The_Who),
                               The_Usage'Access);

    if The_Result /= 0 then
      raise Program_Error;
    end if;

    Usage.User_Time := Time_Type_From_Timeval (The_Usage.ru_utime);
    Usage.System_Time := Time_Type_From_Timeval (The_Usage.ru_stime);
    Usage.In_IO := IO_Operations_Type (The_Usage.ru_inblock);
    Usage.Out_IO := IO_Operations_Type (The_Usage.ru_oublock);
    Usage.Voluntary_Context_Switches :=
      Context_Switches_Type (The_Usage.ru_nvcsw);
    Usage.Involuntary_Context_Switches :=
      Context_Switches_Type (The_Usage.ru_nivcsw);

    return Usage;
  end Resource_Usage;

  procedure Put (Item : in Time_Type) is
  begin
    Put (File => Standard_Output,
         Item => Item);
  end Put;

  procedure Put (File : in File_Type;
                 Item : in Time_Type) is
    use I_IO;
  begin
    Put (File => File, Item => Integer (Item.Seconds), Width => 0);
    Put (File => File, Item => ".");
    Put (File => File, Item => Integer (Item.Micro_Seconds), Width => 0);
  end Put;

  procedure Put (Item : in Usage_Type) is
  begin
    Put (File => Standard_Output,
         Item => Item);
  end Put;

  procedure Put (File : in File_Type;
                 Item : in Usage_Type) is
    use I_IO;
  begin
    Put (File => File, Item => "User time: ");
    Put (File => File, Item => Item.User_Time);
    New_Line (File => File);

    Put (File => File, Item => "System time: ");
    Put (File => File, Item => Item.System_Time);
    New_Line (File => File);

    Put (File => File, Item => "Input I/O operations: ");
    Put (File => File, Item => Integer (Item.In_IO), Width => 0);
    New_Line (File => File);

    Put (File => File, Item => "Output I/O operations: ");
    Put (File => File, Item => Integer (Item.Out_IO), Width => 0);
    New_Line (File => File);

    Put (File => File, Item => "Voluntary context switches: ");
    Put (File => File,
         Item => Integer (Item.Voluntary_Context_Switches),
         Width => 0);
    New_Line (File => File);

    Put (File => File, Item => "Involuntary context switches: ");
    Put (File => File,
         Item => Integer (Item.Involuntary_Context_Switches),
         Width => 0);
    New_Line (File => File);
  end Put;
end Timings;
