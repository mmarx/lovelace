with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;

package x86_64_linux_gnu_bits_time_h is

   type timeval is record
      tv_sec : aliased x86_64_linux_gnu_bits_types_h.uu_time_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:77
      tv_usec : aliased x86_64_linux_gnu_bits_types_h.uu_suseconds_t;  -- /usr/include/x86_64-linux-gnu/bits/time.h:78
   end record;
   pragma Convention (C_Pass_By_Copy, timeval);  -- /usr/include/x86_64-linux-gnu/bits/time.h:75

end x86_64_linux_gnu_bits_time_h;
