with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
with x86_64_linux_gnu_bits_time_h;

package x86_64_linux_gnu_bits_resource_h is

   --  unsupported macro: RLIMIT_CPU RLIMIT_CPU
   --  unsupported macro: RLIMIT_FSIZE RLIMIT_FSIZE
   --  unsupported macro: RLIMIT_DATA RLIMIT_DATA
   --  unsupported macro: RLIMIT_STACK RLIMIT_STACK
   --  unsupported macro: RLIMIT_CORE RLIMIT_CORE
   --  unsupported macro: RLIMIT_RSS __RLIMIT_RSS
   --  unsupported macro: RLIMIT_NOFILE RLIMIT_NOFILE
   --  unsupported macro: RLIMIT_OFILE __RLIMIT_OFILE
   --  unsupported macro: RLIMIT_AS RLIMIT_AS
   --  unsupported macro: RLIMIT_NPROC __RLIMIT_NPROC
   --  unsupported macro: RLIMIT_MEMLOCK __RLIMIT_MEMLOCK
   --  unsupported macro: RLIMIT_LOCKS __RLIMIT_LOCKS
   --  unsupported macro: RLIMIT_SIGPENDING __RLIMIT_SIGPENDING
   --  unsupported macro: RLIMIT_MSGQUEUE __RLIMIT_MSGQUEUE
   --  unsupported macro: RLIMIT_NICE __RLIMIT_NICE
   --  unsupported macro: RLIMIT_RTPRIO __RLIMIT_RTPRIO
   --  unsupported macro: RLIMIT_RTTIME __RLIMIT_RTTIME
   --  unsupported macro: RLIMIT_NLIMITS __RLIMIT_NLIMITS
   --  unsupported macro: RLIM_NLIMITS __RLIM_NLIMITS
   --  unsupported macro: RLIM_INFINITY ((unsigned long int)(~0UL))
   --  unsupported macro: RLIM64_INFINITY 0xffffffffffffffffuLL
   --  unsupported macro: RLIM_SAVED_MAX RLIM_INFINITY
   --  unsupported macro: RLIM_SAVED_CUR RLIM_INFINITY
   --  unsupported macro: RUSAGE_SELF RUSAGE_SELF
   --  unsupported macro: RUSAGE_CHILDREN RUSAGE_CHILDREN
   --  unsupported macro: RUSAGE_THREAD RUSAGE_THREAD
   --  unsupported macro: RUSAGE_LWP RUSAGE_THREAD
   --  unsupported macro: PRIO_MIN -20
   --  unsupported macro: PRIO_MAX 20
   --  unsupported macro: PRIO_PROCESS PRIO_PROCESS
   --  unsupported macro: PRIO_PGRP PRIO_PGRP
   --  unsupported macro: PRIO_USER PRIO_USER
   subtype uu_rlimit_resource is unsigned;
   RLIMIT_CPU : constant uu_rlimit_resource := 0;
   RLIMIT_FSIZE : constant uu_rlimit_resource := 1;
   RLIMIT_DATA : constant uu_rlimit_resource := 2;
   RLIMIT_STACK : constant uu_rlimit_resource := 3;
   RLIMIT_CORE : constant uu_rlimit_resource := 4;
   uu_RLIMIT_RSS : constant uu_rlimit_resource := 5;
   RLIMIT_NOFILE : constant uu_rlimit_resource := 7;
   uu_RLIMIT_OFILE : constant uu_rlimit_resource := 7;
   RLIMIT_AS : constant uu_rlimit_resource := 9;
   uu_RLIMIT_NPROC : constant uu_rlimit_resource := 6;
   uu_RLIMIT_MEMLOCK : constant uu_rlimit_resource := 8;
   uu_RLIMIT_LOCKS : constant uu_rlimit_resource := 10;
   uu_RLIMIT_SIGPENDING : constant uu_rlimit_resource := 11;
   uu_RLIMIT_MSGQUEUE : constant uu_rlimit_resource := 12;
   uu_RLIMIT_NICE : constant uu_rlimit_resource := 13;
   uu_RLIMIT_RTPRIO : constant uu_rlimit_resource := 14;
   uu_RLIMIT_RTTIME : constant uu_rlimit_resource := 15;
   uu_RLIMIT_NLIMITS : constant uu_rlimit_resource := 16;
   uu_RLIM_NLIMITS : constant uu_rlimit_resource := 16;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:33

   subtype rlim_t is x86_64_linux_gnu_bits_types_h.uu_rlim_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:133

   subtype rlim64_t is x86_64_linux_gnu_bits_types_h.uu_rlim64_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:138

   type rlimit is record
      rlim_cur : aliased rlim_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:144
      rlim_max : aliased rlim_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:146
   end record;
   pragma Convention (C_Pass_By_Copy, rlimit);  -- /usr/include/x86_64-linux-gnu/bits/resource.h:141

   type rlimit64 is record
      rlim_cur : aliased rlim64_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:153
      rlim_max : aliased rlim64_t;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:155
   end record;
   pragma Convention (C_Pass_By_Copy, rlimit64);  -- /usr/include/x86_64-linux-gnu/bits/resource.h:150

   subtype uu_rusage_who is unsigned;
   RUSAGE_SELF : constant uu_rusage_who := 0;
   RUSAGE_CHILDREN : constant uu_rusage_who := -1;
   RUSAGE_THREAD : constant uu_rusage_who := 1;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:160

   type rusage is record
      ru_utime : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:187
      ru_stime : aliased x86_64_linux_gnu_bits_time_h.timeval;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:189
      ru_maxrss : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:191
      ru_ixrss : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:194
      ru_idrss : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:196
      ru_isrss : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:198
      ru_minflt : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:201
      ru_majflt : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:203
      ru_nswap : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:205
      ru_inblock : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:208
      ru_oublock : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:210
      ru_msgsnd : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:212
      ru_msgrcv : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:214
      ru_nsignals : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:216
      ru_nvcsw : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:220
      ru_nivcsw : aliased long;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:223
   end record;
   pragma Convention (C_Pass_By_Copy, rusage);  -- /usr/include/x86_64-linux-gnu/bits/resource.h:184

   type uu_priority_which is 
     (PRIO_PROCESS,
      PRIO_PGRP,
      PRIO_USER);
   pragma Convention (C, uu_priority_which);  -- /usr/include/x86_64-linux-gnu/bits/resource.h:232

   function prlimit
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_resource : uu_rlimit_resource;
      uu_new_limit : access constant rlimit;
      uu_old_limit : access rlimit) return int;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:248
   pragma Import (C, prlimit, "prlimit");

   function prlimit64
     (uu_pid : x86_64_linux_gnu_bits_types_h.uu_pid_t;
      uu_resource : uu_rlimit_resource;
      uu_new_limit : access constant rlimit64;
      uu_old_limit : access rlimit64) return int;  -- /usr/include/x86_64-linux-gnu/bits/resource.h:262
   pragma Import (C, prlimit64, "prlimit64");

end x86_64_linux_gnu_bits_resource_h;
