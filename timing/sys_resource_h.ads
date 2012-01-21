with Interfaces.C; use Interfaces.C;
with x86_64_linux_gnu_bits_types_h;
limited with x86_64_linux_gnu_bits_resource_h;

package sys_resource_h is

   subtype id_t is x86_64_linux_gnu_bits_types_h.uu_id_t;  -- /usr/include/sys/resource.h:28

   subtype uu_rlimit_resource_t is int;  -- /usr/include/sys/resource.h:43

   subtype uu_rusage_who_t is int;  -- /usr/include/sys/resource.h:44

   subtype uu_priority_which_t is int;  -- /usr/include/sys/resource.h:45

   function getrlimit (uu_resource : uu_rlimit_resource_t; uu_rlimits : access x86_64_linux_gnu_bits_resource_h.rlimit) return int;  -- /usr/include/sys/resource.h:51
   pragma Import (C, getrlimit, "getrlimit");

   function getrlimit64 (uu_resource : uu_rlimit_resource_t; uu_rlimits : access x86_64_linux_gnu_bits_resource_h.rlimit64) return int;  -- /usr/include/sys/resource.h:62
   pragma Import (C, getrlimit64, "getrlimit64");

   function setrlimit (uu_resource : uu_rlimit_resource_t; uu_rlimits : access constant x86_64_linux_gnu_bits_resource_h.rlimit) return int;  -- /usr/include/sys/resource.h:70
   pragma Import (C, setrlimit, "setrlimit");

   function setrlimit64 (uu_resource : uu_rlimit_resource_t; uu_rlimits : access constant x86_64_linux_gnu_bits_resource_h.rlimit64) return int;  -- /usr/include/sys/resource.h:82
   pragma Import (C, setrlimit64, "setrlimit64");

   function getrusage (uu_who : uu_rusage_who_t; uu_usage : access x86_64_linux_gnu_bits_resource_h.rusage) return int;  -- /usr/include/sys/resource.h:88
   pragma Import (C, getrusage, "getrusage");

   function getpriority (uu_which : uu_priority_which_t; uu_who : id_t) return int;  -- /usr/include/sys/resource.h:94
   pragma Import (C, getpriority, "getpriority");

   function setpriority
     (uu_which : uu_priority_which_t;
      uu_who : id_t;
      uu_prio : int) return int;  -- /usr/include/sys/resource.h:98
   pragma Import (C, setpriority, "setpriority");

end sys_resource_h;
