# 1 "scimark/linpack.c"
# 1 "<built-in>"
# 1 "<command line>"
# 1 "scimark/linpack.c"
void error() {
ERROR: goto ERROR;
}
# 51 "scimark/linpack.c"
# 1 "/usr/include/stdio.h" 1 3 4
# 28 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/features.h" 1 3 4
# 319 "/usr/include/features.h" 3 4
# 1 "/usr/include/sys/cdefs.h" 1 3 4
# 320 "/usr/include/features.h" 2 3 4
# 342 "/usr/include/features.h" 3 4
# 1 "/usr/include/gnu/stubs.h" 1 3 4



# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 5 "/usr/include/gnu/stubs.h" 2 3 4


# 1 "/usr/include/gnu/stubs-32.h" 1 3 4
# 8 "/usr/include/gnu/stubs.h" 2 3 4
# 343 "/usr/include/features.h" 2 3 4
# 29 "/usr/include/stdio.h" 2 3 4





# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 214 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 3 4
typedef unsigned int size_t;
# 35 "/usr/include/stdio.h" 2 3 4

# 1 "/usr/include/bits/types.h" 1 3 4
# 28 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/wordsize.h" 1 3 4
# 29 "/usr/include/bits/types.h" 2 3 4


# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 32 "/usr/include/bits/types.h" 2 3 4


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;




__extension__ typedef signed long long int __int64_t;
__extension__ typedef unsigned long long int __uint64_t;







__extension__ typedef long long int __quad_t;
__extension__ typedef unsigned long long int __u_quad_t;
# 134 "/usr/include/bits/types.h" 3 4
# 1 "/usr/include/bits/typesizes.h" 1 3 4
# 135 "/usr/include/bits/types.h" 2 3 4


__extension__ typedef __u_quad_t __dev_t;
__extension__ typedef unsigned int __uid_t;
__extension__ typedef unsigned int __gid_t;
__extension__ typedef unsigned long int __ino_t;
__extension__ typedef __u_quad_t __ino64_t;
__extension__ typedef unsigned int __mode_t;
__extension__ typedef unsigned int __nlink_t;
__extension__ typedef long int __off_t;
__extension__ typedef __quad_t __off64_t;
__extension__ typedef int __pid_t;
__extension__ typedef struct { int __val[2]; } __fsid_t;
__extension__ typedef long int __clock_t;
__extension__ typedef unsigned long int __rlim_t;
__extension__ typedef __u_quad_t __rlim64_t;
__extension__ typedef unsigned int __id_t;
__extension__ typedef long int __time_t;
__extension__ typedef unsigned int __useconds_t;
__extension__ typedef long int __suseconds_t;

__extension__ typedef int __daddr_t;
__extension__ typedef long int __swblk_t;
__extension__ typedef int __key_t;


__extension__ typedef int __clockid_t;


__extension__ typedef void * __timer_t;


__extension__ typedef long int __blksize_t;




__extension__ typedef long int __blkcnt_t;
__extension__ typedef __quad_t __blkcnt64_t;


__extension__ typedef unsigned long int __fsblkcnt_t;
__extension__ typedef __u_quad_t __fsblkcnt64_t;


__extension__ typedef unsigned long int __fsfilcnt_t;
__extension__ typedef __u_quad_t __fsfilcnt64_t;

__extension__ typedef int __ssize_t;



typedef __off64_t __loff_t;
typedef __quad_t *__qaddr_t;
typedef char *__caddr_t;


__extension__ typedef int __intptr_t;


__extension__ typedef unsigned int __socklen_t;
# 37 "/usr/include/stdio.h" 2 3 4









typedef struct _IO_FILE FILE;





# 62 "/usr/include/stdio.h" 3 4
typedef struct _IO_FILE __FILE;
# 72 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/libio.h" 1 3 4
# 32 "/usr/include/libio.h" 3 4
# 1 "/usr/include/_G_config.h" 1 3 4
# 14 "/usr/include/_G_config.h" 3 4
# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 326 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 3 4
typedef long int wchar_t;
# 355 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 3 4
typedef unsigned int wint_t;
# 15 "/usr/include/_G_config.h" 2 3 4
# 24 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4

# 1 "/usr/include/bits/wchar.h" 1 3 4
# 51 "/usr/include/wchar.h" 2 3 4
# 76 "/usr/include/wchar.h" 3 4
typedef struct
{
  int __count;
  union
  {
    wint_t __wch;
    char __wchb[4];
  } __value;
} __mbstate_t;
# 25 "/usr/include/_G_config.h" 2 3 4

typedef struct
{
  __off_t __pos;
  __mbstate_t __state;
} _G_fpos_t;
typedef struct
{
  __off64_t __pos;
  __mbstate_t __state;
} _G_fpos64_t;
# 44 "/usr/include/_G_config.h" 3 4
# 1 "/usr/include/gconv.h" 1 3 4
# 28 "/usr/include/gconv.h" 3 4
# 1 "/usr/include/wchar.h" 1 3 4
# 48 "/usr/include/wchar.h" 3 4
# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 49 "/usr/include/wchar.h" 2 3 4
# 29 "/usr/include/gconv.h" 2 3 4


# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stddef.h" 1 3 4
# 32 "/usr/include/gconv.h" 2 3 4





enum
{
  __GCONV_OK = 0,
  __GCONV_NOCONV,
  __GCONV_NODB,
  __GCONV_NOMEM,

  __GCONV_EMPTY_INPUT,
  __GCONV_FULL_OUTPUT,
  __GCONV_ILLEGAL_INPUT,
  __GCONV_INCOMPLETE_INPUT,

  __GCONV_ILLEGAL_DESCRIPTOR,
  __GCONV_INTERNAL_ERROR
};



enum
{
  __GCONV_IS_LAST = 0x0001,
  __GCONV_IGNORE_ERRORS = 0x0002
};



struct __gconv_step;
struct __gconv_step_data;
struct __gconv_loaded_object;
struct __gconv_trans_data;



typedef int (*__gconv_fct) (struct __gconv_step *, struct __gconv_step_data *,
       __const unsigned char **, __const unsigned char *,
       unsigned char **, size_t *, int, int);


typedef wint_t (*__gconv_btowc_fct) (struct __gconv_step *, unsigned char);


typedef int (*__gconv_init_fct) (struct __gconv_step *);
typedef void (*__gconv_end_fct) (struct __gconv_step *);



typedef int (*__gconv_trans_fct) (struct __gconv_step *,
      struct __gconv_step_data *, void *,
      __const unsigned char *,
      __const unsigned char **,
      __const unsigned char *, unsigned char **,
      size_t *);


typedef int (*__gconv_trans_context_fct) (void *, __const unsigned char *,
       __const unsigned char *,
       unsigned char *, unsigned char *);


typedef int (*__gconv_trans_query_fct) (__const char *, __const char ***,
     size_t *);


typedef int (*__gconv_trans_init_fct) (void **, const char *);
typedef void (*__gconv_trans_end_fct) (void *);

struct __gconv_trans_data
{

  __gconv_trans_fct __trans_fct;
  __gconv_trans_context_fct __trans_context_fct;
  __gconv_trans_end_fct __trans_end_fct;
  void *__data;
  struct __gconv_trans_data *__next;
};



struct __gconv_step
{
  struct __gconv_loaded_object *__shlib_handle;
  __const char *__modname;

  int __counter;

  char *__from_name;
  char *__to_name;

  __gconv_fct __fct;
  __gconv_btowc_fct __btowc_fct;
  __gconv_init_fct __init_fct;
  __gconv_end_fct __end_fct;



  int __min_needed_from;
  int __max_needed_from;
  int __min_needed_to;
  int __max_needed_to;


  int __stateful;

  void *__data;
};



struct __gconv_step_data
{
  unsigned char *__outbuf;
  unsigned char *__outbufend;



  int __flags;



  int __invocation_counter;



  int __internal_use;

  __mbstate_t *__statep;
  __mbstate_t __state;



  struct __gconv_trans_data *__trans;
};



typedef struct __gconv_info
{
  size_t __nsteps;
  struct __gconv_step *__steps;
  __extension__ struct __gconv_step_data __data [];
} *__gconv_t;
# 45 "/usr/include/_G_config.h" 2 3 4
typedef union
{
  struct __gconv_info __cd;
  struct
  {
    struct __gconv_info __cd;
    struct __gconv_step_data __data;
  } __combined;
} _G_iconv_t;

typedef int _G_int16_t __attribute__ ((__mode__ (__HI__)));
typedef int _G_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int _G_uint16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int _G_uint32_t __attribute__ ((__mode__ (__SI__)));
# 33 "/usr/include/libio.h" 2 3 4
# 53 "/usr/include/libio.h" 3 4
# 1 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stdarg.h" 1 3 4
# 43 "/usr/lib/gcc/i386-redhat-linux/4.0.2/include/stdarg.h" 3 4
typedef __builtin_va_list __gnuc_va_list;
# 54 "/usr/include/libio.h" 2 3 4
# 166 "/usr/include/libio.h" 3 4
struct _IO_jump_t; struct _IO_FILE;
# 176 "/usr/include/libio.h" 3 4
typedef void _IO_lock_t;





struct _IO_marker {
  struct _IO_marker *_next;
  struct _IO_FILE *_sbuf;



  int _pos;
# 199 "/usr/include/libio.h" 3 4
};


enum __codecvt_result
{
  __codecvt_ok,
  __codecvt_partial,
  __codecvt_error,
  __codecvt_noconv
};
# 267 "/usr/include/libio.h" 3 4
struct _IO_FILE {
  int _flags;




  char* _IO_read_ptr;
  char* _IO_read_end;
  char* _IO_read_base;
  char* _IO_write_base;
  char* _IO_write_ptr;
  char* _IO_write_end;
  char* _IO_buf_base;
  char* _IO_buf_end;

  char *_IO_save_base;
  char *_IO_backup_base;
  char *_IO_save_end;

  struct _IO_marker *_markers;

  struct _IO_FILE *_chain;

  int _fileno;



  int _flags2;

  __off_t _old_offset;



  unsigned short _cur_column;
  signed char _vtable_offset;
  char _shortbuf[1];



  _IO_lock_t *_lock;
# 315 "/usr/include/libio.h" 3 4
  __off64_t _offset;
# 324 "/usr/include/libio.h" 3 4
  void *__pad1;
  void *__pad2;
  void *__pad3;
  void *__pad4;
  size_t __pad5;

  int _mode;

  char _unused2[15 * sizeof (int) - 4 * sizeof (void *) - sizeof (size_t)];

};


typedef struct _IO_FILE _IO_FILE;


struct _IO_FILE_plus;

extern struct _IO_FILE_plus _IO_2_1_stdin_;
extern struct _IO_FILE_plus _IO_2_1_stdout_;
extern struct _IO_FILE_plus _IO_2_1_stderr_;
# 360 "/usr/include/libio.h" 3 4
typedef __ssize_t __io_read_fn (void *__cookie, char *__buf, size_t __nbytes);







typedef __ssize_t __io_write_fn (void *__cookie, __const char *__buf,
     size_t __n);







typedef int __io_seek_fn (void *__cookie, __off64_t *__pos, int __w);


typedef int __io_close_fn (void *__cookie);
# 412 "/usr/include/libio.h" 3 4
extern int __underflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __uflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern int __overflow (_IO_FILE *, int) __attribute__ ((__nothrow__));
extern wint_t __wunderflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __wuflow (_IO_FILE *) __attribute__ ((__nothrow__));
extern wint_t __woverflow (_IO_FILE *, wint_t) __attribute__ ((__nothrow__));
# 450 "/usr/include/libio.h" 3 4
extern int _IO_getc (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_putc (int __c, _IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_feof (_IO_FILE *__fp) __attribute__ ((__nothrow__));
extern int _IO_ferror (_IO_FILE *__fp) __attribute__ ((__nothrow__));

extern int _IO_peekc_locked (_IO_FILE *__fp) __attribute__ ((__nothrow__));





extern void _IO_flockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern void _IO_funlockfile (_IO_FILE *) __attribute__ ((__nothrow__));
extern int _IO_ftrylockfile (_IO_FILE *) __attribute__ ((__nothrow__));
# 480 "/usr/include/libio.h" 3 4
extern int _IO_vfscanf (_IO_FILE * __restrict, const char * __restrict,
   __gnuc_va_list, int *__restrict);
extern int _IO_vfprintf (_IO_FILE *__restrict, const char *__restrict,
    __gnuc_va_list);
extern __ssize_t _IO_padn (_IO_FILE *, int, __ssize_t) __attribute__ ((__nothrow__));
extern size_t _IO_sgetn (_IO_FILE *, void *, size_t) __attribute__ ((__nothrow__));

extern __off64_t _IO_seekoff (_IO_FILE *, __off64_t, int, int) __attribute__ ((__nothrow__));
extern __off64_t _IO_seekpos (_IO_FILE *, __off64_t, int) __attribute__ ((__nothrow__));

extern void _IO_free_backup_area (_IO_FILE *) __attribute__ ((__nothrow__));
# 73 "/usr/include/stdio.h" 2 3 4
# 86 "/usr/include/stdio.h" 3 4


typedef _G_fpos_t fpos_t;




# 138 "/usr/include/stdio.h" 3 4
# 1 "/usr/include/bits/stdio_lim.h" 1 3 4
# 139 "/usr/include/stdio.h" 2 3 4



extern struct _IO_FILE *stdin;
extern struct _IO_FILE *stdout;
extern struct _IO_FILE *stderr;









extern int remove (__const char *__filename) __attribute__ ((__nothrow__));

extern int rename (__const char *__old, __const char *__new) __attribute__ ((__nothrow__));









extern FILE *tmpfile (void);
# 180 "/usr/include/stdio.h" 3 4
extern char *tmpnam (char *__s) __attribute__ ((__nothrow__));





extern char *tmpnam_r (char *__s) __attribute__ ((__nothrow__));
# 198 "/usr/include/stdio.h" 3 4
extern char *tempnam (__const char *__dir, __const char *__pfx)
     __attribute__ ((__nothrow__)) __attribute__ ((__malloc__));








extern int fclose (FILE *__stream);




extern int fflush (FILE *__stream);

# 223 "/usr/include/stdio.h" 3 4
extern int fflush_unlocked (FILE *__stream);
# 237 "/usr/include/stdio.h" 3 4






extern FILE *fopen (__const char *__restrict __filename,
      __const char *__restrict __modes);




extern FILE *freopen (__const char *__restrict __filename,
        __const char *__restrict __modes,
        FILE *__restrict __stream);
# 264 "/usr/include/stdio.h" 3 4

# 275 "/usr/include/stdio.h" 3 4
extern FILE *fdopen (int __fd, __const char *__modes) __attribute__ ((__nothrow__));
# 296 "/usr/include/stdio.h" 3 4



extern void setbuf (FILE *__restrict __stream, char *__restrict __buf) __attribute__ ((__nothrow__));



extern int setvbuf (FILE *__restrict __stream, char *__restrict __buf,
      int __modes, size_t __n) __attribute__ ((__nothrow__));





extern void setbuffer (FILE *__restrict __stream, char *__restrict __buf,
         size_t __size) __attribute__ ((__nothrow__));


extern void setlinebuf (FILE *__stream) __attribute__ ((__nothrow__));








extern int fprintf (FILE *__restrict __stream,
      __const char *__restrict __format, ...);




extern int printf (__const char *__restrict __format, ...);

extern int sprintf (char *__restrict __s,
      __const char *__restrict __format, ...) __attribute__ ((__nothrow__));





extern int vfprintf (FILE *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg);




extern int vprintf (__const char *__restrict __format, __gnuc_va_list __arg);

extern int vsprintf (char *__restrict __s, __const char *__restrict __format,
       __gnuc_va_list __arg) __attribute__ ((__nothrow__));





extern int snprintf (char *__restrict __s, size_t __maxlen,
       __const char *__restrict __format, ...)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 4)));

extern int vsnprintf (char *__restrict __s, size_t __maxlen,
        __const char *__restrict __format, __gnuc_va_list __arg)
     __attribute__ ((__nothrow__)) __attribute__ ((__format__ (__printf__, 3, 0)));

# 390 "/usr/include/stdio.h" 3 4





extern int fscanf (FILE *__restrict __stream,
     __const char *__restrict __format, ...) ;




extern int scanf (__const char *__restrict __format, ...) ;

extern int sscanf (__const char *__restrict __s,
     __const char *__restrict __format, ...) __attribute__ ((__nothrow__));

# 432 "/usr/include/stdio.h" 3 4





extern int fgetc (FILE *__stream);
extern int getc (FILE *__stream);





extern int getchar (void);

# 456 "/usr/include/stdio.h" 3 4
extern int getc_unlocked (FILE *__stream);
extern int getchar_unlocked (void);
# 467 "/usr/include/stdio.h" 3 4
extern int fgetc_unlocked (FILE *__stream);











extern int fputc (int __c, FILE *__stream);
extern int putc (int __c, FILE *__stream);





extern int putchar (int __c);

# 500 "/usr/include/stdio.h" 3 4
extern int fputc_unlocked (int __c, FILE *__stream);







extern int putc_unlocked (int __c, FILE *__stream);
extern int putchar_unlocked (int __c);






extern int getw (FILE *__stream);


extern int putw (int __w, FILE *__stream);








extern char *fgets (char *__restrict __s, int __n, FILE *__restrict __stream)
     ;






extern char *gets (char *__s) ;

# 581 "/usr/include/stdio.h" 3 4





extern int fputs (__const char *__restrict __s, FILE *__restrict __stream);





extern int puts (__const char *__s);






extern int ungetc (int __c, FILE *__stream);






extern size_t fread (void *__restrict __ptr, size_t __size,
       size_t __n, FILE *__restrict __stream) ;




extern size_t fwrite (__const void *__restrict __ptr, size_t __size,
        size_t __n, FILE *__restrict __s) ;

# 634 "/usr/include/stdio.h" 3 4
extern size_t fread_unlocked (void *__restrict __ptr, size_t __size,
         size_t __n, FILE *__restrict __stream) ;
extern size_t fwrite_unlocked (__const void *__restrict __ptr, size_t __size,
          size_t __n, FILE *__restrict __stream) ;








extern int fseek (FILE *__stream, long int __off, int __whence);




extern long int ftell (FILE *__stream) ;




extern void rewind (FILE *__stream);

# 670 "/usr/include/stdio.h" 3 4
extern int fseeko (FILE *__stream, __off_t __off, int __whence);




extern __off_t ftello (FILE *__stream) ;
# 689 "/usr/include/stdio.h" 3 4






extern int fgetpos (FILE *__restrict __stream, fpos_t *__restrict __pos);




extern int fsetpos (FILE *__stream, __const fpos_t *__pos);
# 712 "/usr/include/stdio.h" 3 4

# 721 "/usr/include/stdio.h" 3 4


extern void clearerr (FILE *__stream) __attribute__ ((__nothrow__));

extern int feof (FILE *__stream) __attribute__ ((__nothrow__)) ;

extern int ferror (FILE *__stream) __attribute__ ((__nothrow__)) ;




extern void clearerr_unlocked (FILE *__stream) __attribute__ ((__nothrow__));
extern int feof_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
extern int ferror_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;








extern void perror (__const char *__s);






# 1 "/usr/include/bits/sys_errlist.h" 1 3 4
# 27 "/usr/include/bits/sys_errlist.h" 3 4
extern int sys_nerr;
extern __const char *__const sys_errlist[];
# 751 "/usr/include/stdio.h" 2 3 4




extern int fileno (FILE *__stream) __attribute__ ((__nothrow__)) ;




extern int fileno_unlocked (FILE *__stream) __attribute__ ((__nothrow__)) ;
# 770 "/usr/include/stdio.h" 3 4
extern FILE *popen (__const char *__command, __const char *__modes) ;





extern int pclose (FILE *__stream);





extern char *ctermid (char *__s) __attribute__ ((__nothrow__));
# 810 "/usr/include/stdio.h" 3 4
extern void flockfile (FILE *__stream) __attribute__ ((__nothrow__));



extern int ftrylockfile (FILE *__stream) __attribute__ ((__nothrow__)) ;


extern void funlockfile (FILE *__stream) __attribute__ ((__nothrow__));
# 837 "/usr/include/stdio.h" 3 4

# 52 "scimark/linpack.c" 2
# 1 "/usr/include/math.h" 1 3 4
# 29 "/usr/include/math.h" 3 4




# 1 "/usr/include/bits/huge_val.h" 1 3 4
# 34 "/usr/include/math.h" 2 3 4
# 46 "/usr/include/math.h" 3 4
# 1 "/usr/include/bits/mathdef.h" 1 3 4
# 47 "/usr/include/math.h" 2 3 4
# 70 "/usr/include/math.h" 3 4
# 1 "/usr/include/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/bits/mathcalls.h" 3 4


extern double acos (double __x) __attribute__ ((__nothrow__)); extern double __acos (double __x) __attribute__ ((__nothrow__));

extern double asin (double __x) __attribute__ ((__nothrow__)); extern double __asin (double __x) __attribute__ ((__nothrow__));

extern double atan (double __x) __attribute__ ((__nothrow__)); extern double __atan (double __x) __attribute__ ((__nothrow__));

extern double atan2 (double __y, double __x) __attribute__ ((__nothrow__)); extern double __atan2 (double __y, double __x) __attribute__ ((__nothrow__));


extern double cos (double __x) __attribute__ ((__nothrow__)); extern double __cos (double __x) __attribute__ ((__nothrow__));

extern double sin (double __x) __attribute__ ((__nothrow__)); extern double __sin (double __x) __attribute__ ((__nothrow__));

extern double tan (double __x) __attribute__ ((__nothrow__)); extern double __tan (double __x) __attribute__ ((__nothrow__));




extern double cosh (double __x) __attribute__ ((__nothrow__)); extern double __cosh (double __x) __attribute__ ((__nothrow__));

extern double sinh (double __x) __attribute__ ((__nothrow__)); extern double __sinh (double __x) __attribute__ ((__nothrow__));

extern double tanh (double __x) __attribute__ ((__nothrow__)); extern double __tanh (double __x) __attribute__ ((__nothrow__));

# 87 "/usr/include/bits/mathcalls.h" 3 4


extern double acosh (double __x) __attribute__ ((__nothrow__)); extern double __acosh (double __x) __attribute__ ((__nothrow__));

extern double asinh (double __x) __attribute__ ((__nothrow__)); extern double __asinh (double __x) __attribute__ ((__nothrow__));

extern double atanh (double __x) __attribute__ ((__nothrow__)); extern double __atanh (double __x) __attribute__ ((__nothrow__));







extern double exp (double __x) __attribute__ ((__nothrow__)); extern double __exp (double __x) __attribute__ ((__nothrow__));


extern double frexp (double __x, int *__exponent) __attribute__ ((__nothrow__)); extern double __frexp (double __x, int *__exponent) __attribute__ ((__nothrow__));


extern double ldexp (double __x, int __exponent) __attribute__ ((__nothrow__)); extern double __ldexp (double __x, int __exponent) __attribute__ ((__nothrow__));


extern double log (double __x) __attribute__ ((__nothrow__)); extern double __log (double __x) __attribute__ ((__nothrow__));


extern double log10 (double __x) __attribute__ ((__nothrow__)); extern double __log10 (double __x) __attribute__ ((__nothrow__));


extern double modf (double __x, double *__iptr) __attribute__ ((__nothrow__)); extern double __modf (double __x, double *__iptr) __attribute__ ((__nothrow__));

# 127 "/usr/include/bits/mathcalls.h" 3 4


extern double expm1 (double __x) __attribute__ ((__nothrow__)); extern double __expm1 (double __x) __attribute__ ((__nothrow__));


extern double log1p (double __x) __attribute__ ((__nothrow__)); extern double __log1p (double __x) __attribute__ ((__nothrow__));


extern double logb (double __x) __attribute__ ((__nothrow__)); extern double __logb (double __x) __attribute__ ((__nothrow__));

# 152 "/usr/include/bits/mathcalls.h" 3 4


extern double pow (double __x, double __y) __attribute__ ((__nothrow__)); extern double __pow (double __x, double __y) __attribute__ ((__nothrow__));


extern double sqrt (double __x) __attribute__ ((__nothrow__)); extern double __sqrt (double __x) __attribute__ ((__nothrow__));





extern double hypot (double __x, double __y) __attribute__ ((__nothrow__)); extern double __hypot (double __x, double __y) __attribute__ ((__nothrow__));






extern double cbrt (double __x) __attribute__ ((__nothrow__)); extern double __cbrt (double __x) __attribute__ ((__nothrow__));








extern double ceil (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __ceil (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern double fabs (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __fabs (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern double floor (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __floor (double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern double fmod (double __x, double __y) __attribute__ ((__nothrow__)); extern double __fmod (double __x, double __y) __attribute__ ((__nothrow__));




extern int __isinf (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int __finite (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern int isinf (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int finite (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern double drem (double __x, double __y) __attribute__ ((__nothrow__)); extern double __drem (double __x, double __y) __attribute__ ((__nothrow__));



extern double significand (double __x) __attribute__ ((__nothrow__)); extern double __significand (double __x) __attribute__ ((__nothrow__));





extern double copysign (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __copysign (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));

# 231 "/usr/include/bits/mathcalls.h" 3 4
extern int __isnan (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));



extern int isnan (double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern double j0 (double) __attribute__ ((__nothrow__)); extern double __j0 (double) __attribute__ ((__nothrow__));
extern double j1 (double) __attribute__ ((__nothrow__)); extern double __j1 (double) __attribute__ ((__nothrow__));
extern double jn (int, double) __attribute__ ((__nothrow__)); extern double __jn (int, double) __attribute__ ((__nothrow__));
extern double y0 (double) __attribute__ ((__nothrow__)); extern double __y0 (double) __attribute__ ((__nothrow__));
extern double y1 (double) __attribute__ ((__nothrow__)); extern double __y1 (double) __attribute__ ((__nothrow__));
extern double yn (int, double) __attribute__ ((__nothrow__)); extern double __yn (int, double) __attribute__ ((__nothrow__));






extern double erf (double) __attribute__ ((__nothrow__)); extern double __erf (double) __attribute__ ((__nothrow__));
extern double erfc (double) __attribute__ ((__nothrow__)); extern double __erfc (double) __attribute__ ((__nothrow__));
extern double lgamma (double) __attribute__ ((__nothrow__)); extern double __lgamma (double) __attribute__ ((__nothrow__));

# 265 "/usr/include/bits/mathcalls.h" 3 4
extern double gamma (double) __attribute__ ((__nothrow__)); extern double __gamma (double) __attribute__ ((__nothrow__));






extern double lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__)); extern double __lgamma_r (double, int *__signgamp) __attribute__ ((__nothrow__));







extern double rint (double __x) __attribute__ ((__nothrow__)); extern double __rint (double __x) __attribute__ ((__nothrow__));


extern double nextafter (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern double __nextafter (double __x, double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern double remainder (double __x, double __y) __attribute__ ((__nothrow__)); extern double __remainder (double __x, double __y) __attribute__ ((__nothrow__));



extern double scalbn (double __x, int __n) __attribute__ ((__nothrow__)); extern double __scalbn (double __x, int __n) __attribute__ ((__nothrow__));



extern int ilogb (double __x) __attribute__ ((__nothrow__)); extern int __ilogb (double __x) __attribute__ ((__nothrow__));
# 359 "/usr/include/bits/mathcalls.h" 3 4





extern double scalb (double __x, double __n) __attribute__ ((__nothrow__)); extern double __scalb (double __x, double __n) __attribute__ ((__nothrow__));
# 71 "/usr/include/math.h" 2 3 4
# 93 "/usr/include/math.h" 3 4
# 1 "/usr/include/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/bits/mathcalls.h" 3 4


extern float acosf (float __x) __attribute__ ((__nothrow__)); extern float __acosf (float __x) __attribute__ ((__nothrow__));

extern float asinf (float __x) __attribute__ ((__nothrow__)); extern float __asinf (float __x) __attribute__ ((__nothrow__));

extern float atanf (float __x) __attribute__ ((__nothrow__)); extern float __atanf (float __x) __attribute__ ((__nothrow__));

extern float atan2f (float __y, float __x) __attribute__ ((__nothrow__)); extern float __atan2f (float __y, float __x) __attribute__ ((__nothrow__));


extern float cosf (float __x) __attribute__ ((__nothrow__)); extern float __cosf (float __x) __attribute__ ((__nothrow__));

extern float sinf (float __x) __attribute__ ((__nothrow__)); extern float __sinf (float __x) __attribute__ ((__nothrow__));

extern float tanf (float __x) __attribute__ ((__nothrow__)); extern float __tanf (float __x) __attribute__ ((__nothrow__));




extern float coshf (float __x) __attribute__ ((__nothrow__)); extern float __coshf (float __x) __attribute__ ((__nothrow__));

extern float sinhf (float __x) __attribute__ ((__nothrow__)); extern float __sinhf (float __x) __attribute__ ((__nothrow__));

extern float tanhf (float __x) __attribute__ ((__nothrow__)); extern float __tanhf (float __x) __attribute__ ((__nothrow__));

# 87 "/usr/include/bits/mathcalls.h" 3 4


extern float acoshf (float __x) __attribute__ ((__nothrow__)); extern float __acoshf (float __x) __attribute__ ((__nothrow__));

extern float asinhf (float __x) __attribute__ ((__nothrow__)); extern float __asinhf (float __x) __attribute__ ((__nothrow__));

extern float atanhf (float __x) __attribute__ ((__nothrow__)); extern float __atanhf (float __x) __attribute__ ((__nothrow__));







extern float expf (float __x) __attribute__ ((__nothrow__)); extern float __expf (float __x) __attribute__ ((__nothrow__));


extern float frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__)); extern float __frexpf (float __x, int *__exponent) __attribute__ ((__nothrow__));


extern float ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__)); extern float __ldexpf (float __x, int __exponent) __attribute__ ((__nothrow__));


extern float logf (float __x) __attribute__ ((__nothrow__)); extern float __logf (float __x) __attribute__ ((__nothrow__));


extern float log10f (float __x) __attribute__ ((__nothrow__)); extern float __log10f (float __x) __attribute__ ((__nothrow__));


extern float modff (float __x, float *__iptr) __attribute__ ((__nothrow__)); extern float __modff (float __x, float *__iptr) __attribute__ ((__nothrow__));

# 127 "/usr/include/bits/mathcalls.h" 3 4


extern float expm1f (float __x) __attribute__ ((__nothrow__)); extern float __expm1f (float __x) __attribute__ ((__nothrow__));


extern float log1pf (float __x) __attribute__ ((__nothrow__)); extern float __log1pf (float __x) __attribute__ ((__nothrow__));


extern float logbf (float __x) __attribute__ ((__nothrow__)); extern float __logbf (float __x) __attribute__ ((__nothrow__));

# 152 "/usr/include/bits/mathcalls.h" 3 4


extern float powf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __powf (float __x, float __y) __attribute__ ((__nothrow__));


extern float sqrtf (float __x) __attribute__ ((__nothrow__)); extern float __sqrtf (float __x) __attribute__ ((__nothrow__));





extern float hypotf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __hypotf (float __x, float __y) __attribute__ ((__nothrow__));






extern float cbrtf (float __x) __attribute__ ((__nothrow__)); extern float __cbrtf (float __x) __attribute__ ((__nothrow__));








extern float ceilf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __ceilf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern float fabsf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __fabsf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern float floorf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __floorf (float __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern float fmodf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __fmodf (float __x, float __y) __attribute__ ((__nothrow__));




extern int __isinff (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int __finitef (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern int isinff (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int finitef (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern float dremf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __dremf (float __x, float __y) __attribute__ ((__nothrow__));



extern float significandf (float __x) __attribute__ ((__nothrow__)); extern float __significandf (float __x) __attribute__ ((__nothrow__));





extern float copysignf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __copysignf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));

# 231 "/usr/include/bits/mathcalls.h" 3 4
extern int __isnanf (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));



extern int isnanf (float __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern float j0f (float) __attribute__ ((__nothrow__)); extern float __j0f (float) __attribute__ ((__nothrow__));
extern float j1f (float) __attribute__ ((__nothrow__)); extern float __j1f (float) __attribute__ ((__nothrow__));
extern float jnf (int, float) __attribute__ ((__nothrow__)); extern float __jnf (int, float) __attribute__ ((__nothrow__));
extern float y0f (float) __attribute__ ((__nothrow__)); extern float __y0f (float) __attribute__ ((__nothrow__));
extern float y1f (float) __attribute__ ((__nothrow__)); extern float __y1f (float) __attribute__ ((__nothrow__));
extern float ynf (int, float) __attribute__ ((__nothrow__)); extern float __ynf (int, float) __attribute__ ((__nothrow__));






extern float erff (float) __attribute__ ((__nothrow__)); extern float __erff (float) __attribute__ ((__nothrow__));
extern float erfcf (float) __attribute__ ((__nothrow__)); extern float __erfcf (float) __attribute__ ((__nothrow__));
extern float lgammaf (float) __attribute__ ((__nothrow__)); extern float __lgammaf (float) __attribute__ ((__nothrow__));

# 265 "/usr/include/bits/mathcalls.h" 3 4
extern float gammaf (float) __attribute__ ((__nothrow__)); extern float __gammaf (float) __attribute__ ((__nothrow__));






extern float lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__)); extern float __lgammaf_r (float, int *__signgamp) __attribute__ ((__nothrow__));







extern float rintf (float __x) __attribute__ ((__nothrow__)); extern float __rintf (float __x) __attribute__ ((__nothrow__));


extern float nextafterf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern float __nextafterf (float __x, float __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern float remainderf (float __x, float __y) __attribute__ ((__nothrow__)); extern float __remainderf (float __x, float __y) __attribute__ ((__nothrow__));



extern float scalbnf (float __x, int __n) __attribute__ ((__nothrow__)); extern float __scalbnf (float __x, int __n) __attribute__ ((__nothrow__));



extern int ilogbf (float __x) __attribute__ ((__nothrow__)); extern int __ilogbf (float __x) __attribute__ ((__nothrow__));
# 359 "/usr/include/bits/mathcalls.h" 3 4





extern float scalbf (float __x, float __n) __attribute__ ((__nothrow__)); extern float __scalbf (float __x, float __n) __attribute__ ((__nothrow__));
# 94 "/usr/include/math.h" 2 3 4
# 114 "/usr/include/math.h" 3 4
# 1 "/usr/include/bits/mathcalls.h" 1 3 4
# 53 "/usr/include/bits/mathcalls.h" 3 4


extern long double acosl (long double __x) __attribute__ ((__nothrow__)); extern long double __acosl (long double __x) __attribute__ ((__nothrow__));

extern long double asinl (long double __x) __attribute__ ((__nothrow__)); extern long double __asinl (long double __x) __attribute__ ((__nothrow__));

extern long double atanl (long double __x) __attribute__ ((__nothrow__)); extern long double __atanl (long double __x) __attribute__ ((__nothrow__));

extern long double atan2l (long double __y, long double __x) __attribute__ ((__nothrow__)); extern long double __atan2l (long double __y, long double __x) __attribute__ ((__nothrow__));


extern long double cosl (long double __x) __attribute__ ((__nothrow__)); extern long double __cosl (long double __x) __attribute__ ((__nothrow__));

extern long double sinl (long double __x) __attribute__ ((__nothrow__)); extern long double __sinl (long double __x) __attribute__ ((__nothrow__));

extern long double tanl (long double __x) __attribute__ ((__nothrow__)); extern long double __tanl (long double __x) __attribute__ ((__nothrow__));




extern long double coshl (long double __x) __attribute__ ((__nothrow__)); extern long double __coshl (long double __x) __attribute__ ((__nothrow__));

extern long double sinhl (long double __x) __attribute__ ((__nothrow__)); extern long double __sinhl (long double __x) __attribute__ ((__nothrow__));

extern long double tanhl (long double __x) __attribute__ ((__nothrow__)); extern long double __tanhl (long double __x) __attribute__ ((__nothrow__));

# 87 "/usr/include/bits/mathcalls.h" 3 4


extern long double acoshl (long double __x) __attribute__ ((__nothrow__)); extern long double __acoshl (long double __x) __attribute__ ((__nothrow__));

extern long double asinhl (long double __x) __attribute__ ((__nothrow__)); extern long double __asinhl (long double __x) __attribute__ ((__nothrow__));

extern long double atanhl (long double __x) __attribute__ ((__nothrow__)); extern long double __atanhl (long double __x) __attribute__ ((__nothrow__));







extern long double expl (long double __x) __attribute__ ((__nothrow__)); extern long double __expl (long double __x) __attribute__ ((__nothrow__));


extern long double frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__)); extern long double __frexpl (long double __x, int *__exponent) __attribute__ ((__nothrow__));


extern long double ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__)); extern long double __ldexpl (long double __x, int __exponent) __attribute__ ((__nothrow__));


extern long double logl (long double __x) __attribute__ ((__nothrow__)); extern long double __logl (long double __x) __attribute__ ((__nothrow__));


extern long double log10l (long double __x) __attribute__ ((__nothrow__)); extern long double __log10l (long double __x) __attribute__ ((__nothrow__));


extern long double modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__)); extern long double __modfl (long double __x, long double *__iptr) __attribute__ ((__nothrow__));

# 127 "/usr/include/bits/mathcalls.h" 3 4


extern long double expm1l (long double __x) __attribute__ ((__nothrow__)); extern long double __expm1l (long double __x) __attribute__ ((__nothrow__));


extern long double log1pl (long double __x) __attribute__ ((__nothrow__)); extern long double __log1pl (long double __x) __attribute__ ((__nothrow__));


extern long double logbl (long double __x) __attribute__ ((__nothrow__)); extern long double __logbl (long double __x) __attribute__ ((__nothrow__));

# 152 "/usr/include/bits/mathcalls.h" 3 4


extern long double powl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __powl (long double __x, long double __y) __attribute__ ((__nothrow__));


extern long double sqrtl (long double __x) __attribute__ ((__nothrow__)); extern long double __sqrtl (long double __x) __attribute__ ((__nothrow__));





extern long double hypotl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __hypotl (long double __x, long double __y) __attribute__ ((__nothrow__));






extern long double cbrtl (long double __x) __attribute__ ((__nothrow__)); extern long double __cbrtl (long double __x) __attribute__ ((__nothrow__));








extern long double ceill (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __ceill (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern long double fabsl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __fabsl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern long double floorl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __floorl (long double __x) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern long double fmodl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __fmodl (long double __x, long double __y) __attribute__ ((__nothrow__));




extern int __isinfl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int __finitel (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern int isinfl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern int finitel (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern long double dreml (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __dreml (long double __x, long double __y) __attribute__ ((__nothrow__));



extern long double significandl (long double __x) __attribute__ ((__nothrow__)); extern long double __significandl (long double __x) __attribute__ ((__nothrow__));





extern long double copysignl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __copysignl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));

# 231 "/usr/include/bits/mathcalls.h" 3 4
extern int __isnanl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));



extern int isnanl (long double __value) __attribute__ ((__nothrow__)) __attribute__ ((__const__));


extern long double j0l (long double) __attribute__ ((__nothrow__)); extern long double __j0l (long double) __attribute__ ((__nothrow__));
extern long double j1l (long double) __attribute__ ((__nothrow__)); extern long double __j1l (long double) __attribute__ ((__nothrow__));
extern long double jnl (int, long double) __attribute__ ((__nothrow__)); extern long double __jnl (int, long double) __attribute__ ((__nothrow__));
extern long double y0l (long double) __attribute__ ((__nothrow__)); extern long double __y0l (long double) __attribute__ ((__nothrow__));
extern long double y1l (long double) __attribute__ ((__nothrow__)); extern long double __y1l (long double) __attribute__ ((__nothrow__));
extern long double ynl (int, long double) __attribute__ ((__nothrow__)); extern long double __ynl (int, long double) __attribute__ ((__nothrow__));






extern long double erfl (long double) __attribute__ ((__nothrow__)); extern long double __erfl (long double) __attribute__ ((__nothrow__));
extern long double erfcl (long double) __attribute__ ((__nothrow__)); extern long double __erfcl (long double) __attribute__ ((__nothrow__));
extern long double lgammal (long double) __attribute__ ((__nothrow__)); extern long double __lgammal (long double) __attribute__ ((__nothrow__));

# 265 "/usr/include/bits/mathcalls.h" 3 4
extern long double gammal (long double) __attribute__ ((__nothrow__)); extern long double __gammal (long double) __attribute__ ((__nothrow__));






extern long double lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__)); extern long double __lgammal_r (long double, int *__signgamp) __attribute__ ((__nothrow__));







extern long double rintl (long double __x) __attribute__ ((__nothrow__)); extern long double __rintl (long double __x) __attribute__ ((__nothrow__));


extern long double nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__)); extern long double __nextafterl (long double __x, long double __y) __attribute__ ((__nothrow__)) __attribute__ ((__const__));





extern long double remainderl (long double __x, long double __y) __attribute__ ((__nothrow__)); extern long double __remainderl (long double __x, long double __y) __attribute__ ((__nothrow__));



extern long double scalbnl (long double __x, int __n) __attribute__ ((__nothrow__)); extern long double __scalbnl (long double __x, int __n) __attribute__ ((__nothrow__));



extern int ilogbl (long double __x) __attribute__ ((__nothrow__)); extern int __ilogbl (long double __x) __attribute__ ((__nothrow__));
# 359 "/usr/include/bits/mathcalls.h" 3 4





extern long double scalbl (long double __x, long double __n) __attribute__ ((__nothrow__)); extern long double __scalbl (long double __x, long double __n) __attribute__ ((__nothrow__));
# 115 "/usr/include/math.h" 2 3 4
# 130 "/usr/include/math.h" 3 4
extern int signgam;
# 257 "/usr/include/math.h" 3 4
typedef enum
{
  _IEEE_ = -1,
  _SVID_,
  _XOPEN_,
  _POSIX_,
  _ISOC_
} _LIB_VERSION_TYPE;




extern _LIB_VERSION_TYPE _LIB_VERSION;
# 282 "/usr/include/math.h" 3 4
struct exception

  {
    int type;
    char *name;
    double arg1;
    double arg2;
    double retval;
  };




extern int matherr (struct exception *__exc);
# 438 "/usr/include/math.h" 3 4

# 53 "scimark/linpack.c" 2

static float time[9][9];
int time_dim1=9;
int time_dim2=9;
int aa_dim1=200;
int aa_dim2=200;
int a_dim1=200;
int a_dim2=201;
int b_dim=200;
int x_dim=200;
int ipvt_dim=200;

main ()
{
 static float aa[200][200],a[200][201],b[200],x[200];
 float cray,ops,total,norma,normx;
 float resid,residn,eps,t1,tm,tm2;
 float epslon(),second(),kf;
 static int ipvt[200],n,i,ntimes,info,lda,ldaa,kflops;

 lda = 201;
 ldaa = 200;
 cray = .056;
 n = 100;

 fprintf(stdout,"Rolled ");fprintf(stdout,"Single ");fprintf(stdout,"Precision Linpack\n\n");
 fprintf(stderr,"Rolled ");fprintf(stderr,"Single ");fprintf(stderr,"Precision Linpack\n\n");

        ops = (2.0e0*(n*n*n))/3.0 + 2.0*(n*n);

        matgen(a,lda,n,b,&norma);
        t1 = second();
        dgefa(a,lda,n,ipvt,&info);
if (0<0 || 0>=time_dim1) { error(); };
if (0<0 || 0>=time_dim2) { error(); };
        time[0][0] = second() - t1;
        t1 = second();
        dgesl(a,lda,n,ipvt,b,0);
if (1<0 || 1>=time_dim1) { error(); };
if (0<0 || 0>=time_dim2) { error(); };
        time[1][0] = second() - t1;
        total = time[0][0] + time[1][0];



        for (i = 0; i < n; i++) {
if (i<0 || i>=b_dim) { error(); };
if (i<0 || i>=x_dim) { error(); };
             x[i] = b[i];
 }
        matgen(a,lda,n,b,&norma);
        for (i = 0; i < n; i++) {
if (i<0 || i>=b_dim) { error(); };
if (i<0 || i>=b_dim) { error(); };
             b[i] = -b[i];
 }
        dmxpy(n,b,n,lda,x,a);
        resid = 0.0;
        normx = 0.0;
        for (i = 0; i < n; i++) {
if (i<0 || i>=b_dim) { error(); };
             resid = (resid > fabs((double)b[i]))
   ? resid : fabs((double)b[i]);
if (i<0 || i>=x_dim) { error(); };
             normx = (normx > fabs((double)x[i]))
   ? normx : fabs((double)x[i]);
 }
        eps = epslon((float)1.0);
        residn = resid/( n*norma*normx*eps );

    printf("     norm. resid      resid           machep");
        printf("         x[0]-1        x[n-1]-1\n");
if (0<0 || 0>=x_dim) { error(); };
if (n-1<0 || n-1>=x_dim) { error(); };
 printf("  %8.1f      %16.8e%16.8e%16.8e%16.8e\n",
        (double)residn, (double)resid, (double)eps,
               (double)x[0]-1, (double)x[n-1]-1);

    fprintf(stderr,"    times are reported for matrices of order %5d\n",n);
 fprintf(stderr,"      dgefa      dgesl      total       kflops     unit");
 fprintf(stderr,"      ratio\n");

if (2<0 || 2>=time_dim1) { error(); };
if (0<0 || 0>=time_dim2) { error(); };
if (3<0 || 3>=time_dim1) { error(); };
if (4<0 || 4>=time_dim1) { error(); };
if (5<0 || 5>=time_dim1) { error(); };
        time[2][0] = total;
        time[3][0] = ops/(1.0e3*total);
        time[4][0] = 2.0e3/time[3][0];
        time[5][0] = total/cray;

    fprintf(stderr," times for array with leading dimension of%5d\n",lda);
 print_time(0);

        matgen(a,lda,n,b,&norma);
        t1 = second();
        dgefa(a,lda,n,ipvt,&info);
        time[0][1] = second() - t1;
        t1 = second();
        dgesl(a,lda,n,ipvt,b,0);
        time[1][1] = second() - t1;
        total = time[0][1] + time[1][1];
        time[2][1] = total;
        time[3][1] = ops/(1.0e3*total);
        time[4][1] = 2.0e3/time[3][1];
        time[5][1] = total/cray;

        matgen(a,lda,n,b,&norma);
        t1 = second();
        dgefa(a,lda,n,ipvt,&info);
        time[0][2] = second() - t1;
        t1 = second();
        dgesl(a,lda,n,ipvt,b,0);
        time[1][2] = second() - t1;
        total = time[0][2] + time[1][2];
        time[2][2] = total;
        time[3][2] = ops/(1.0e3*total);
        time[4][2] = 2.0e3/time[3][2];
        time[5][2] = total/cray;

        ntimes = 10;
        tm2 = 0.0;
        t1 = second();

 for (i = 0; i < ntimes; i++) {
             tm = second();
  matgen(a,lda,n,b,&norma);
  tm2 = tm2 + second() - tm;
  dgefa(a,lda,n,ipvt,&info);
 }

        time[0][3] = (second() - t1 - tm2)/ntimes;
        t1 = second();

 for (i = 0; i < ntimes; i++) {
             dgesl(a,lda,n,ipvt,b,0);
 }

        time[1][3] = (second() - t1)/ntimes;
        total = time[0][3] + time[1][3];
        time[2][3] = total;
        time[3][3] = ops/(1.0e3*total);
        time[4][3] = 2.0e3/time[3][3];
        time[5][3] = total/cray;

 print_time(1);
 print_time(2);
 print_time(3);

        matgen(aa,ldaa,n,b,&norma);
        t1 = second();
        dgefa(aa,ldaa,n,ipvt,&info);
        time[0][4] = second() - t1;
        t1 = second();
        dgesl(aa,ldaa,n,ipvt,b,0);
        time[1][4] = second() - t1;
        total = time[0][4] + time[1][4];
        time[2][4] = total;
        time[3][4] = ops/(1.0e3*total);
        time[4][4] = 2.0e3/time[3][4];
        time[5][4] = total/cray;

        matgen(aa,ldaa,n,b,&norma);
        t1 = second();
        dgefa(aa,ldaa,n,ipvt,&info);
        time[0][5] = second() - t1;
        t1 = second();
        dgesl(aa,ldaa,n,ipvt,b,0);
        time[1][5] = second() - t1;
        total = time[0][5] + time[1][5];
        time[2][5] = total;
        time[3][5] = ops/(1.0e3*total);
        time[4][5] = 2.0e3/time[3][5];
        time[5][5] = total/cray;

 matgen(aa,ldaa,n,b,&norma);
 t1 = second();
 dgefa(aa,ldaa,n,ipvt,&info);
 time[0][6] = second() - t1;
 t1 = second();
 dgesl(aa,ldaa,n,ipvt,b,0);
 time[1][6] = second() - t1;
 total = time[0][6] + time[1][6];
 time[2][6] = total;
 time[3][6] = ops/(1.0e3*total);
 time[4][6] = 2.0e3/time[3][6];
 time[5][6] = total/cray;

 ntimes = 10;
 tm2 = 0;
 t1 = second();
 for (i = 0; i < ntimes; i++) {
  tm = second();
  matgen(aa,ldaa,n,b,&norma);
  tm2 = tm2 + second() - tm;
  dgefa(aa,ldaa,n,ipvt,&info);
 }
 time[0][7] = (second() - t1 - tm2)/ntimes;
 t1 = second();
 for (i = 0; i < ntimes; i++) {
  dgesl(aa,ldaa,n,ipvt,b,0);
 }
 time[1][7] = (second() - t1)/ntimes;
 total = time[0][7] + time[1][7];
 time[2][7] = total;
 time[3][7] = ops/(1.0e3*total);
 time[4][7] = 2.0e3/time[3][7];
 time[5][7] = total/cray;




 kf = (time[3][3] < time[3][7]) ? time[3][3] : time[3][7];
 kf = (kf > 0.0) ? (kf + .5) : (kf - .5);
 if (fabs((double)kf) < 1.0)
  kflops = 0;
 else {
  kflops = floor(fabs((double)kf));
  if (kf < 0.0) kflops = -kflops;
 }

 fprintf(stderr," times for array with leading dimension of%4d\n",ldaa);
 print_time(4);
 print_time(5);
 print_time(6);
 print_time(7);
 fprintf(stderr,"Rolled ");fprintf(stderr,"Single ");
 fprintf(stderr," Precision %5d Kflops ; %d Reps \n",kflops,10);
}


print_time (row)
int row;
{
if (row<0 || row>=time_dim2) { error(); };
fprintf(stderr,"%11.2f%11.2f%11.2f%11.0f%11.2f%11.2f\n", (double)time[0][row],
       (double)time[1][row], (double)time[2][row], (double)time[3][row],
       (double)time[4][row], (double)time[5][row]);
}


matgen(a,lda,n,b,norma)
float a[],b[],*norma;
int lda, n;




{
 int init, i, j;

 init = 1325;
 *norma = 0.0;
 for (j = 0; j < n; j++) {
  for (i = 0; i < n; i++) {
   init = 3125*init % 65536;
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
   a[lda*j+i] = (init - 32768.0)/16384.0;
   *norma = (a[lda*j+i] > *norma) ? a[lda*j+i] : *norma;
  }
 }
 for (i = 0; i < n; i++) {
if (i<0 || i>=b_dim) { error(); };
          b[i] = 0.0;
 }
 for (j = 0; j < n; j++) {
  for (i = 0; i < n; i++) {
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (i<0 || i>=b_dim) { error(); };
   b[i] = b[i] + a[lda*j+i];
  }
 }
}


dgefa(a,lda,n,ipvt,info)
float a[];
int lda,n,ipvt[],*info;
# 382 "scimark/linpack.c"
{


float t;
int idamax(),j,k,kp1,l,nm1;




 *info = 0;
 nm1 = n - 1;
 if (nm1 >= 0) {
  for (k = 0; k < nm1; k++) {
   kp1 = k + 1;



if (k<0 || k>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
   l = idamax(n-k,&a[lda*k+k],1) + k;
if (k<0 || k>=ipvt_dim) { error(); };
   ipvt[k] = l;




if (k<0 || k>=aa_dim1) { error(); };
if (l<0 || l>=aa_dim2) { error(); };
   if (a[lda*k+l] != 0.0) {



    if (l != k) {
if (k<0 || k>=aa_dim1) { error(); };
if (l<0 || l>=aa_dim2) { error(); };
     t = a[lda*k+l];
if (k<0 || k>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
     a[lda*k+l] = a[lda*k+k];
     a[lda*k+k] = t;
    }



if (k<0 || k>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
    t = -1.0/a[lda*k+k];
if (k<0 || k>=aa_dim1) { error(); };
if (k+1<0 || k+1>=aa_dim2) { error(); };
    dscal(n-(k+1),t,&a[lda*k+k+1],1);



    for (j = kp1; j < n; j++) {
if (j<0 || j>=aa_dim1) { error(); };
if (l<0 || l>=aa_dim2) { error(); };
     t = a[lda*j+l];
     if (l != k) {
if (j<0 || j>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (l<0 || l>=aa_dim2) { error(); };
      a[lda*j+l] = a[lda*j+k];
      a[lda*j+k] = t;
     }
if (k<0 || k>=aa_dim1) { error(); };
if (k+1<0 || k+1>=aa_dim2) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (k+1<0 || k+1>=aa_dim2) { error(); };
     daxpy(n-(k+1),t,&a[lda*k+k+1],1,
           &a[lda*j+k+1],1);
      }
     }
   else {
               *info = k;
   }
  }
 }
if (n-1<0 || n-1>=ipvt_dim) { error(); };
 ipvt[n-1] = n-1;
if (n-1<0 || n-1>=aa_dim1) { error(); };
if (n-1<0 || n-1>=aa_dim2) { error(); };
 if (a[lda*(n-1)+(n-1)] == 0.0) *info = n-1;
}



dgesl(a,lda,n,ipvt,b,job)
int lda,n,ipvt[],job;
float a[],b[];
# 531 "scimark/linpack.c"
{


 float ddot(),t;
 int k,kb,l,nm1;

 nm1 = n - 1;
 if (job == 0) {




  if (nm1 >= 1) {
   for (k = 0; k < nm1; k++) {
if (k<0 || k>=ipvt_dim) { error(); };
    l = ipvt[k];
if (l<0 || l>=b_dim) { error(); };
    t = b[l];
    if (l != k){
if (k<0 || k>=b_dim) { error(); };
if (l<0 || l>=b_dim) { error(); };
     b[l] = b[k];
     b[k] = t;
    }
if (k<0 || k>=aa_dim1) { error(); };
if (k+1<0 || k+1>=aa_dim2) { error(); };
if (k+1<0 || k+1>=b_dim) { error(); };
    daxpy(n-(k+1),t,&a[lda*k+k+1],1,&b[k+1],1);
   }
  }



  for (kb = 0; kb < n; kb++) {
      k = n - (kb + 1);
if (k<0 || k>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
if (k<0 || k>=b_dim) { error(); };
      b[k] = b[k]/a[lda*k+k];
      t = -b[k];
if (k<0 || k>=aa_dim1) { error(); };
if (0<0 || 0>=aa_dim2) { error(); };
if (0<0 || 0>=aa_dim1) { error(); };
      daxpy(k,t,&a[lda*k+0],1,&b[0],1);
  }
 }
 else {




  for (k = 0; k < n; k++) {
if (k<0 || k>=aa_dim1) { error(); };
if (0<0 || 0>=aa_dim2) { error(); };
if (0<0 || 0>=b_dim) { error(); };
   t = ddot(k,&a[lda*k+0],1,&b[0],1);
if (k<0 || k>=aa_dim1) { error(); };
if (k<0 || k>=aa_dim2) { error(); };
if (k<0 || k>=b_dim) { error(); };
   b[k] = (b[k] - t)/a[lda*k+k];
  }



  if (nm1 >= 1) {
   for (kb = 1; kb < nm1; kb++) {
    k = n - (kb+1);
if (k<0 || k>=b_dim) { error(); };
if (k<0 || k>=aa_dim1) { error(); };
if (k+1<0 || k+1>=aa_dim2) { error(); };
if (k+1<0 || k+1>=b_dim) { error(); };
    b[k] = b[k] + ddot(n-(k+1),&a[lda*k+k+1],1,&b[k+1],1);
if (k<0 || k>=ipvt_dim) { error(); };
    l = ipvt[k];
    if (l != k) {
if (k<0 || k>=b_dim) { error(); };
if (l<0 || l>=b_dim) { error(); };
     t = b[l];
     b[l] = b[k];
     b[k] = t;
    }
   }
  }
 }
}



void daxpy(n,da,dx,incx,dy,incy)




float dx[],dy[],da;
int incx,incy,n;
{
 int i,ix,iy,m,mp1;

 if(n <= 0) return;
 if (da == 0.0) return;

 if(incx != 1 || incy != 1) {




  ix = 0;
  iy = 0;
  if(incx < 0) ix = (-n+1)*incx;
  if(incy < 0)iy = (-n+1)*incy;
  for (i = 0;i < n; i++) {
if (201*0+0+ix<0 || 201*0+0+ix>=aa_dim1*aa_dim2) { error(); };
if (iy<0 || iy>=b_dim) { error(); };
   dy[iy] = dy[iy] + da*dx[ix];
   ix = ix + incx;
   iy = iy + incy;
  }
        return;
 }




 for (i = 0;i < n; i++) {
if (201*0+0+i<0 || 201*0+0+i>=aa_dim1*aa_dim2) { error(); };
if (i<0 || i>=b_dim) { error(); };
  dy[i] = dy[i] + da*dx[i];
 }
# 675 "scimark/linpack.c"
}



float ddot(n,dx,incx,dy,incy)




float dx[],dy[];

int incx,incy,n;
{
 float dtemp;
 int i,ix,iy,m,mp1;

 dtemp = 0.0;

 if(n <= 0) return(0.0);

 if(incx != 1 || incy != 1) {




  ix = 0;
  iy = 0;
  if (incx < 0) ix = (-n+1)*incx;
  if (incy < 0) iy = (-n+1)*incy;
  for (i = 0;i < n; i++) {
if (201*0+0+ix<0 || 201*0+0+ix>=aa_dim1*aa_dim2) { error(); };
if (iy<0 || iy>=b_dim) { error(); };
   dtemp = dtemp + dx[ix]*dy[iy];
   ix = ix + incx;
   iy = iy + incy;
  }
  return(dtemp);
 }




 for (i=0;i < n; i++){
if (201*0+0+i<0 || 201*0+0+i>=aa_dim1*aa_dim2) { error(); };
if (i<0 || i>=b_dim) { error(); };
  dtemp = dtemp + dx[i]*dy[i];
 }
 return(dtemp);
# 739 "scimark/linpack.c"
}


void dscal(n,da,dx,incx)




float da,dx[];
int n, incx;
{
 int i,m,mp1,nincx;

 if(n <= 0)return;
 if(incx != 1) {



  nincx = n*incx;
  for (i = 0; i < nincx; i = i + incx){
if (i<0 || i>=aa_dim1*aa_dim2) { error(); };
   dx[i] = da*dx[i];
  }
  return;
 }




for (i = 0; i < n; i++){
if (i<0 || i>=aa_dim1*aa_dim2) { error(); };
  dx[i] = da*dx[i];
 }
# 790 "scimark/linpack.c"
}


int idamax(n,dx,incx)






float dx[];
int incx,n;
{
 float dmax;
 int i, ix, itemp;

 if( n < 1 ) return(-1);
 if(n ==1 ) return(0);
 if(incx != 1) {



  ix = 1;
if (0<0 || 0>=aa_dim1*aa_dim2) { error(); };
  dmax = fabs((double)dx[0]);
  ix = ix + incx;
  for (i = 1; i < n; i++) {
if (ix<0 || ix>=aa_dim1*aa_dim2) { error(); };
   if(fabs((double)dx[ix]) > dmax) {
    itemp = i;
if (ix<0 || ix>=aa_dim1*aa_dim2) { error(); };
    dmax = fabs((double)dx[ix]);
   }
   ix = ix + incx;
  }
 }
 else {



  itemp = 0;
if (0<0 || 0>=aa_dim1*aa_dim2) { error(); };
  dmax = fabs((double)dx[0]);
  for (i = 1; i < n; i++) {
if (i<0 || i>=aa_dim1*aa_dim2) { error(); };
   if(fabs((double)dx[i]) > dmax) {
    itemp = i;
if (i<0 || i>=aa_dim1*aa_dim2) { error(); };
    dmax = fabs((double)dx[i]);
   }
  }
 }
 return (itemp);
}


float epslon (x)
float x;




{
 float a,b,c,eps;
# 882 "scimark/linpack.c"
 a = 4.0e0/3.0e0;
 eps = 0.0;
 while (eps == 0.0) {
  b = a - 1.0;
  c = b + b + b;
  eps = fabs((double)(c-1.0));
 }
 return(eps*fabs((double)x));
}


dmxpy (n1, y, n2, ldm, x, m)
float y[], x[], m[];
int n1, n2, ldm;
# 923 "scimark/linpack.c"
{
 int j,i,jmin;


 j = n2 % 2;
 if (j >= 1) {
  j = j - 1;
  for (i = 0; i < n1; i++) {
if (i<0 || i>=b_dim) { error(); };
if (j<0 || j>=x_dim) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
              y[i] = (y[i]) + x[j]*m[ldm*j+i];
             }
 }



 j = n2 % 4;
 if (j >= 2) {
  j = j - 1;
  for (i = 0; i < n1; i++) {
if (i<0 || i>=b_dim) { error(); };
if (j-1<0 || j-1>=x_dim) { error(); };
if (j-1<0 || j-1>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j<0 || j>=x_dim) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
              y[i] = ( (y[i])
                          + x[j-1]*m[ldm*(j-1)+i]) + x[j]*m[ldm*j+i];
                         }
 }



 j = n2 % 8;
 if (j >= 4) {
  j = j - 1;
  for (i = 0; i < n1; i++) {
if (i<0 || i>=b_dim) { error(); };
if (j-3<0 || j-3>=x_dim) { error(); };
if (j-3<0 || j-3>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-2<0 || j-2>=x_dim) { error(); };
if (j-2<0 || j-2>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j<0 || j>=x_dim) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
   y[i] = ((( (y[i])
          + x[j-3]*m[ldm*(j-3)+i])
          + x[j-2]*m[ldm*(j-2)+i])
          + x[j-1]*m[ldm*(j-1)+i]) + x[j]*m[ldm*j+i];
         }
 }



 j = n2 % 16;
 if (j >= 8) {
  j = j - 1;
  for (i = 0; i < n1; i++) {
if (i<0 || i>=b_dim) { error(); };
if (j-7<0 || j-7>=x_dim) { error(); };
if (j-7<0 || j-7>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-6<0 || j-6>=x_dim) { error(); };
if (j-6<0 || j-6>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-5<0 || j-5>=x_dim) { error(); };
if (j-5<0 || j-5>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-4<0 || j-4>=x_dim) { error(); };
if (j-4<0 || j-4>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-3<0 || j-3>=x_dim) { error(); };
if (j-3<0 || j-3>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-2<0 || j-2>=x_dim) { error(); };
if (j-2<0 || j-2>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-1<0 || j-1>=x_dim) { error(); };
if (j-1<0 || j-1>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j<0 || j>=x_dim) { error(); };
if (j<0 || j>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
   y[i] = ((((((( (y[i])
          + x[j-7]*m[ldm*(j-7)+i]) + x[j-6]*m[ldm*(j-6)+i])
            + x[j-5]*m[ldm*(j-5)+i]) + x[j-4]*m[ldm*(j-4)+i])
          + x[j-3]*m[ldm*(j-3)+i]) + x[j-2]*m[ldm*(j-2)+i])
          + x[j-1]*m[ldm*(j-1)+i]) + x[j] *m[ldm*j+i];
         }
 }



 jmin = (n2%16)+16;
 for (j = jmin-1; j < n2; j = j + 16) {
  for (i = 0; i < n1; i++) {
if (i<0 || i>=b_dim) { error(); };
if (j-15<0 || j-15>=x_dim) { error(); };
if (j-15<0 || j-15>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-14<0 || j-14>=x_dim) { error(); };
if (j-14<0 || j-14>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-13<0 || j-13>=x_dim) { error(); };
if (j-13<0 || j-13>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-12<0 || j-12>=x_dim) { error(); };
if (j-12<0 || j-12>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-11<0 || j-11>=x_dim) { error(); };
if (j-11<0 || j-11>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-10<0 || j-10>=x_dim) { error(); };
if (j-10<0 || j-10>=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-9 <0 || j-9 >=x_dim) { error(); };
if (j-9 <0 || j-9 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-8 <0 || j-8 >=x_dim) { error(); };
if (j-8 <0 || j-8 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-7 <0 || j-7 >=x_dim) { error(); };
if (j-7 <0 || j-7 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-6 <0 || j-6 >=x_dim) { error(); };
if (j-6 <0 || j-6 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-5 <0 || j-5 >=x_dim) { error(); };
if (j-5 <0 || j-5 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-4 <0 || j-4 >=x_dim) { error(); };
if (j-4 <0 || j-4 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-3 <0 || j-3 >=x_dim) { error(); };
if (j-3 <0 || j-3 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-2 <0 || j-2 >=x_dim) { error(); };
if (j-2 <0 || j-2 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-1 <0 || j-1 >=x_dim) { error(); };
if (j-1 <0 || j-1 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
if (j-0 <0 || j-0 >=x_dim) { error(); };
if (j-0 <0 || j-0 >=aa_dim1) { error(); };
if (i<0 || i>=aa_dim2) { error(); };
   y[i] = ((((((((((((((( (y[i])
           + x[j-15]*m[ldm*(j-15)+i])
    + x[j-14]*m[ldm*(j-14)+i])
           + x[j-13]*m[ldm*(j-13)+i])
    + x[j-12]*m[ldm*(j-12)+i])
           + x[j-11]*m[ldm*(j-11)+i])
    + x[j-10]*m[ldm*(j-10)+i])
           + x[j- 9]*m[ldm*(j- 9)+i])
    + x[j- 8]*m[ldm*(j- 8)+i])
           + x[j- 7]*m[ldm*(j- 7)+i])
    + x[j- 6]*m[ldm*(j- 6)+i])
           + x[j- 5]*m[ldm*(j- 5)+i])
    + x[j- 4]*m[ldm*(j- 4)+i])
           + x[j- 3]*m[ldm*(j- 3)+i])
    + x[j- 2]*m[ldm*(j- 2)+i])
           + x[j- 1]*m[ldm*(j- 1)+i])
    + x[j] *m[ldm*j+i];
   }
 }
}


float second()
{
# 1 "/usr/include/sys/time.h" 1 3 4
# 26 "/usr/include/sys/time.h" 3 4
# 1 "/usr/include/time.h" 1 3 4
# 74 "/usr/include/time.h" 3 4


typedef __time_t time_t;



# 27 "/usr/include/sys/time.h" 2 3 4

# 1 "/usr/include/bits/time.h" 1 3 4
# 69 "/usr/include/bits/time.h" 3 4
struct timeval
  {
    __time_t tv_sec;
    __suseconds_t tv_usec;
  };
# 29 "/usr/include/sys/time.h" 2 3 4

# 1 "/usr/include/sys/select.h" 1 3 4
# 31 "/usr/include/sys/select.h" 3 4
# 1 "/usr/include/bits/select.h" 1 3 4
# 32 "/usr/include/sys/select.h" 2 3 4


# 1 "/usr/include/bits/sigset.h" 1 3 4
# 23 "/usr/include/bits/sigset.h" 3 4
typedef int __sig_atomic_t;




typedef struct
  {
    unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
  } __sigset_t;
# 35 "/usr/include/sys/select.h" 2 3 4



typedef __sigset_t sigset_t;





# 1 "/usr/include/time.h" 1 3 4
# 118 "/usr/include/time.h" 3 4
struct timespec
  {
    __time_t tv_sec;
    long int tv_nsec;
  };
# 45 "/usr/include/sys/select.h" 2 3 4

# 1 "/usr/include/bits/time.h" 1 3 4
# 47 "/usr/include/sys/select.h" 2 3 4


typedef __suseconds_t suseconds_t;





typedef long int __fd_mask;
# 67 "/usr/include/sys/select.h" 3 4
typedef struct
  {






    __fd_mask __fds_bits[1024 / (8 * sizeof (__fd_mask))];


  } fd_set;






typedef __fd_mask fd_mask;
# 99 "/usr/include/sys/select.h" 3 4

# 109 "/usr/include/sys/select.h" 3 4
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 121 "/usr/include/sys/select.h" 3 4
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);



# 31 "/usr/include/sys/time.h" 2 3 4








# 56 "/usr/include/sys/time.h" 3 4
struct timezone
  {
    int tz_minuteswest;
    int tz_dsttime;
  };

typedef struct timezone *__restrict __timezone_ptr_t;
# 72 "/usr/include/sys/time.h" 3 4
extern int gettimeofday (struct timeval *__restrict __tv,
    __timezone_ptr_t __tz) __attribute__ ((__nothrow__));




extern int settimeofday (__const struct timeval *__tv,
    __const struct timezone *__tz) __attribute__ ((__nothrow__));





extern int adjtime (__const struct timeval *__delta,
      struct timeval *__olddelta) __attribute__ ((__nothrow__));




enum __itimer_which
  {

    ITIMER_REAL = 0,


    ITIMER_VIRTUAL = 1,



    ITIMER_PROF = 2

  };



struct itimerval
  {

    struct timeval it_interval;

    struct timeval it_value;
  };






typedef int __itimer_which_t;




extern int getitimer (__itimer_which_t __which,
        struct itimerval *__value) __attribute__ ((__nothrow__));




extern int setitimer (__itimer_which_t __which,
        __const struct itimerval *__restrict __new,
        struct itimerval *__restrict __old) __attribute__ ((__nothrow__));




extern int utimes (__const char *__file, __const struct timeval __tvp[2])
     __attribute__ ((__nothrow__));



extern int lutimes (__const char *__file, __const struct timeval __tvp[2])
     __attribute__ ((__nothrow__));


extern int futimes (int __fd, __const struct timeval __tvp[2]) __attribute__ ((__nothrow__));
# 181 "/usr/include/sys/time.h" 3 4

# 1098 "scimark/linpack.c" 2
# 1 "/usr/include/sys/resource.h" 1 3 4
# 25 "/usr/include/sys/resource.h" 3 4
# 1 "/usr/include/bits/resource.h" 1 3 4
# 33 "/usr/include/bits/resource.h" 3 4
enum __rlimit_resource
{

  RLIMIT_CPU = 0,



  RLIMIT_FSIZE = 1,



  RLIMIT_DATA = 2,



  RLIMIT_STACK = 3,



  RLIMIT_CORE = 4,






  __RLIMIT_RSS = 5,



  RLIMIT_NOFILE = 7,
  __RLIMIT_OFILE = RLIMIT_NOFILE,




  RLIMIT_AS = 9,



  __RLIMIT_NPROC = 6,



  __RLIMIT_MEMLOCK = 8,



  __RLIMIT_LOCKS = 10,



  __RLIMIT_SIGPENDING = 11,



  __RLIMIT_MSGQUEUE = 12,





  __RLIMIT_NICE = 13,




  __RLIMIT_RTPRIO = 14,


  __RLIMIT_NLIMITS = 15,
  __RLIM_NLIMITS = __RLIMIT_NLIMITS


};
# 127 "/usr/include/bits/resource.h" 3 4
typedef __rlim_t rlim_t;







struct rlimit
  {

    rlim_t rlim_cur;

    rlim_t rlim_max;
  };
# 154 "/usr/include/bits/resource.h" 3 4
enum __rusage_who
{

  RUSAGE_SELF = 0,



  RUSAGE_CHILDREN = -1

};


# 1 "/usr/include/bits/time.h" 1 3 4
# 167 "/usr/include/bits/resource.h" 2 3 4


struct rusage
  {

    struct timeval ru_utime;

    struct timeval ru_stime;

    long int ru_maxrss;


    long int ru_ixrss;

    long int ru_idrss;

    long int ru_isrss;


    long int ru_minflt;

    long int ru_majflt;

    long int ru_nswap;


    long int ru_inblock;

    long int ru_oublock;

    long int ru_msgsnd;

    long int ru_msgrcv;

    long int ru_nsignals;



    long int ru_nvcsw;


    long int ru_nivcsw;
  };







enum __priority_which
{
  PRIO_PROCESS = 0,

  PRIO_PGRP = 1,

  PRIO_USER = 2

};
# 26 "/usr/include/sys/resource.h" 2 3 4


typedef __id_t id_t;




# 43 "/usr/include/sys/resource.h" 3 4
typedef int __rlimit_resource_t;
typedef int __rusage_who_t;
typedef int __priority_which_t;





extern int getrlimit (__rlimit_resource_t __resource,
        struct rlimit *__rlimits) __attribute__ ((__nothrow__));
# 70 "/usr/include/sys/resource.h" 3 4
extern int setrlimit (__rlimit_resource_t __resource,
        __const struct rlimit *__rlimits) __attribute__ ((__nothrow__));
# 88 "/usr/include/sys/resource.h" 3 4
extern int getrusage (__rusage_who_t __who, struct rusage *__usage) __attribute__ ((__nothrow__));





extern int getpriority (__priority_which_t __which, id_t __who) __attribute__ ((__nothrow__));



extern int setpriority (__priority_which_t __which, id_t __who, int __prio)
     __attribute__ ((__nothrow__));


# 1099 "scimark/linpack.c" 2

struct rusage ru;
float t ;



getrusage(RUSAGE_SELF,&ru) ;

t = (float) (ru.ru_utime.tv_sec+ru.ru_stime.tv_sec) +
    ((float) (ru.ru_utime.tv_usec+ru.ru_stime.tv_usec))/1.0e6 ;
return t ;
}
