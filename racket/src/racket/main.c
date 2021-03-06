/*
  Racket
  Copyright (c) 2004-2017 PLT Design Inc.
  Copyright (c) 1995-2000 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file defines Racket's main(), which is a jumble of
   platform-specific initialization. The included file "cmdline.inc"
   implements command-line parsing. (GRacket also uses "cmdline.inc".)

   The rest of the source code resides in the `src' subdirectory
   (except for the garbage collector, which is in `gc', `sgc', or
   `gc2', depending on which one you're using). */

#ifdef __MINGW32__
# define __MINGW32_DELAY_LOAD__ 1
#endif
#include "scheme.h"

/*========================================================================*/
/*                       configuration and includes                       */
/*========================================================================*/

/* #define STANDALONE_WITH_EMBEDDED_EXTENSION */
/*    STANDALONE_WITH_EMBEDDED_EXTENSION builds an executable with
      built-in extensions. The extension is initialized by calling
      scheme_initialize(env), where `env' is the initial environment.
      By default, command-line parsing, the REPL, and initilization
      file loading are turned off. */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
# define DONT_PARSE_COMMAND_LINE
# define DONT_RUN_REP
# define DONT_LOAD_INIT_FILE
#endif

#if defined(MZ_XFORM) && defined(__MINGW32__) && !defined(USE_THREAD_LOCAL)
XFORM_GC_VARIABLE_STACK_THROUGH_DIRECT_FUNCTION;
#endif

#ifdef MZ_XFORM
START_XFORM_SUSPEND;
#endif

#include <sys/types.h>
#ifndef DOS_FILE_SYSTEM
# include <sys/time.h>
#endif
#ifndef NO_USER_BREAK_HANDLER
# include <signal.h>
#endif
#ifdef OS_X
# include <unistd.h>
#endif

#ifdef INSTRUMENT_PRIMITIVES 
extern int g_print_prims;
#endif

#ifdef MZ_XFORM
END_XFORM_SUSPEND;
#endif

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
# include <windows.h>
# define GC_THINKS_ITS_A_DLL_BUT_ISNT
#endif
#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
#endif

/*========================================================================*/
/*                configuration for command-line parsing                  */
/*========================================================================*/

#ifndef DONT_LOAD_INIT_FILE
/*
 * Get the init filename for the system
 * * First look to see if <addon-dir>/interactive.rkt exists
 * * Otherwise check config file for location
 */
static Scheme_Object *get_init_filename(Scheme_Env *env,
                                        char *init_filename_sym,
                                        char *default_init_module,
                                        char *user_init_module)
{
  Scheme_Object *f, *a[2], *build_path;
  Scheme_Thread * volatile p;
  mz_jmp_buf * volatile save, newbuf;

  p = scheme_get_current_thread();
  save = p->error_buf;
  p->error_buf = &newbuf;

  if(!scheme_setjmp(newbuf)) {
    build_path = scheme_builtin_value("build-path");

    /* First test to see if user init file exists */
    f = scheme_builtin_value("find-system-path");
    a[0] = scheme_intern_symbol("addon-dir");
    a[0] = _scheme_apply(f, 1, a);
    a[1] = scheme_make_path(user_init_module);
    f = _scheme_apply(build_path, 2, a);
    if (SCHEME_PATHP(f)) {
      char *filename;
      filename = scheme_expand_filename(SCHEME_PATH_VAL(f), -1, "startup", NULL, SCHEME_GUARD_FILE_EXISTS);
      if(scheme_file_exists(filename)) {
        p->error_buf = save;
        return scheme_make_path(filename);
      }
    }

    /* Failed, next check config.rkt fo system init file */
    f = scheme_builtin_value("find-main-config");
    a[0] = _scheme_apply(f, 0, NULL);
    a[1] = scheme_make_path("config.rktd");
    f = _scheme_apply(build_path, 2, a);
    if (SCHEME_PATHP(f)) {
      char *filename;
      filename = scheme_expand_filename(SCHEME_PATH_VAL(f), -1, "startup", NULL,
                                        SCHEME_GUARD_FILE_EXISTS | SCHEME_GUARD_FILE_READ);
      if(scheme_file_exists(filename)) {
        Scheme_Object * port;
        port = scheme_open_input_file(SCHEME_PATH_VAL(f), "get-init-filename");
        f = scheme_read(port);
        scheme_close_input_port(port);
        if(SCHEME_HASHTRP(f)) {
          f = scheme_hash_tree_get((Scheme_Hash_Tree *)f, scheme_intern_symbol(init_filename_sym));
          if(f) {
            p->error_buf = save;
            return f;
          }
        }
      }
    }

    /* Failed to load custom init file, load racket/interactive */
    f = scheme_intern_symbol(default_init_module);
    p->error_buf = save;
    return f;
  }

  p->error_buf = save;

  return NULL;
}
#endif

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
extern Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifndef UNIX_INIT_FILENAME
# define UNIX_INIT_FILENAME "~/.racketrc"
# define WINDOWS_INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\racketrc.rktl"
# define MACOS9_INIT_FILENAME "PREFERENCES:racketrc.rktl"
# define INIT_FILENAME_CONF_SYM "interactive-file"
# define DEFAULT_INIT_MODULE "racket/interactive"
# define USER_INIT_MODULE "interactive.rkt"
# define PRINTF printf
# define PROGRAM "Racket"
# define PROGRAM_LC "racket"
# define INITIAL_BIN_TYPE "zi"
# define RACKET_CMD_LINE
# define INITIAL_NAMESPACE_MODULE "racket/init"
#endif

#ifdef DOS_FILE_SYSTEM
# define INIT_FILENAME WINDOWS_INIT_FILENAME
#else
# define INIT_FILENAME UNIX_INIT_FILENAME
#endif

#define CMDLINE_FFLUSH fflush

#define BANNER scheme_banner()

/*========================================================================*/
/*                            OS process name                             */
/*========================================================================*/

#if defined(__linux__)
# include <sys/prctl.h>
# ifdef PR_SET_NAME
#  define CAN_SET_OS_PROCESS_NAME 1
void set_os_process_name(char *sprog)
{
  int i = strlen(sprog) - 1;
  while (i && (sprog[i - 1] != '/')) {
    --i;
  }
  prctl(PR_SET_NAME, sprog + i);
}
# endif
#endif

/*========================================================================*/
/*                        command-line parsing                            */
/*========================================================================*/

#include "cmdline.inc"

/*========================================================================*/
/*                           ctl-C handler                                */
/*========================================================================*/

#if !defined(NO_USER_BREAK_HANDLER) || defined(DOS_FILE_SYSTEM)

static void *break_handle;
static void *signal_handle;

# ifndef NO_USER_BREAK_HANDLER

static void user_break_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_main_thread_at(break_handle);
  scheme_signal_received_at(signal_handle);
}

# ifndef NO_SIGTERM_HANDLER
static void term_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_kind_main_thread_at(break_handle, MZEXN_BREAK_TERMINATE);
  scheme_signal_received_at(signal_handle);
}
# endif

# ifndef NO_SIGHUP_HANDLER
static void hup_hit(int ignore)
  XFORM_SKIP_PROC
{
  scheme_break_kind_main_thread_at(break_handle, MZEXN_BREAK_HANG_UP);
  scheme_signal_received_at(signal_handle);
}
# endif

# endif

# ifdef DOS_FILE_SYSTEM
static BOOL WINAPI ConsoleBreakHandler(DWORD op)
{
  scheme_break_main_thread_at(break_handle);
  scheme_signal_received_at(signal_handle);
  return TRUE;
}
#endif

#endif

/*========================================================================*/
/*                                 main                                   */
/*========================================================================*/

#ifdef USE_SENORA_GC
# include "sgc/sgc.h"
#endif

/* Forward declarations: */
static void do_scheme_rep(Scheme_Env *, FinishArgs *f);
static int cont_run(FinishArgs *f);

#if defined(__MINGW32__)
# define MAIN zmain
# define MAIN_char char
# define MAIN_argv argv
#elif defined(WINDOWS_UNICODE_SUPPORT) && !defined(__CYGWIN32__) && !defined(MZ_DEFINE_UTF8_MAIN)
# define MAIN wmain
# define MAIN_char wchar_t
# define MAIN_argv wargv
# define WINDOWS_UNICODE_MAIN
#else
# define MAIN main
# define MAIN_char char
# define MAIN_argv argv
#endif

/*****************************     main    ********************************/
/*          Prepare for delayload, then call main_after_dlls              */

static int main_after_dlls(int argc, MAIN_char **MAIN_argv);
static int main_after_stack(void *data);

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

#if defined(__MINGW32__) || defined(WINMAIN_ALREADY)
# include "parse_cmdl.inc"
#endif

#ifdef DOS_FILE_SYSTEM
# include "win_tls.inc"
#endif

#ifdef DOS_FILE_SYSTEM
void load_delayed()
{
  (void)SetErrorMode(SEM_FAILCRITICALERRORS);

# ifndef MZ_NO_LIBRACKET_DLL
  /* Order matters: load dependencies first */
#  ifndef MZ_PRECISE_GC
  load_delayed_dll(NULL, "libmzgcxxxxxxx.dll");
#  endif
  load_delayed_dll(NULL, "libracket" DLL_3M_SUFFIX "xxxxxxx.dll");
# endif
  record_dll_path();

  register_win_tls();
}
#endif

int MAIN(int argc, MAIN_char **MAIN_argv)
{
#if defined(DOS_FILE_SYSTEM) && !defined(__MINGW32__)
  load_delayed();
#endif

  return main_after_dlls(argc, MAIN_argv);
}

#if defined(__MINGW32__) && !defined(WINMAIN_ALREADY)
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR ignored, int nCmdShow)
{
  int argc;
  char **argv;

  load_delayed();

  scheme_set_atexit(atexit);

  argv = cmdline_to_argv(&argc, NULL);

  return zmain(argc, argv);
}
#endif

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

/************************     main_after_dlls    **************************/
/*        Prep stack for GC, then call main_after_stack (indirectly)      */

typedef struct {
  int argc;
  MAIN_char **argv;
} Main_Args;

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
# endif

static int main_after_dlls(int argc, MAIN_char **argv)
{
  Main_Args ma;
  ma.argc = argc;
  ma.argv = argv;
  return scheme_main_stack_setup(1, main_after_stack, &ma);
}

# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif

/************************     main_after_stack    *************************/
/*               Setup, parse command-line, and go to cont_run            */

static int main_after_stack(void *data)
{
  int rval;
  int argc;
  MAIN_char **MAIN_argv;
#ifdef WINDOWS_UNICODE_MAIN
  char **argv;
#endif

  argc = ((Main_Args *)data)->argc;
  MAIN_argv = ((Main_Args *)data)->argv;

#ifdef WINDOWS_UNICODE_MAIN
  {
    char *a;
    int i, j, l;
    argv = (char **)malloc(sizeof(char*)*argc);
    for (i = 0; i < argc; i++) {
      for (j = 0; wargv[i][j]; j++) {
      }
      l = scheme_utf8_encode((unsigned int*)wargv[i], 0, j, 
                             NULL, 0,
                             1 /* UTF-16 */);
      a = malloc(l + 1);
      scheme_utf8_encode((unsigned int *)wargv[i], 0, j, 
                         (unsigned char *)a, 0,
                         1 /* UTF-16 */);
      a[l] = 0;
      argv[i] = a;
    }
  }
#endif


#if !defined(NO_USER_BREAK_HANDLER) || defined(DOS_FILE_SYSTEM)
  break_handle = scheme_get_main_thread_break_handle();
  signal_handle = scheme_get_signal_handle();
# ifndef NO_USER_BREAK_HANDLER
  scheme_set_signal_handler(SIGINT, user_break_hit);
#  ifndef NO_SIGTERM_HANDLER
  scheme_set_signal_handler(SIGTERM, term_hit);
#  endif
#  ifndef NO_SIGHUP_HANDLER
  scheme_set_signal_handler(SIGHUP, hup_hit);
#  endif
# endif
# ifdef DOS_FILE_SYSTEM
  SetConsoleCtrlHandler(ConsoleBreakHandler, TRUE);      
# endif
#endif

#ifdef PRE_FILTER_CMDLINE_ARGUMENTS
  pre_filter_cmdline_arguments(&argc, &MAIN_argv);
#endif

  rval = run_from_cmd_line(argc, argv, scheme_basic_env, cont_run);

#ifndef DEFER_EXPLICIT_EXIT
  scheme_immediate_exit(rval);
  /* shouldn't get here */
#endif

  return rval;
}

/*************************      cont_run     ******************************/
/*                          Go to do_scheme_rep                           */

static int cont_run(FinishArgs *f)
{
  return finish_cmd_line_run(f, do_scheme_rep);
}

/*************************   do_scheme_rep   *****************************/
/*                  Finally, do a read-eval-print-loop                   */

static void do_scheme_rep(Scheme_Env *env, FinishArgs *fa)
{
  /* enter read-eval-print loop */
  Scheme_Object *rep, *a[2];
  int ending_newline = 1;

#ifdef GRAPHICAL_REPL
  if (!fa->a->alternate_rep) {
    a[0] = scheme_intern_symbol("racket/gui/init");
    a[1] = scheme_intern_symbol("graphical-read-eval-print-loop");
    ending_newline = 0;
  } else
#endif
    {
      a[0] = scheme_intern_symbol("racket/base");
      a[1] = scheme_intern_symbol("read-eval-print-loop");
    }

  rep = scheme_dynamic_require(2, a);
    
  if (rep) {
    scheme_apply(rep, 0, NULL);
    if (ending_newline)
      printf("\n");
  }
}

/*========================================================================*/
/*                         junk for testing                               */
/*========================================================================*/

#if 0
/* For testing STANDALONE_WITH_EMBEDDED_EXTENSION */
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_eval_string("(lambda (v) (and (eq? v #t) "
			    "  (lambda () "
			    "    (printf \"These were the args: ~a~n\" argv))))", 
			    env);
}
#endif
