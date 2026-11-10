/* Send Ctrl-C to the processes on another process's console.

   Windows has no signals to send to another process.  The nearest thing to
   SIGINT is Ctrl-C, which reaches every process attached to a console, and
   which the C runtime delivers as SIGINT to a program that handles it.  A
   worker running as a service has no console, so Windows gives each console
   program it starts a console of its own, shared only with that program's
   descendants.

   Sending Ctrl-C means attaching to that console, and a process attached
   while it is sent receives it too.  So the worker does not do it itself:
   this runs in a helper process ([worker.exe console-interrupt PID WORKER])
   which ignores Ctrl-C, attaches, sends it, and exits.

   Exit codes: 0 sent; 3 could not attach; 4 could not send; 5 the worker is
   on that console too (a worker started from a terminal shares its console
   with all its children), so nothing was sent. */
#include <caml/mlvalues.h>

#ifdef _WIN32
#include <windows.h>

value ocluster_console_interrupt_helper(value v_pid, value v_worker)
{
  DWORD procs[256];
  DWORD n, i;
  SetConsoleCtrlHandler(NULL, TRUE);
  FreeConsole();
  if (!AttachConsole((DWORD) Long_val(v_pid)))
    return Val_int(3);
  n = GetConsoleProcessList(procs, 256);
  for (i = 0; i < n && i < 256; i++)
    if (procs[i] == (DWORD) Long_val(v_worker)) {
      FreeConsole();
      return Val_int(5);
    }
  return Val_int(GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0) ? 0 : 4);
}
#else
value ocluster_console_interrupt_helper(value v_pid, value v_worker)
{
  (void) v_pid;
  (void) v_worker;
  return Val_int(2);
}
#endif
