/*!!snippet csyscall.c*/
int CommandProcessorAvailable()
{
// See if a command processor is present.
// If it is then system() returns a non-zero value,
// otherwise it returns zero.
  char* s;
  s = 0;
  return(system(s));
}

int RunSimpleCommand(char command[], char argument[])
{
// This builds a command "command argument".
// The code then executes that command and returns
// the status.  The allocated space for the combined
// command is then freed.  The status is 0 for success
// and 1 for failure.

  const char* separator = " ";  // This works for Unix, MS-DOS.
  char* syscommand;
  int status;

  /* Malloc enough space to contain the built command */
  syscommand = (char*)malloc(strlen(command)+strlen(separator)
			    +strlen(argument)+1);
  strcpy(syscommand, command);
  strcat(syscommand, separator);
  strcat(syscommand, argument);
  status = system(syscommand);
  free(syscommand);
  return (status);
}
/*!!end snippet csyscall.c*/
