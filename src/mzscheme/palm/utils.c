
#include <PalmOS.h>
typedef long FILE;
#define _LINUX_TYPES_H  /* Blocks types.h */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "resnum.h"
#include "mzpalm.h"

/* FILE *stdin, *stdout, *stderr; */
int errno;
char str[256];

void output_str()
{
  FormPtr frmP;
  void *field;
  MemHandle hand;
  long len;

  frmP = FrmGetActiveForm();
  field = FrmGetObjectPtr(frmP, FrmGetObjectIndex(frmP, OutputText));

  len = strlen(str) + 1;
  if (len & 0x1)
    len++;
  hand = MemHandleNew(len);
  MemMove(MemHandleLock(hand), str, len);
  MemHandleUnlock(hand);

  FldSetTextHandle((FieldPtr)field, hand);

  FrmDrawForm(frmP);
}

int printf(const char *s, ...)
{
  va_list args;

  va_start(args, s);
  StrVPrintF(str, s, args);
  va_end(args);

  output_str();
}

void fprintf(FILE *f, char *s, ...)
{
  va_list args;

  va_start(args, s);
  StrVPrintF(str, s, args);
  va_end(args);

  output_str();  
}

void vfprintf(FILE *f, char *s, va_list l)
{
  StrVPrintF(str, s, l);

  output_str();
}

int fread(char *s, int l, int c, FILE *f)
{
  return 0;
}

int fwrite(char *s, int l, int c, FILE *f)
{
  return 0;
}

void fflush(FILE *f)
{
}

int getc(FILE *f)
{
  return 0;
}

int feof(FILE *f)
{
}

void clearerr(FILE *f)
{
}

int fclose(FILE *f)
{
  return 0;
}

FILE *fopen(char *fn, char *m)
{
  return NULL;
}

int unlink(char *fn)
{
  return 0;
}

int fseek(FILE *f, int p, int d)
{
  return 0;
}

int ftell(FILE *f)
{
  return 0;
}

char *strerror(intn)
{
  return "unknown cause";
}

/*****************************************************************************/

void _cleanup()
{
}

jmpbuf exit_buf;

void _exit(int v)
{
  longjmp(&exit_buf, 1);
}

/*****************************************************************************/

double fmod(double a, double b)
{
  return 0;
}

double modf(double a, double *b)
{
  return 0;
}

double strtod(char *s, char **r)
{
  return 0;
}

/***************************************************************************/

void qsort(void *p, int s, int l, int (*f)(const void *, const void *))
{
}

void sleep(long s)
{
}

char* getcwd(char *s, int l)
{
  return NULL;
}

int chdir(char *d)
{
  return -1;
}
