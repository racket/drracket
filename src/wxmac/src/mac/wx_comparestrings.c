// Needed for the mac since ANSI stdlib is C compiled
#include <string.h>
#include <ctype.h>
#ifdef CJC
int wx_comparestrings(const void *arg1, const void *arg2);
extern "C" int strcmp(char* s1, char* s2);

int wx_comparestrings(const void *arg1, const void *arg2)
{
  char **s1 = (char **)arg1;
  char **s2 = (char **)arg2;

  return strcmp(*s1, *s2);
}
#endif

int strcasecmp(char *s, char *t);
int strcasecmp(char *s, char *t)
{
	int r;
	while (*s && *t) {
		r = tolower(*s++) - tolower(*t++);
		if (r != 0) return r;
	}
	return (tolower(*s) - tolower(*t));		// CJC is this correct
}

int strncasecmp(char *s, char *t, int w);
int strncasecmp(char *s, char *t, int w)
{
	int r,i = 0;
	while (i < w) {
		r = tolower(s[i]) - tolower(t[i]);
		if (r != 0) return r;
		i += 1;	
	}
	return 0;
}
