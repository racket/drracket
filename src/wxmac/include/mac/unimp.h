#ifndef _unimp_h
#define _unimp_h 1

#define wxOPEN 1
#define wxSAVE 2
#define wxOVERWRITE_PROMPT 4
#define wxHIDE_READONLY 8

Bool wxWriteResource(const char *section, const char *entry, char *value,  char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value, const char *file = NULL);

Bool wxGetResource(const char *section, const char *entry, char **value,  char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value, const char *file = NULL);

#endif