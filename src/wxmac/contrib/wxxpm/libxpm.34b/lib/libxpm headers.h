/*
 *	MacHeaders.h
 *
 *	Processor independant interface to the MacHeaders<xxx> files ...
 */

#if __POWERPC__
	#include <MacHeadersPPC>
#elif __CFM68K__
	#include <MacHeadersCFM68K>
#else
	#include <MacHeaders68K>
#endif
#define FOR_MAC
