# Makefile for mysterx.dll, myspage.dll, myssink.dll

# If you get errors about a missing file atlbase.h,
# check the value of the environment variable INCLUDE.
# The subdirectory ATL beneath the Visual C++ directory
# needs to be in the list of INCLUDE directories.
# For example, if Visual Studio 6 is installed at the root of
# the C: drive, that directory would be
#   C:\Microsoft Visual Studio\VC98\ATL

# The Microsoft HTML Help Workshop must be installed to
# build mysterx.dll.  The files are on the Visual Studio
# CD.

# The code for mx_element_focus in htmlutil.cxx uses the
# COM interface IHTMLElement2.  That interface is defined
# in MsHTML.h in recent versions of the Platform SDK (more 
# recent than the VC++6 release.  If you don't have such a 
# Platform SDK, comment out the code in the body of that 
# function.

all : 
	cd myspage
	nmake /f myspage.mak
	cd ../mysc
	nmake /f mysc.mak
	cd ../myssink
	nmake /f myssink.mak
	cd ..
	nmake /f mysterx.mak

clean :
	cd myspage
	nmake clean /f myspage.mak
	cd ../mysc
	nmake clean /f mysc.mak
	cd ../myssink
	nmake clean /f myssink.mak
	cd ..
	nmake clean /f mysterx.mak
