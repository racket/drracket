# Makefile for srpersist.so on Unix boxes

# change to ODBC version supported by driver manager
ODBCVER=0x0351

# static library that implements driver manager
DRIVER_MGR_LIB=odbc

# installation directory
LIBDIR=/usr/site/unixODBC/lib

MZC=../../bin/mzc 

CPP=gcc 
CPP_FLAGS=-DODBCVER=$(ODBCVER) -I. -I../../collects/mzscheme/include -c 

OBJS=srpersist.o srpbuffer.o srptypes.o

ARCH=`archsys`
 
NATIVE_DIR=../../collects/srpersist/lib/$(ARCH)

.cxx.o ::
	$(CPP) $(CPP_FLAGS) $<  

all : srpmain.so

install : srpmain.so 
	mkdir -p $(NATIVE_DIR)
	cp -p srpmain.so $(NATIVE_DIR)/

srpmain.so : $(OBJS)
	$(MZC) --ld srpmain.so $(OBJS) -L$(LIBDIR) -l$(DRIVER_MGR_LIB)

srpersist.o : srpersist.cxx srptypes.h srpbuffer.h srpersist.h srpprims.tbl \
	srpstructs.tbl srpexns.tbl srpconsts.tbl srpinfo.tbl srpbitmask.tbl
	$(CPP) $(CPP_FLAGS) srpersist.cxx

srpbuffer.o : srpbuffer.cxx srptypes.h srpbuffer.h srpersist.h
	$(CPP) $(CPP_FLAGS) srpbuffer.cxx

srptypes.o : srptypes.cxx srptypes.h srpersist.h
	$(CPP) $(CPP_FLAGS) srptypes.cxx


