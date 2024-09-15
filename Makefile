#---------------------------------------------------------------------------------
# BUILD is the directory where object files & intermediate files will be placed
# all directories are relative to this makefile
#---------------------------------------------------------------------------------
BUILD		:= src
SRCDIR		:= src
INCDIR		:= include

LIB_SOURCES	:= ndarray.c
LIB_INCLUDES	:= ndarray.h


#---------------------------------------------------------------------------------
# options for code generation
#---------------------------------------------------------------------------------
CC = gcc
COMPILERFLAGS = -Wall -std=gnu11 -O3
#DEBUG_FLAGS = -g -O0
#MYFLAGS = -DVGA_DEBUG
INCLUDE  = -I$(CURDIR)/$(INCDIR) -I$(CURDIR)/$(SRCDIR)
CFLAGS = $(COMPILERFLAGS) $(MYFLAGS) $(DEBUG_FLAGS) $(INCLUDE)

LIBDIRS  = -L$(CURDIR)/$(BUILD) -L$(CURDIR)
LIBS     =  

DEPSDIR := $(CURDIR)/$(BUILD)
#CFILES	:= $(foreach dir,$(SRCDIR),$(notdir $(wildcard $(dir)/*.c)))
CFILES	:= $(LIB_SOURCES)
OFILES	:= $(CFILES:.c=.o)

OBJS = $(addprefix $(BUILD)/, $(OFILES))

#---------------------------------------------------------------------------------
# basic rules
#---------------------------------------------------------------------------------
.SUFFIXES: .c .o

%.o : %.c
	@echo $(notdir $<)
	$(CC) $(CFLAGS) -c $< -o $@

#---------------------------------------------------------------------------------
#
#---------------------------------------------------------------------------------
all: libvolrend.a ndarray-test

libvolrend.a : $(OBJS)
	ar rcs $@ $(OBJS)

volrend-test : test/volrend-test.o
	$(CC) $(CFLAGS) -o volrend-test $(LIBDIRS) test/volrend-test.o $(LIBS) -lvolrend

ndarray-test : test/ndarray-test.c
	$(CC) $(CFLAGS) -o test/ndarray-test $(LIBDIRS) test/ndarray-test.c $(LIBS) -lvolrend

debug : 
	@echo $(CFILES)
	@echo $(OBJS)

install :
	cp $(INCLUDES)/*.h /usr/local/include/libvolrend
	cp libvolrend.a /usr/local/lib/

clean:
	@echo clean ...
	rm -f $(BUILD)/*.o
	rm -f libvolrend.a
