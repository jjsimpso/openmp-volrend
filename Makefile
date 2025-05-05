#---------------------------------------------------------------------------------
# BUILD is the directory where object files & intermediate files will be placed
# all directories are relative to this makefile
#---------------------------------------------------------------------------------
BUILD		:= src
SRCDIR		:= src
INCDIR		:= include

LIB_SOURCES	:= ndarray.c ppm.c nda_ops.c
LIB_INCLUDES	:= ndarray.h ppm.h nda_ops.h
LIB_NAME        := libvolrend

#---------------------------------------------------------------------------------
# options for code generation
#---------------------------------------------------------------------------------
CC = gcc
COMPILERFLAGS = -Wall -std=gnu11 -O3
DEBUG_FLAGS = -g -O0
OPENMP_FLAGS = -fopenmp
#SAN_FLAGS = -fsanitize=address -fno-omit-frame-pointer
#MYFLAGS = -DVGA_DEBUG
INCLUDE  = -I$(CURDIR)/$(INCDIR) -I$(CURDIR)/$(SRCDIR)
LIBS     =  -lm
CFLAGS = $(COMPILERFLAGS) $(MYFLAGS) $(DEBUG_FLAGS) $(OPENMP_FLAGS) $(SAN_FLAGS) $(INCLUDE) $(LIBS)

LDFLAGS = -shared -fpic
#LDFLAGS += -fsanitize=address

LIBDIRS  = -L$(CURDIR)/$(BUILD) -L$(CURDIR)

DEPSDIR := $(CURDIR)/$(BUILD)
#CFILES	:= $(foreach dir,$(SRCDIR),$(notdir $(wildcard $(dir)/*.c)))
CFILES	:= $(LIB_SOURCES)
OFILES	:= $(CFILES:.c=.o)

OBJS = $(addprefix $(BUILD)/, $(OFILES))
# these two are used when building the shared object
LIB_SRC = $(addprefix $(SRCDIR)/, $(LIB_SOURCES))
LIB_INC = $(addprefix $(SRCDIR)/, $(LIB_INCLUDES))

#---------------------------------------------------------------------------------
# basic rules
#---------------------------------------------------------------------------------
%.o : %.c
	@echo $(notdir $<)
	$(CC) $(CFLAGS) -c $< -o $@

#---------------------------------------------------------------------------------
#
#---------------------------------------------------------------------------------
all: $(LIB_NAME).a $(LIB_NAME).so ndarray-test

$(LIB_NAME).a : $(OBJS)
	ar rcs $@ $(OBJS)

# don't use .o files to build shared object. compile from source.
$(LIB_NAME).so : $(LIB_SRC) $(LIB_INC) Makefile
	$(CC) $(LIB_SRC) $(CFLAGS) $(LDFLAGS) -o $@

volrend-test : test/volrend-test.o
	$(CC) $(CFLAGS) -o volrend-test $(LIBDIRS) test/volrend-test.o $(LIBS) -lvolrend

ndarray-test : test/ndarray-test.c
	$(CC) $(CFLAGS) -o test/ndarray-test $(LIBDIRS) test/ndarray-test.c $(LIBS) -lvolrend

debug : 
	@echo $(CFILES)
	@echo $(OBJS)

install :
	cp $(INCLUDES)/*.h /usr/local/include/libvolrend
	cp $(LIB_NAME).a /usr/local/lib/
	cp $(LIB_NAME).so /usr/local/lib/

clean:
	@echo clean ...
	rm -f $(BUILD)/*.o
	rm -f $(LIB_NAME).a
	rm -f $(LIB_NAME).so
