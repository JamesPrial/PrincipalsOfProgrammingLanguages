CC = @g++
CCFLAGS = -Isrc -Isrc/util -Isrc/gpu_required -Isrc/gpu_extra_credit -ggdb3 -Wall -Wno-write-strings

NVCC = @/usr/local/cuda/bin/nvcc
NVCCFLAGS = -Isrc -Isrc/util -Isrc/gpu_required -Isrc/gpu_extra_credit

rm = @rm
ifeq ($(OS),Windows_NT)
	rm = @del
endif

default: objs gsn match
	@chmod 700 .

debug: CCFLAGS += -O0 -ggdb0
debug: NVCCFLAGS += -g -G -O0
debug: default

OBJs_match = \
	objs/utils.o \
	objs/mmio.o \
	\
	objs/onewaywrapper.o \
	\
	objs/collateSegments.o \
	objs/strongestNeighborScan.o \
	\
	objs/check_handshaking.o \
	objs/markFilterEdges.o \
	objs/exclusive_prefix_sum.o \
	objs/packGraph.o \
	\

OBJs_gsn = \
	objs/utils.o \
	objs/mmio.o \
	\
	objs/strongwrapper.o \
	\
	objs/collateSegments.o \
	objs/strongestNeighborScan.o \
	\


objs:
	@chmod 700 .
	@chmod 400 testcases/*
	@mkdir -p objs

objs/utils.o: src/util/utils.cpp src/util/utils.hpp
	${CC} ${CCFLAGS} -o $@ -c src/util/utils.cpp

objs/mmio.o: src/util/mmio.cpp src/util/mmio.hpp
	${CC} ${CCFLAGS} -o $@ -c src/util/mmio.cpp
	

objs/onewaywrapper.o: src/onewaywrapper.cu src/onewaywrapper.hpp
	${NVCC} ${NVCCFLAGS} -o $@ -c src/onewaywrapper.cu

objs/strongwrapper.o: src/strongwrapper.cu src/strongwrapper.hpp
	${NVCC} ${NVCCFLAGS} -o $@ -c src/strongwrapper.cu
	

objs/strongestNeighborScan.o: src/gpu_required/strongestNeighborScan.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_required/strongestNeighborScan.cu

objs/collateSegments.o: src/gpu_required/collateSegments.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_required/collateSegments.cu
	

objs/check_handshaking.o: src/gpu_extra_credit/check_handshaking.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_extra_credit/check_handshaking.cu
	
objs/markFilterEdges.o: src/gpu_extra_credit/markFilterEdges.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_extra_credit/markFilterEdges.cu
	
objs/exclusive_prefix_sum.o: src/gpu_extra_credit/exclusive_prefix_sum.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_extra_credit/exclusive_prefix_sum.cu

objs/packGraph.o: src/gpu_extra_credit/packGraph.cu
	${NVCC} ${NVCCFLAGS} -o $@ -c src/gpu_extra_credit/packGraph.cu
	
	
match: ${OBJs_match} src/main.cu
	@chmod 700 .
	@chmod 400 testcases/*
	${NVCC} ${NVCCFLAGS} ${OBJs_match} src/main.cu -o $@

gsn: ${OBJs_gsn} src/mainStrong.cu
	@chmod 700 .
	@chmod 400 testcases/*
	${NVCC} ${NVCCFLAGS} ${OBJs_gsn} src/mainStrong.cu -o $@
	

submit: $(wildcard src/gpu_*/*)
	tar cvf submit_me.tar src/gpu_*
	@chmod 700 .
	@chmod 700 submit_me.tar
	

clean:
	rm -rf match gsn submit_me.tar objs/*.o

