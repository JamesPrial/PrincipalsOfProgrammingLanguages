/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include "utils.hpp"
#include <strings.h>
#include "DataStructure.hpp"
#include "onewaywrapper.hpp"
#include <iostream>
using namespace std;

int main(int argc, char *argv[])
{
    if (argc < 4) {
        printUsage(argv[0]);
        exit(EXIT_FAILURE);
    }

    int i = 1;

    char * inputFile = argv[i++];
    char * outputFile = argv[i++];
	int numthreads = atoi(argv[i++]);
	if(numthreads < 1) {
		printUsage(argv[0]);
        exit(EXIT_FAILURE);
	}
	
	if(argc == 5) {
		cudaSetDevice(atoi(argv[i++]));
	}

    GraphData graph;

    //read the matrix/graph from the matrix market format file(.mtx) and sort it
    readmm(inputFile, &graph);

    //allocate memory for matching result
    int *res = (int *) malloc(graph.numNodes * sizeof(int));
	
    //initialize res to UNMATCHED
    for (i = 0 ; i < graph.numNodes; i++) res[i] = -1;
	
    one_way_handshake_wrapper(graph, res, numthreads);
	
    //write result to output file
    write_match_result(outputFile, res, graph.numNodes);

    //clean allocated memory
	free(res);
	free(graph.src);
	free(graph.dst);
	free(graph.weight);

    return 0;
}
