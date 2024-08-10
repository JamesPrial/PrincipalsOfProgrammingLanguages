/*
 **********************************************
 *  CS314 Principles of Programming Languages *
 *  Spring 2020                               *
 **********************************************
 */
#include <vector>
#include <algorithm>
#include <iostream>
#include "utils.hpp"
using namespace std;

void setTime(struct timeval *time)
{
    gettimeofday(time, NULL);
}

double getTime(struct timeval *startTime, struct timeval *endTime)
{
    struct timeval elapsedTime;
    timersub(endTime, startTime, &elapsedTime);
    return (elapsedTime.tv_sec * 1000.0 + elapsedTime.tv_usec / 1000.0);
}

int err(const char *level, const char *func, int line, const char *fmt, ...)
{
	char s[1024];
	va_list va;
	int r;

	va_start(va, fmt);
	vsprintf(s, fmt, va);
	va_end(va);
	r = fprintf(stderr, "\n[%s:%s:%d] %s", level, func, line, s);
	return r;
}

void printUsage(char *argv) {
    fprintf(stderr, "Usage: %s <input file> <output file> <# threads> [GPU ID]\n"
                    "       e.g.: %s cant.mtx output.txt 4096\n"
                    "       <input file>: input file (matrix market format)\n"
                    "       <output file>: output file\n"
                    "       <# threads>: number of threads each kernel will launch\n"
					"       [GPU ID]: optional argument that decides which GPU device to use\n", argv, argv);
}

void swapArray(void **arrayA, void **arrayB) {
    void *tmp = *arrayA;
    *arrayA = *arrayB;
    *arrayB = tmp;
}

void exclusive_prefix_sum(int *num, int *sum, int size)
{
    int i;
    sum[0] = 0;
    for (i = 1; i <=size; i++) {
        sum[i] = sum[i - 1] + num[i - 1];
    }
}

void inclusive_prefix_sum(int *num, int *sum, int size)
{
    int i;
    sum[0] = num[0];
    for (i = 1; i < size; i++) {
        sum[i] = sum[i - 1] + num[i];
    }
}

void write_match_result(char *fileName, int *res, int nNodes)
{
    FILE *f;
    if ((f = fopen(fileName, "w")) == NULL) {
        ERROR("Could not open the output file.\n");
        exit(EXIT_FAILURE);
    }

    int i;

    fprintf(stderr, "Start Writing Matching Results to %s ... ", fileName);

    for (i = 0; i < nNodes; i++) {
        fprintf(f, "%d\n", res[i]);
    }

    fclose(f);

    fprintf(stderr, "Done writing.\n");

}

bool compareneighbors (pair<int,int> i, pair<int,int> j) { return (i.first<j.first); }
bool compareneighborsR (pair<int,int> i, pair<int,int> j) { return (i.first>j.first); }

void readmm(char *fileName, GraphData *graph)
{
    FILE *f;
    MM_typecode matCode;
    int retCode;

    int i;

    fprintf(stderr, "Start Reading Matrix %s ... ", fileName);

    if ((f = fopen(fileName, "r")) == NULL) {
        ERROR("Could not open the input Matrix Market file.\n");
        exit(EXIT_FAILURE);
    }

    if (mm_read_banner(f, &matCode) != 0) {
        fprintf(stderr, "Matrix Market type: %s\n",
                mm_typecode_to_str(matCode));
        ERROR("Could not process Matrix Market banner.\n");
        exit(EXIT_FAILURE);
    }

    /* Read the size of the sparse matrix */
    int nrow, ncol, nnz;
    if ((retCode = mm_read_mtx_crd_size(f, &nrow, &ncol, &nnz)) != 0) {
        ERROR("Could not read the size of the sparse matrix.\n");
        exit(EXIT_FAILURE);
    }
	
	int numNodes = (nrow > ncol) ? nrow : ncol;
	vector<pair<int, int> > neighborLists[numNodes];
	
    /* Read the matrix and pre-process it*/
    for (i = 0; i < nnz; i++) {
		int r, c, w;
        fscanf(f, "%d %d %d", &r, &c, &w);
        r--;
        c--;
        
		neighborLists[r].push_back(make_pair(c, w));
		neighborLists[c].push_back(make_pair(r, w));
    }

    /* Reserve memory for the graph */
    int * src = (int *) malloc(sizeof(int) * nnz * 2);
	int * dst = (int *) malloc(sizeof(int) * nnz * 2);
	int * weight = (int *) malloc(sizeof(int) * nnz * 2);
    graph->src = src;
    graph->dst = dst;
	graph->weight = weight;
	graph->numEdges = nnz * 2;
	graph->numNodes = numNodes;
	
    /* Populate graph data */
	int edgesProcced = 0;
	for(int i = 0; i < numNodes; i++) {
		int node1 = i;
		std::sort(neighborLists[i].begin(), neighborLists[i].end(), compareneighbors);
		for(unsigned int j = 0; j < neighborLists[i].size(); j++) {
			int node2 = neighborLists[i].at(j).first;
			int w = neighborLists[i].at(j).second;
			
			src[edgesProcced] = node1;
			dst[edgesProcced] = node2;
			weight[edgesProcced] = w;
			edgesProcced++;
		}
	}
	
	if(edgesProcced != nnz*2) {
		ERROR("Wrong edge count during graph generation.\n");
		exit(1);
	}
	
    if (f != stdin) fclose(f);

    fprintf(stderr, "Done reading.\n");
}
