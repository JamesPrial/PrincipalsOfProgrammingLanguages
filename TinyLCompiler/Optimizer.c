/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2014                              *
 *  Authors: Ulrich Kremer                   *
 *           Hans Christian Woithe           *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"
struct stackNode {
	Instruction *inst;
	struct stackNode *next;
};
int isEmpty(struct stackNode *root){
	if(root == NULL){
		return 1;
	}
	return 0;
}

void push(struct stackNode **root, Instruction *toPush){
	struct stackNode *ptr = (struct stackNode *)malloc(sizeof(struct stackNode));
	ptr->next = *root;
	ptr->inst = toPush;
	*root = ptr;
}

Instruction* pop(struct stackNode **root){
	if(isEmpty(*root)){
		return NULL;
	}
	struct stackNode *temp = *root;
	*root = (**root).next;
	Instruction *ret = temp->inst;
	free(temp);
	return ret;
}
struct stackNode *buildWriteStack(Instruction *head){
	struct stackNode *root = NULL;
	Instruction *ptr = head;
	while(ptr != NULL){
		if(ptr->opcode == WRITE || ptr->opcode == READ){
			if(ptr->opcode == WRITE){
				push(&root, ptr);
			}
			ptr->critical = 'y';
		}
		ptr = ptr->next;
	}
	return root;
}

void markCrits(Instruction *head, Instruction *instr){
	switch(instr->opcode){
		case LOAD: ;
			char var = instr->field2;
			instr = instr->prev;
			while(instr != NULL){
				if(instr->opcode == STORE && instr->field1 == var){
					instr->critical = 'y';
					markCrits(head, instr);
					break;
				}
				instr = instr->prev;
			}
		case LOADI: break;
		case STORE: ;
			int reg = instr->field2;
			instr = instr->prev;
			int found = 0;
			while(instr != NULL && !found){
				switch(instr->opcode){
					case LOAD:
					case LOADI:
						if(instr->field1 == reg){
							instr->critical = 'y';
							found++;
							if(instr->opcode == LOAD){
								markCrits(head, instr);
							}
						}
						break;
					case ADD:
					case SUB:
					case MUL:
					case OR:
					case AND:
						if(instr->field1 == reg){
							instr->critical = 'y';
							found++;
							markCrits(head, instr);
						}
						break;
					case STORE:
					case READ:
					case WRITE:
					default:
						break;
				}
				instr = instr->next;
			}
		case ADD:
		case SUB:
		case MUL:
		case OR:
		case AND: ;
			int leftReg = instr->field2;
			int rightReg = instr->field3;
			instr = instr->prev;
			while(instr != NULL && (leftReg != 0 || rightReg != 0)){
				switch(instr->opcode){
					case LOAD:
					case LOADI:
						if(instr->field1 == leftReg || instr->field1 == rightReg){
							instr->critical = 'y';
							
							if(instr->field1 ==leftReg){
								leftReg = 0;
							}else{
								rightReg = 0;
							}
							if(instr->opcode == LOAD){
								markCrits(head, instr);
							}
						}
						break;
					case ADD:
					case SUB:
					case MUL:
					case OR:
					case AND:
						if(instr->field1 == leftReg || rightReg){
							instr->critical = 'y';
							if(instr->field1 ==leftReg){
								leftReg = 0;
							}else{
								rightReg = 0;
							}
							markCrits(head, instr);
						}
						break;
					case STORE:
					case READ:
					case WRITE:
					default:
						break;
				}
				instr = instr->prev;
			}
			break;			
		case READ: break;
		case WRITE: ;
			char id = instr->field1;
			instr = instr->prev;
			while(instr != NULL){
				if(instr->opcode == STORE && instr->field1 == id){
					instr->critical = 'y';
					markCrits(head, instr);
					break;
				}
				instr = instr->prev;
			}
			break;
		default:
			break;
	}
}

int main()
{
	Instruction *head;


	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}
	/* YOUR CODE GOES HERE */
	struct stackNode *writeRoot = buildWriteStack(head);
	while(!isEmpty(writeRoot)){
		markCrits(head, pop(&writeRoot));
	}
	Instruction *ptr = head;
	
	while(ptr != NULL){
		if(ptr->critical != 'y'){
			Instruction *next = ptr->next;
			if(ptr->prev != NULL){
				ptr->prev->next = next;
			}
			if(next != NULL){
				next->prev = ptr->prev;
			}
			free(ptr);
			ptr = next;
			
		}else{
			ptr = ptr->next;
		}
	}
	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}


