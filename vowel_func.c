#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


char *string_concat(char *s1, char *s2) {
    char *new = (char *) malloc(strlen(s1) + strlen(s2));
    strncpy(new, s1, strlen(s1)-1);
    strncat(new, s2+1, strlen(s2)-1);
    return new;
}

int *string_equality(char *s1, char *s2){
	int *result = (int *) malloc(sizeof(int));
	int res = strcmp(s1, s2);
	result = &res;

	return result;
}
