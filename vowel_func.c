#include <stdlib.h>
#include <stdio.h>
#include <string.h>


char *string_concat(char *s1, char *s2) {
    char *new = (char *) malloc(strlen(s1) + strlen(s2));
    strncpy(new, s1, strlen(s1)-1);
    strncat(new, s2+1, strlen(s2)-1);
    return new;
}

int string_equality(char *s1, char *s2){
	int result;
	result = strcmp(s1, s2);
	return result;
}
