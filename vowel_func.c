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

bool string_inequality(char *s1, char *s2){
	bool result;
	int res = strcmp(s1, s2);
	bool bres = false;
	if (res == 0){
		bres = false;
	}
	else{
		bres = true;
	}
	result = bres;
	return result;
}
