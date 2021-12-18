#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *string_concat(char *s1, char *s2) {
    char *new = (char *) malloc(strlen(s1) + strlen(s2));
    strncpy(new, s1, strlen(s1)-1);
    strncat(new, s2+1, strlen(s2)-1);
    return new;
}


char *slice(const char *str, size_t s, size_t e){
	size_t index = 0;
	size_t length = strlen(str);
	char *slicestring = (char*)malloc(length +1);
	while (s < e && s <length){
		slicestring[index] = str[s];
		index++;
		s++;
	}
	slicestring[index] = '\0';
	return slicestring;
}