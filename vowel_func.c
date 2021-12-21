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

bool string_equality(char *s1, char *s2){
	bool result;
	int res = strcmp(s1, s2);
	bool bres = false;
	if (res == 0){
		bres = true;
	}
	else{
		bres = false;
	}
	result = bres;
	return result;
}

 char** string_sub(char s1[], char s2[]) { 
 	  char** string_intersect(char s1[], char s2[]) { 
    
    int space_counter1 = 1;
    int space_counter2 = 1;
    
    for(int i = 0; s1[i] != '\0'; i++)
    {
         if (s1[i] == ' ')
         {
              space_counter1++;
         }
    }
    for(int i = 0; s2[i] != '\0'; i++)
    {
         if (s2[i] == ' ')
         {
              space_counter2++;
         }
    }
    
    char **array = malloc (sizeof (char *) * space_counter1);
    char **remove_array = malloc (sizeof (char *) * space_counter1);
    char **array2 = malloc (sizeof (char *) * space_counter2);
    
    char *zero = "0";
    for (int w = 0; w < space_counter1; w++){
        remove_array[w] = zero;
    }

    
    char *t = strtok(s1, " ");

	
	int i = 0;
	while (t != NULL)
    {
        array[i] = t;
        i++; 
        t = strtok (NULL, " ");
    }
    
    char *t2 = strtok (s2, " ");
    int i2 = 0; 
    while (t2 != NULL)
    {
        array2[i2] = t2; 
        i2++; 
        t2 = strtok (NULL, " ");
    }
    
	char *one = "1";
    int counter = 0;
	for (int j = 0; j < i; j++) {
	    for (int k = 0; k < i2; k++) {
	        if (strcmp(array[j], array2[k]) == 0) {
	            counter++; 
	            remove_array[j] = one;
	            
	           
	        }
	    }
	}
    printf("%d", i);
    printf("%d", i2);
	char **arr_res = malloc (sizeof (char *) * space_counter1);
	int l = 0;
	for (int j = 0; j < i; j++) {
	        if (remove_array[j]== zero) {
	            arr_res[l] = array[j];
	            l++;
	        }
	
	}
    return arr_res;
}















 }
