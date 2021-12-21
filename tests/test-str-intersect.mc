int foo(){
	string a = "my name is julie";
    string b = "my name is bob";
    string[] c = a & b; 

	int i = 0;
    for (i = 0 ; i < 3 ; i = i + 1) {
        printstr(c[i]);
    }
}

foo();