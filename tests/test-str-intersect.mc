int main(){
	string a; 
    string b; 
    string[] c;
    int i;

    a = "my name is julie";
    b = "my name is bob";
	c = a & b; 
	
    for (i = 0 ; i < 3 ; i = i + 1) {
        printstr(c[i]);
    }
}