int main() { int x[2][3]; int *y=x; *(y+1)=1; return *(*x+1); }
