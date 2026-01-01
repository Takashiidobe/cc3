int main() { int x[2][3]; int *y=x; *(y+5)=5; return *(*(x+1)+2); }
