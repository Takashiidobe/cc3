int main() { int x[2][3]; int *y=x; *y=0; return **x; }
