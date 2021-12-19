int main()
{
  int[] a;
  int b;
  int c;
  a = [1, 2, 3];
  b = a[2];
  a[2] = 4;
  print(b);
  c = a[2];
  print(c);
  return 0;
}