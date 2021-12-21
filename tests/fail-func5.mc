int bar() {
  int a = 0;
  void b; /* Error: illegal void local b */
  bool c = true;

  return 0;
}

bar();