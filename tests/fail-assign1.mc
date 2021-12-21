int assign_fail()
{
  int i = 42;
  bool b = true; 

  i = false; /* Fail: assigning a bool to an integer */

}

assign_fail();
