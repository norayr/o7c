// FILE: hanoi.C
//
// The Towers of Hanoi
//
// How to move n discs from pin 1 to pin 3 using pin 2 as spare

//#include <assert.h>    // Provides assert
#include <iostream.h>  // Provides cout and cin
//#include <stdlib.h>    // Provides EXIT_SUCCESS


void move(int n, int A, int B, int C)
// n : number to move
// A : source spindle
// B : destination spindle
// C : spare spindle
{
  //assert(n>0);

  if (n==1)
    cout << A << " ---> " << C << "\n";
  else {
    move(n-1,A,B,C);
    move(1,A,C,B);
    move(n-1,B,C,A);
  }
}


int main()
{
  int n;

  /*cout << "Enter the number of disc to play Towers of Hanoi with: \n";
  cin >> n;*/
  n = 15;

  move(n,1,3,2);

  return 0;// EXIT_SUCCESS;
}

