#include <stdio.h>

void WriteStep(long Peg1, long Peg2){
	    printf ("%d", Peg1);
	    printf(" ---> ");
	    printf("%d", Peg2);
	    printf ("\n");
}


void BuildTower(long DiscNum, long OrigPeg, long NewPeg, long TempPeg){
    if (DiscNum == 1) {
	    WriteStep(OrigPeg,NewPeg);
	    }
    else {
	    BuildTower(DiscNum-1,OrigPeg,TempPeg,NewPeg);
	    WriteStep(OrigPeg,NewPeg);
	    BuildTower(DiscNum-1,TempPeg,NewPeg,OrigPeg);
    }
}    

int main(){
    long DiscNum = 15;
    if (DiscNum > 0){
       BuildTower(DiscNum, 1, 3, 2);
       }
    else {
	    printf("Error: The number of discs is invalid\n");
	}

}

