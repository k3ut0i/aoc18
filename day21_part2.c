#include <stdio.h>
#include <stdlib.h>
int main(){
  long long r1, r3, r4, r5;
  do{
    r3 = 123;
    r3 = r3 & 456;
  }while(r3 != 72);
  r3 = 0;

 label1:
  r1 = r3 | 65536;
  r3 = 10373714;
 label2:
  r5 = r1 & 255;
  r3 = r3 + r5;
  r3 = r3 & 16777215;
  r3 = r3 * 65899;
  r3 = r3 & 16777215;
  
  if (r1 < 256){
    if((r3 | 65536) == (0 | 65536)){
      printf("%lld\n", r3);
      exit(0);
    }
    else
      goto label1;
  }
  r5 = 0;

 label3:
  r4 = r5 + 1;
  r4 = r4 * 256;
  if(r4 > r1){
    r1 = r5;
    goto label2;
  }else{
    r5 = r5 + 1;
    goto label3;
  }
  return 0;
}

/* Local Variables: */
/* compile-command: "gcc -Wall -Wextra -o day21_part2 day21_part2.c" */
/* End: */
