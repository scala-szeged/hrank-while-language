# The While language from hackerrank.com

According to the original implementation task it can handle 
nested if, nested while, variable, integer and boolean expressions.

I implemented array handling, too which is not needed for the task on hackerrank.com. Please see the algorithm below which demonstrates it.
```go
a := [4,3,2,1];
j:=0;
while(j<3)
do
{
   j:=j+1;
   i:=0;
   while(i<3)
   do
   {
     i:=i+1;
     if(a[i-1] > a[i]) then
     {
       t:=a[i-1];
       a[i-1]:=a[i];
       a[i]:=t
     }
   }
}
```

The result of running the above naive sort algorithm:
```
a [1,2,3,4]
i 3
j 3
t 2
```

## I use the following convention for my git commit messages

Please see https://www.conventionalcommits.org