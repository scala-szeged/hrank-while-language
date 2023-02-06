# The While language from hackerrank.com

According to the original implementation task I made an interpreter which can handle 
nested if, nested while, variables, integer and boolean expressions.

I implemented array handling, too which is an additional feature. It was not needed 
for the succesful execution of the original task on hackerrank.com. 
Please see the algorithm below which demonstrates array handling.
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

## I have 200 lines long interpreter

It can execute the kind of algorithms you see above, which is a sort algorithm. 
Using the link below you can see a fibonacci and a factorial algorithm, too.
The interpreter executes the aformentioned language. Please see the
[src/main/WhileLanguage](https://github.com/scala-szeged/hrank-while-language/blob/master/src/main/WhileLanguage) folder.
