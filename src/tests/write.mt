
/* write test */

f = open("tests/write.out", "w");

/* first argument is the data, */
write("hello", 5,1,f);
close(f);

print("hello");

