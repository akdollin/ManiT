d = 4;
/* comment */

def int foo(int a)
{
  return a;
}

print(foo(4));

def void bar(int b)
{
	i = 0;
	for (i = 0; i < b; i = i + 1)
	{
		print(i);
	}
}

bar(4);

def void test()
{
	print("hello");
}

test();

struct tmp {
	int tmp1;
	int tmp2;
};

a = (1 + 1) * 2;
print(a);

pid = fork();
if (pid == 0)
{
	f1 = open("tmp.txt", "a");
	tmp = "hello1";
	write(tmp,1,6, f1);
	close(f1);
}
if (pid != 0)
{
	f2 = open("tmp.txt", "a");
	tmp1 = "hello2";
	write(tmp1,1,6,f2);
	close(f2);
}

