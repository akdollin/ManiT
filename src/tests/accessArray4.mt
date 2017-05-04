def string foo() {
	a = ["hi","hello","world"];
	b = a[0];
	c = a[2];
	d = a[0];
	return d;
}

temp = foo();
print(temp);