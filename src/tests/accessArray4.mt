def float foo() {
	a = [1.1,1.0,1.000001];
	b = a[0];
	c = a[2];
	d = a[0];
	return d;
}

temp = foo();