def float foo() {
	arra = [1.0,23.1,0.2];
	e = arra[0];
	g = arra[0];
	arra[0] = 7.9;
	return arra[0];
}

tempFloat = foo();
print(tempFloat);