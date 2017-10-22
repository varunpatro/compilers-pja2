class Test {
	void main () {
		Bool a;
		Bool b;
		Bool c;
		Bool d;

		Int x;
		Int y;
		Int z;

		A myCustomClass;

 		if (a || b && c && d) {
			while ((x != 1) && (y >= z) && (x < y)) {
				x = myCustomClass.myCustomProp.myCustomNestedMethod();
			}
		} else {
			y = 102;
			println(y);
		}
		return;
	}
}

class A {
	B myCustomProp;
}

class B {
	Int myCustomNestedMethod() {
		return 123;
	}
}