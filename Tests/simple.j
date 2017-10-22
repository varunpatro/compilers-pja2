class A {
	void main(Bool d) {
		Int a;
		Int b;
		Int i;
		a = 1 + 2;

		if (i > 0) {
			i = i + 2;
			i = i * 2;
		} else
		{
		println(i);
		}

		return;
	}
}


class A2 {
	void main2(Bool d) {
		Int a;
		Bool b;
		B c;
		c = new B();
		c = NULL;
		a = c.getTemperature();
		a = c.temperature;
		//return new B();
		return;
	}
}


class B {
	Int temperature;
	A2 hi;

	Bool setTemperature(Int temp) {
		temperature = temp;
		temp = getTemperature();
		return true;
	}

	B retNull() {
		B x;
		x = new B();
		return NULL;
	}

	Int getTemperature() {
		return temperature;
	}

	void hello() {
		return hi.main2(false);
	}
}
