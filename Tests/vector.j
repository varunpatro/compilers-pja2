class Main123 {
	void main () {
		return ;
	}
}

class Vector {
	Int x;
	Int y;

	void addVector(Vector that) {
		this.x = this.x + that.x;
		this.y = this.y + that.y;
	}

	void scaleVector(Int x) {
		this.x = this.x * x;
		this.y = this.y * x;
	}

	Int dotVector(Vector that) {
		Int result;
		result = result + this.x * that.x;
		result = result + this.y * that.y;
		return result;
	}

	Bool isEqual_v1(Vector that) {
		if (that == NULL) {
			return false;
		} else {
			return this.x == that.x && this.y == that.y;
		}
	}

	Bool isEqual_v2(Vector that) {
		if (that == NULL) {
			return false;
		} else {
			return this.x == that.x && this.y == that.y;
		}
	}

	Bool isEqual_v3(Vector that) {
		return that != NULL && this.x == that.x && this.y == that.y;
	}
}
