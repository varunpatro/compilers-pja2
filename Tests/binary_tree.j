class Main {
	void main(Int v1, Int v2, Int v3, Int target){
		Node n1;
		Node n2;
		Node n3;

/*
		n1 = new Node();
		n2 = new Node();
		n3 = new Node();

		n1.init(5);
		n2.init(1);
		n3.init(10);

		n1.set_left_child(n2);
		n1.set_right_child(n3);

		println(n1.search(target));
		*/
		return;
   }
}

class Node {
	Int value;
	Node left_child;
	Node right_child;

	void init(Int value) {
		this.value = value;
		this.left_child = NULL;
		this.right_child = NULL;
	}

	void set_left_child(Node child) {
		this.left_child = child;
	}


	void set_right_child(Node child) {
		this.right_child = child;
	}

	void set_value(Int value) {
		this.value = value;
	}

	Bool search(Int target) {
		if (target == this.value) {
			return true;
		} else {
			if (target < this.value && this.left_child != NULL) {
				return this.left_child.search(target);
			} else {
				if (target > this.value && this.right_child != NULL) {
					return this.left_child.search(target);
				} else {
					return false;
				}
			}
		}
	}
}
