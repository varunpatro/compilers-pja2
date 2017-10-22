class Main {
	void main(){
		LinkedList ls;
		Int i;

		ls = new LinkedList();

		i = 0;
		while(i<10){
			ls.insert(i);
			i = i+1;
		}

		ls.print_list();

	}
}

class Node {
	Int x;
	Node next;
}

class LinkedList {
	Node head;

	void insert(Int x){
		Node n;
		n = new Node();

		n.x = x;
		n.next = head.next;

		head = n;
	}

	void print_list(){
		Node curr;
		curr = this.head;
		while(curr != NULL){
			println(curr.x);
			curr = curr.next;
		}
	}
}