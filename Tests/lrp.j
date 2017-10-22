class LRP {
      void main() {
          List list;
          List reversed;
          List ptr;
          ListReverser lr;

          list = list.cons(1, list.cons(2, list.cons(3, list.consempty())));
          println(list.head());

          reversed = lr.reverse(list);
          println(reversed.head());

          return;
      }
}

class ListPrinter {
    void print(List list) {
        List ptr;
        ptr = list;
        while (!ptr.empty()) {
            println(ptr.head());
            ptr = ptr.tail();
        }
    }
}

class ListReverser {
    List reverse(List list) {
        if (list.empty()) {
            return list;
        } else {
            list.cons(list.head(), reverse(list.tail()));
            return NULL;
        }
    }
}

class List {
    Bool is_empty;
    Int val;
    List tailmem;

    void init(Bool e, Int v, List t) {
        this.is_empty = e;
        this.val = v;
        this.tailmem = t;
        return;
    }

    Int head() {
        return this.val;
    }

    List tail() {
        return this.tailmem;
    }

    List consempty() {
        List result;
        result.init(true, 0, NULL);
        return result;
    }

    List cons(Int v, List t) {
        List result;
        result.init(false, v, t);
        return result;
    }

    Bool empty() {
        return this.is_empty;
    }
}
