from run import run
import scope as scopem
import syntax as sy
import unittest


class MiscTest(unittest.TestCase):

    def setUp(self):
        self.maxDiff = None

    def test_non_trivial1(self):
        py = """
def get_counter_info(initial_value, increment):
    print("initial value is", initial_value)
    return 0, 1

def get_middle_element(names):
    c_info = get_counter_info(0, 1)
    counter = c_info[0]
    for name in names:
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2:
            return name
    print("didn't find middle element, last one was", last_element)

middle = get_middle_element(("e1", "e2", "e3", "e4"))
print(middle)
"""

        self._t(syntax=sy.JavaSyntax(), code=py, expected="""
public List<Integer> get_counter_info(Integer initial_value, Integer increment) {
    System.out.println(String.format("%s %d", "initial value is", initial_value));
    return new ArrayList<>(List.of(0, 1));
}
public String get_middle_element(List<String> names) {
    String last_element = null;
    List<Integer> c_info = get_counter_info(0, 1);
    Integer counter = c_info.get(0);
    for (String name : names) {
        counter = counter + c_info.get(1);
        last_element = name;
        if (counter == names.size() / 2) {
            return name;
        }
    }
    System.out.println(String.format("%s %s", "didn't find middle element, last one was", last_element));
}
String middle = get_middle_element(new ArrayList<>(List.of("e1", "e2", "e3", "e4")));
System.out.println(middle);
""")

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)
    

if __name__ == '__main__':
    unittest.main()
