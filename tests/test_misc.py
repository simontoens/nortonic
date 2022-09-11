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
    middle_element_index = None
    for name in names:
        counter = counter + c_info[1]
        last_element = name
        if counter == len(names) / 2:
            middle_element_index = counter
    print("last element processed:", last_element)
    return names[middle_element_index]

el = get_middle_element(("e1", "e2", "e3", "e4"))
print("the element closest to the middle is", el)
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
    Integer middle_element_index = null;
    for (String name : names) {
        counter = counter + c_info.get(1);
        last_element = name;
        if (counter == names.size() / 2) {
            middle_element_index = counter;
        }
    }
    System.out.println(String.format("%s %s", "last element processed:", last_element));
    return names.get(middle_element_index);
}
String el = get_middle_element(new ArrayList<>(List.of("e1", "e2", "e3", "e4")));
System.out.println(String.format("%s %s", "the element closest to the middle is", el));
""")

        self._t(syntax=sy.ElispSyntax(), code=py, expected="""
(defun get_counter_info (initial_value increment)
    (message "%s %s" "initial value is" initial_value)
    (list 0 1))
(defun get_middle_element (names)
    (setq c_info (get_counter_info 0 1))
    (setq counter (nth 0 c_info))
    (setq middle_element_index nil)
    (dolist (name names)
        (setq counter (+ counter (nth 1 c_info)))
        (setq last_element name)
        (if (equal counter (/ (length names) 2))
            (setq middle_element_index counter)))
    (message "%s %s" "last element processed:" last_element)
    (nth middle_element_index names))
(setq el (get_middle_element (list "e1" "e2" "e3" "e4")))
(message "%s %s" "the element closest to the middle is" el)
""")        

    def _t(self, code, expected, syntax):
        generated_code = run(code, syntax)

        self.assertEqual(expected.strip(), generated_code)
    

if __name__ == '__main__':
    unittest.main()
